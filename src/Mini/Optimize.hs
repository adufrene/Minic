module Mini.Optimize ( removeUselessCode 
                     , createReachingDefs ) where

import Mini.Iloc.Types
import Mini.RegAlloc
import Mini.Graph

import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.List
import Data.Graph hiding (Node)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Maybe

import Debug.Trace

data Marked a = Marked a Bool deriving (Eq, Show)

instance Functor Marked where 
        fmap f (Marked a m) = Marked (f a) m

instance Applicative Marked where
        pure = return
        (<*>) = ap

instance Monad Marked where
        return = flip Marked False
        (Marked a _) >>= f = f a

type MarkedIloc = Marked Iloc
type MarkedNode = Node MarkedIloc
type Worklist = [IlocNdx]

type Index = Int
type IlocNdx = (Vertex, Index)

type RegSet = S.Set Reg
type IlocDef = (Reg, Index)
type VertexReach = (Reg, IlocNdx)

type Reaches = HM.HashMap Vertex NodeReaches
type NodeReaches = S.Set VertexReach
type ReachSet = S.Set IlocDef
type ReachGen = ReachSet
type ReachKill = ReachSet

type MarkedHash = HM.HashMap Vertex MarkedNode

isMarked :: Marked a -> Bool
isMarked (Marked _ m) = m

mark :: Marked a -> Marked a
mark (Marked a _) = Marked a True

unmark :: Marked a -> Marked a
unmark (Marked a _) = Marked a False

getMarkedData :: Marked a -> a
getMarkedData (Marked a _) = a

removeUselessCode :: IlocGraph -> IlocGraph
removeUselessCode ilGraph = (fst ilGraph, sweep <$> fullyMarked)
    where reaches = createReachingDefs ilGraph
          (worklist, marked) = initialMark ilGraph
          fullyMarked = markWorklist worklist marked reaches

initialMark :: IlocGraph -> (Worklist, MarkedHash)
initialMark = HM.foldlWithKey' foldFun ([], HM.empty) . snd
    where insertNode vert hash' node = HM.insert vert node hash'
          addVertex vert = map (\x -> (vert, x)) 
          foldFun (work, hash') vert node = ((++) work . addVertex vert) 
            *** (insertNode vert hash' . replaceNode node) 
                $ markIloc (zip [0..] $ getData node)
                    
markIloc :: [(Index, Iloc)] -> ([Index], [MarkedIloc])
markIloc = mapAccumL mapFun []
    where mapFun l (ndx, x) = if isCritical x 
                           then (ndx:l, Marked x True) 
                           else (l, Marked x False)

markWorklist :: Worklist -> MarkedHash -> Reaches -> MarkedHash
markWorklist [] hash _ = hash
markWorklist ((vert, ndx):rest) hash reaches = 
        markWorklist newInsns newHash reaches
    where currReach = reaches HM.! vert
          externDefs = S.filter (\(r,( _, _)) -> r `elem` restSrcs) currReach
          defs = internDef `S.union` externDefs
          (newHash, newInsns) = S.foldl' foldDefs (hash, rest) defs
          (internDef, restSrcs) = first (S.map $ makeVertexReach vert) 
                                    $ findDefs ndx (hash HM.! vert)

findDefs :: Index -> MarkedNode -> (ReachSet, [Reg])
findDefs ndx (Node _ insns) = (defsFound, srcs \\ S.toList (S.map fst defsFound))
    where revPrevInsns = reverse $ take ndx insns
          len = length revPrevInsns
          srcs = getSrcIlocRegs $ getMarkedData $ insns !! ndx
          defsFound = S.fromList $ concatMap findSrc srcs
          findSrc s = fromMaybe [] $ (\x -> [(s, len - 1 - x)]) <$> findIndex 
                        (elem s . getDstIlocRegs . getMarkedData) revPrevInsns

foldDefs :: (MarkedHash, Worklist) -> VertexReach -> (MarkedHash, Worklist)
foldDefs (oldHash, oldWork) (_, (vert, ndx)) = (newHash, newWork)
    where block = getData $ oldHash HM.! vert
          savedInsn = block !! ndx
          (newHash, newWork) = if isMarked savedInsn
                                   then (oldHash, oldWork)
                                   else (HM.adjust (markInsn ndx) vert oldHash, 
                                            (vert, ndx):oldWork)

markInsn :: Index -> MarkedNode -> MarkedNode
markInsn ndx = mapNode mapFun
    where mapFun = zipWith (curry markIloc) [0..]
          markIloc (insnNdx, x)
            | insnNdx == ndx = mark x
            | otherwise = x
          

replaceNode :: Node a -> [b] -> Node b
replaceNode (Node label _) = Node label

sweep :: MarkedNode -> IlocNode 
sweep = mapNode (map getMarkedData . filter isMarked)

-- Map over graph, iteratively
createReachingDefs :: IlocGraph -> Reaches
createReachingDefs ilGraph@(graph, hash) = iterateReaches ilGraph firstPass
    where firstPass = foldl' (createDef ilGraph) HM.empty $ HM.keys hash

createDef :: IlocGraph -> Reaches -> Vertex -> Reaches
createDef ig@(graph, hash) reaches vert = HM.insert vert thisReach reaches
        where preds = filter (/= 0) $ graph `getPredecessors` vert
              predReach = S.map (createPredReach reaches hash) $ S.fromList preds
              startGen = S.map (makeVertexReach vert) $ createGen $ hash HM.! vert
              thisReach = S.foldl' S.union S.empty predReach

createPredReach :: Reaches -> HM.HashMap Vertex IlocNode -> Vertex -> NodeReaches
createPredReach reaches hash vert = gen `S.union` (savedReach S.\\ kill)
    where (gen, kill) = S.map addVertex *** S.map addVertex 
                            $ createGenKills (hash HM.! vert)
          addVertex = makeVertexReach vert
          savedReach = if vert `HM.member` reaches
                             then reaches HM.! vert
                             else S.empty
          
iterateReaches :: IlocGraph -> Reaches -> Reaches
iterateReaches ig@(graph, hash) old
    | old == new = old
    | otherwise = iterateReaches ig new
    where new = foldl' (createDef ig) old $ HM.keys hash

getReaches :: Vertex -> Reaches -> IlocGraph -> (NodeReaches, Reaches)
getReaches vert inProgress ig@(graph, hash) = (newReaches HM.! vert, newReaches)
    where newReaches = if vert `HM.member` inProgress
                           then inProgress
                           else createReaches vert inProgress ig

createReaches :: Vertex -> Reaches -> IlocGraph -> Reaches
createReaches vert inProgress ig@(graph, hash) = 
        HM.insert vert thisReach newReaches 
    where preds = filter (/= entryVertex) $ graph `getPredecessors` vert
          (newReaches, thisReach) = foldl' (reachFoldFun ig) 
                                        (inProgress, startDefs) preds
          startDefs = S.map (makeVertexReach vert) $ fst (createGenKills 
                            $ hash HM.! vert)

reachFoldFun :: IlocGraph -> (Reaches, NodeReaches) -> Vertex -> (Reaches, NodeReaches)
reachFoldFun graph@(_, hash) (oldReach, reachSoFar) vert = 
        (newReach, reachSoFar `S.union` tempReach)
    where node = hash HM.! vert
          (gen, kill) = S.map addVertex *** S.map addVertex $ createGenKills node
          addVertex = makeVertexReach vert
          (thisReach, newReach) = getReaches vert oldReach graph
          tempReach = gen `S.union` (thisReach S.\\ kill)

makeVertexReach :: Vertex -> IlocDef -> VertexReach
makeVertexReach vert (r, i) = (r, (vert, i))

createGen :: IlocNode -> ReachGen
createGen = fst . createGenKills

createGenKills :: IlocNode -> (ReachGen, ReachKill)
createGenKills = fst . foldl' genKillFoldFun ((S.empty, S.empty), S.empty) 
                    . zip [0..] . getData

genKillFoldFun :: ((ReachGen, ReachKill), RegSet) -> (Index, Iloc) 
                        -> ((ReachGen, ReachKill), RegSet)
genKillFoldFun ((gen, kill), redefs) (ndx, iloc) = ((newGen, newKill), newRedefs)
        where dsts = S.fromList $ map createTup $ getDstIlocRegs iloc
              newKill = kill `S.union` S.map createTup (S.fromList $ getSrcIlocRegs iloc)
              newGen = gen `S.union` dsts `reachDiff` newRedefs
              newRedefs = redefs `S.union` (S.map fst dsts `S.intersection` S.map fst gen)
              createTup x = (x, ndx)

reachDiff :: ReachSet -> RegSet -> ReachSet
reachDiff reach regs = S.map mapFun diff
        where reachRegs = S.map fst reach
              diff = reachRegs S.\\ regs
              mapFun x = S.elemAt 0 $ S.filter ((== x) . fst) reach

isCritical :: Iloc -> Bool
isCritical Storeret{} = True
isCritical Brz{} = True
isCritical Jumpi{} = True
isCritical Call{} = True
isCritical RetILOC{} = True
isCritical Storeglobal{} = True
isCritical Loadglobal{} = True  -- Is this critical?
isCritical Println{} = True
isCritical PrintILOC{} = True
isCritical ReadILOC{} = True
isCritical PrepArgs{} = True
isCritical Comp{} = True
isCritical Compi{} = True
isCritical UnprepArgs{} = True
isCritical Storeoutargument{} = True
isCritical Storeai{} = True
isCritical _ = False
