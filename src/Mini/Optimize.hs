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
type Worklist = [(Vertex, Iloc)]
type RegSet = S.Set Reg
type ReachSet = S.Set IlocDef
type IlocDef = (Reg, Iloc)
type VertexReach = (Reg, Vertex, Iloc)

type Reaches = HM.HashMap Vertex NodeReaches
type NodeReaches = S.Set VertexReach
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

getReg :: VertexReach -> Reg
getReg (r, _, _) = r

getVertex :: VertexReach -> Vertex
getVertex (_, v, _) = v

getIloc :: VertexReach -> Iloc
getIloc (_, _, i) = i

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
            $ markIloc (getData node)
                    
markIloc :: [Iloc] -> ([Iloc], [MarkedIloc])
markIloc = mapAccumL mapFun []
    where mapFun l x = if isCritical x 
                           then (x:l, Marked x True) 
                           else (l, Marked x False)

markWorklist :: Worklist -> MarkedHash -> Reaches -> MarkedHash
markWorklist [] hash _ = hash
markWorklist ((vert, insn):rest) hash reaches = 
        markWorklist newInsns newHash reaches
    where currReach = reaches HM.! vert
          srcs = getSrcIlocRegs insn
          externDefs = S.filter (\(r, _, _) -> r `elem` restSrcs) currReach
          (internDef, restSrcs) = first (S.map $ makeVertexReach vert) 
                                    $ findDefs insn (hash HM.! vert)
          defs = internDef `S.union` externDefs
          (newHash, newInsns) = S.foldl' foldDefs (hash, rest) defs

findDefs :: Iloc -> MarkedNode -> (ReachSet, [Reg])
findDefs iloc (Node _ insns) = (defsFound, srcs \\ S.toList (S.map fst defsFound))
    where revPrevInsns = reverse $ takeWhile (iloc /=) $ map getMarkedData insns
          srcs = getSrcIlocRegs iloc
          findSrc s = fromMaybe [] $ (\x -> [(s, x)]) <$> 
                            find (elem s . getDstIlocRegs) revPrevInsns
          defsFound = S.fromList $ concatMap findSrc srcs

foldDefs :: (MarkedHash, Worklist) -> VertexReach -> (MarkedHash, Worklist)
foldDefs (oldHash, oldWork) (_, vert, iloc) = (newHash, newWork)
    where block = getData $ oldHash HM.! vert
          savedInsn = fromJust $ find ((==) iloc . getMarkedData) block
          (newHash, newWork) = if isMarked savedInsn
                                   then (oldHash, oldWork)
                                   else (HM.adjust (markInsn iloc) vert oldHash, 
                                            (vert, iloc):oldWork)

markInsn :: Iloc -> MarkedNode -> MarkedNode
markInsn iloc = mapNode mapFun
    where mapFun = map markIloc 
          markIloc x
            | getMarkedData x == iloc = mark x
            | otherwise = x
          

{-
before ++ [Marked iloc True] ++ after
            where (before, after) = second tail 
                    $ span ((/=) iloc . getMarkedData) xs
                    -}

replaceNode :: Node a -> [b] -> Node b
replaceNode (Node label _) = Node label

sweep :: MarkedNode -> IlocNode 
sweep = mapNode (map getMarkedData . filter isMarked)

-- Map over graph, iteratively
createReachingDefs :: IlocGraph -> Reaches
createReachingDefs ilGraph@(graph, hash) = iterateReaches ilGraph firstPass -- createReaches exitVertex HM.empty
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
makeVertexReach vert (r, i) = (r, vert, i)

createGen :: IlocNode -> ReachGen
createGen = fst . createGenKills

createGenKills :: IlocNode -> (ReachGen, ReachKill)
createGenKills = fst . foldl' genKillFoldFun ((S.empty, S.empty), S.empty) . getData

genKillFoldFun :: ((ReachGen, ReachKill), RegSet) -> Iloc -> ((ReachGen, ReachKill), RegSet)
genKillFoldFun ((gen, kill), redefs) iloc = ((newGen, newKill), newRedefs)
        where dsts = S.fromList $ map createTup $ getDstIlocRegs iloc
              newKill = kill `S.union` S.map createTup (S.fromList $ getSrcIlocRegs iloc)
              newGen = gen `S.union` dsts `reachDiff` newRedefs
              newRedefs = redefs `S.union` (S.map fst dsts `S.intersection` S.map fst gen)
              createTup x = (x, iloc)

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
