module Mini.Optimize ( removeUselessCode
                     , createReachingDefs
                     , debugMarked ) where

import Mini.Iloc.Types
import Mini.Graph

import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.List
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

type MarkedGraph = Graph MarkedNode
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
removeUselessCode ilGraph = mapGraph sweep fullyMarked
    where reaches = createReachingDefs ilGraph
          (worklist, marked) = initialMark ilGraph
          fullyMarked = markWorklist worklist marked reaches

debugMarked :: IlocGraph -> MarkedGraph
debugMarked ig = markWorklist worklist marked reaches
    where (worklist, marked) = initialMark ig
          reaches = createReachingDefs ig

initialMark :: IlocGraph -> (Worklist, MarkedGraph)
initialMark = mapGraphKeyAccumL accFun []
    where accFun work vert node = ((++) work . addVertex vert) *** 
                replaceNode node $ markIloc (zip [0..] $ getData node)
          addVertex vert = map (\x -> (vert, x)) 
                    
markIloc :: [(Index, Iloc)] -> ([Index], [MarkedIloc])
markIloc = mapAccumL mapFun []
    where mapFun l (ndx, x) = if isCritical x 
                           then (ndx:l, Marked x True) 
                           else (l, Marked x False)

markWorklist :: Worklist -> MarkedGraph -> Reaches -> MarkedGraph
markWorklist [] graph _ = graph
markWorklist ((vert, ndx):rest) graph reaches = 
        markWorklist newInsns newGraph reaches
    where currReach = reaches HM.! vert
          externDefs = S.filter (\(r,( _, _)) -> r `elem` restSrcs) currReach
          defs = internDef `S.union` externDefs
          (newGraph, newInsns) = S.foldl' foldDefs (graph, rest) defs
          (internDef, restSrcs) = first (S.map $ makeVertexReach vert) 
                                    $ findDefs ndx (graph `at` vert)

findDefs :: Index -> MarkedNode -> (ReachSet, [Reg])
findDefs ndx (Node _ insns) = (defsFound, srcs \\ S.toList (S.map fst defsFound))
    where revPrevInsns = reverse $ take ndx insns
          len = length revPrevInsns
          srcs = getSrcIlocRegs $ getMarkedData $ insns !! ndx
          defsFound = S.fromList $ concatMap findSrc srcs
          findSrc s = fromMaybe [] $ (\x -> [(s, len - 1 - x)]) <$> findIndex 
                        (elem s . getDstIlocRegs . getMarkedData) revPrevInsns

foldDefs :: (MarkedGraph, Worklist) -> VertexReach -> (MarkedGraph, Worklist)
foldDefs (oldGraph, oldWork) (_, (vert, ndx)) = (newGraph, newWork)
    where block = getData $ oldGraph `at` vert
          savedInsn = block !! ndx
          (newGraph, newWork) = if isMarked savedInsn
                                   then (oldGraph, oldWork)
                                   else (adjustGraph (markInsn ndx) vert oldGraph, 
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
createReachingDefs graph = iterateReaches graph firstPass
    where firstPass = foldl' (createDef graph) HM.empty $ vertices graph

createDef :: IlocGraph -> Reaches -> Vertex -> Reaches
createDef graph reaches vert = HM.insert vert thisReach reaches
        where preds = filter (/= 0) $ graph `getPredecessors` vert
              predReach = S.map (createPredReach reaches graph) $ S.fromList preds
              startGen = S.map (makeVertexReach vert) $ createGen $ graph `at` vert
              thisReach = S.foldl' S.union S.empty predReach

createPredReach :: Reaches -> IlocGraph -> Vertex -> NodeReaches
createPredReach reaches graph vert = gen `S.union` (savedReach S.\\ kill)
    where (gen, kill) = S.map addVertex *** S.map addVertex 
                            $ createGenKills (graph `at` vert)
          addVertex = makeVertexReach vert
          savedReach = if vert `HM.member` reaches
                             then reaches HM.! vert
                             else S.empty
          
iterateReaches :: IlocGraph -> Reaches -> Reaches
iterateReaches graph old
    | old == new = old
    | otherwise = iterateReaches graph new
    where new = foldl' (createDef graph) old $ vertices graph

getReaches :: Vertex -> Reaches -> IlocGraph -> (NodeReaches, Reaches)
getReaches vert inProgress graph = (newReaches HM.! vert, newReaches)
    where newReaches = if vert `HM.member` inProgress
                           then inProgress
                           else createReaches vert inProgress graph

createReaches :: Vertex -> Reaches -> IlocGraph -> Reaches
createReaches vert inProgress graph = 
        HM.insert vert thisReach newReaches 
    where preds = filter (/= entryVertex) $ graph `getPredecessors` vert
          (newReaches, thisReach) = foldl' (reachFoldFun graph) 
                                        (inProgress, startDefs) preds
          startDefs = S.map (makeVertexReach vert) $ fst (createGenKills 
                            $ graph `at` vert)

reachFoldFun :: IlocGraph -> (Reaches, NodeReaches) -> Vertex -> (Reaches, NodeReaches)
reachFoldFun graph (oldReach, reachSoFar) vert = 
        (newReach, reachSoFar `S.union` tempReach)
    where node = graph `at` vert
          (gen, kill) = S.map addVertex *** S.map addVertex $ createGenKills node
          addVertex = makeVertexReach vert
          (thisReach, newReach) = getReaches vert oldReach graph
          tempReach = gen `S.union` (thisReach S.\\ kill)

makeVertexReach :: Vertex -> IlocDef -> VertexReach
makeVertexReach vert (r, i) = (r, (vert, i))

createGen :: IlocNode -> ReachGen
createGen = fst . createGenKills

createGenKills :: IlocNode -> (ReachGen, ReachKill)
createGenKills = foldl' genKillFoldFun (S.empty, S.empty) . zip [0..] . getData

genKillFoldFun :: (ReachGen, ReachKill) -> (Index, Iloc) -> (ReachGen, ReachKill)
genKillFoldFun (gen, kill) (ndx, iloc) = (newGen, newKill)
        where dstList = getDstIlocRegs iloc
              dstSet = S.fromList $ map createTup dstList
              newKill = kill `S.union` S.map createTup (S.fromList $ getSrcIlocRegs iloc)
              newGen = S.filter (not . (`elem` dstList) . fst) gen `S.union` dstSet
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
isCritical PrintILOC{} = True
isCritical ReadILOC{} = True
isCritical PrepArgs{} = True
isCritical Comp{} = True
isCritical Compi{} = True
isCritical UnprepArgs{} = True
isCritical Storeoutargument{} = True
isCritical Storeai{} = True
isCritical Del{} = True
isCritical Storeglobal{} = True
isCritical Println{} = True
isCritical _ = False
