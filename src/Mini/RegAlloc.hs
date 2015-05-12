module Mini.RegAlloc
  ( GenSet
  , KillSet
  , GenSetLookup
  , KillSetLookup
  , createGenKillSets
  , findGenAndKill
  , LiveOutLookup
  , InterferenceGraph
  , testIntGraph
  ) where

import Control.Arrow
import Data.HashMap.Strict hiding (filter, null, foldl, foldr, foldl')
import Data.Graph hiding (Node)
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Array hiding ((!), elems)

import Mini.Iloc.Types
import Mini.CFG

type GenSet = Set.Set Reg
type KillSet = Set.Set Reg

-- maps a node to the list of registers in its gen set
type GenSetLookup = HashMap Vertex GenSet
-- maps a node to the list of registers in its kill set
type KillSetLookup = HashMap Vertex KillSet

type GenKillLookup = HashMap Vertex (GenSet, KillSet)

-- finds the gen and kill sets of a node graph
createGenKillSets :: NodeGraph -> GenKillLookup
createGenKillSets (graph, vertToNodeHM) = fromList vertGenKillTups 
  where 
    vertGenTups = L.map (\(vertex, (gen, _)) -> (vertex, gen)) vertGenKillTups
    vertKillTups = L.map (\(vertex, (_, kill)) -> (vertex, kill)) vertGenKillTups
    vertGenKillTups = L.map (second findGenAndKill) vertNodeTups
    vertNodeTups = toList vertToNodeHM

-- takes a node and returns its gen and kill set
findGenAndKill :: Node -> (GenSet, KillSet)
findGenAndKill (Node _ iloc) = findGenAndKillHelper iloc Set.empty Set.empty
  where
    findGenAndKillHelper [] genSet killSet = (genSet, killSet)
    findGenAndKillHelper (x:rest) genSet killSet = findGenAndKillHelper rest nextGenSet nextKillSet
      where
        nextGenSet = genSet `Set.union` (srcRegs Set.\\ killSet)
        nextKillSet = killSet `Set.union` dstRegs
        srcRegs = Set.fromList $ getSrcRegs x
        dstRegs = Set.fromList $ getDstRegs x

type LiveOutLookup = HashMap Vertex (Set.Set Reg)

createLiveOut :: NodeGraph ->  GenKillLookup -> LiveOutLookup
createLiveOut (nodeGraph, vertToNodeHM) lookup =
  actuallyCreateLiveOut startingLiveOutHM lookup (nodeGraph, vertToNodeHM)
  where
    startingLiveOutHM = fromList $ L.map (\x -> (x, Set.empty)) $ vertices nodeGraph

actuallyCreateLiveOut :: LiveOutLookup -> GenKillLookup -> NodeGraph -> LiveOutLookup
actuallyCreateLiveOut stuffSoFar lookup (nodeGraph, vertToNodeHM)
  | Set.null (newStuff Set.\\ Set.fromList (toList stuffSoFar)) = stuffSoFar
  | otherwise = actuallyCreateLiveOut (fromList $ Set.toList newStuff) lookup (nodeGraph, vertToNodeHM)
  where
    vertices = Set.map fst (Set.fromList $ toList stuffSoFar)
    newStuff = Set.map (\x -> (x, getLiveOutOfVert x stuffSoFar (nodeGraph, vertToNodeHM) lookup)) vertices
    getLiveOutOfVert vert liveOutHM (graph, hm) lookup =
      Set.foldl' Set.union Set.empty (Set.map (\x -> fst (lookup ! x) `Set.union` ((liveOutHM ! x) Set.\\ snd (lookup ! x))) successors)
      where
        successors = Set.fromList $ getSuccessors (graph, hm) vert

type InterferenceGraph = Graph

createInterferenceGraph :: NodeGraph -> LiveOutLookup -> InterferenceGraph
createInterferenceGraph (_, nodeHash) lookup = foldrWithKey foldFun (buildG (1,1) []) nodeHash 
    where foldFun key node graph = fst $ foldr foldIntGraph (graph, lookup ! key) $ getIloc node

foldIntGraph :: Iloc -> (Graph, Set.Set Reg) -> (Graph, Set.Set Reg)
foldIntGraph insn (graph, liveNow) = (newGraph, newLiveNow)
    where targetRegs = getDstRegs insn 
          sourceRegSet = Set.fromList $ getSrcRegs insn
          newLiveNow = Set.union sourceRegSet $ Set.filter (`elem` targetRegs) liveNow
          newGraph = connectVertices graph targetRegs $ Set.toList liveNow

connectVertices :: Graph -> [Reg] -> [Reg] -> Graph
connectVertices graph srcRegs destRegs = buildG (lowerBound, upperBound) newEdges
    where upperBound = max currUpperBound maxReg 
          lowerBound = fst $ bounds graph
          currUpperBound = snd $ bounds graph
          maxReg = safeMaximum 0 $ srcRegs `L.union` destRegs
          newEdges = edges graph `L.union` L.concatMap (`createEdges` destRegs) srcRegs
          createEdges reg = L.nub . L.map (\dest -> (reg, dest)) . filter(/= reg)

type DeconstructionStack = [(Vertex, [Vertex])]

deconstructInterferenceGraph :: InterferenceGraph -> DeconstructionStack
deconstructInterferenceGraph graph = actuallyDeconstructInterferenceGraph graph []

actuallyDeconstructInterferenceGraph :: InterferenceGraph -> DeconstructionStack -> DeconstructionStack
actuallyDeconstructInterferenceGraph graph stack
  | emptyGraph graph = stack
  | otherwise = actuallyDeconstructInterferenceGraph newGraph newStack
  where
    nextVertex = pickNextVertex graph
    neighbors = getNeighbors graph nextVertex
    newEdges = [(nextVertex , nextNeighbor) | nextNeighbor <- neighbors]
    filteredOldEdges = [(v1, v2) | (v1, v2) <- edges graph, (v1 /= nextVertex) && (v2 /= nextVertex)]
    newStack = push stack (nextVertex, neighbors)
    newGraph = buildG (bounds graph) (filteredOldEdges ++ newEdges)

-- uses heuristic to pick the next vertex to pull out of interference graph
-- assumes graph is not empty
pickNextVertex :: InterferenceGraph -> Vertex
pickNextVertex graph = head $ vertices graph -- TODO: pick better heuristic

type Color = Int
colors = [42..0xDEADBEEF] -- TODO: implement me
spillColor = -1

type ColorLookup = HashMap Vertex Color

theEmptyGraph = buildG (1, 1) []

reconstructInterferenceGraph :: DeconstructionStack -> ColorLookup
reconstructInterferenceGraph stack = actuallyReconstruct stack theEmptyGraph empty

actuallyReconstruct :: DeconstructionStack -> InterferenceGraph -> ColorLookup -> ColorLookup
actuallyReconstruct [] _ colorHM = colorHM
actuallyReconstruct ((nextVert, neighbors):rest) interferenceGraph colorHM =
  actuallyReconstruct rest newInterferenceGraph newHM
  where
    newEdges = [ (nextVert, nextNeighbor) | nextNeighbor <- neighbors ]
    newInterferenceGraph = addEdges interferenceGraph newEdges
    color = pickColor nextVert newInterferenceGraph colorHM
    newHM = insert nextVert color colorHM

-- picks the first color that not used by any of its neighbors
pickColor :: Vertex -> InterferenceGraph -> ColorLookup -> Color
pickColor nextVert graph colorHM
  | null availColors = spillColor
  | otherwise = head availColors
  where
    neighbors = getNeighbors graph nextVert
    neighborColors = [ colorHM ! n | n <- neighbors `L.intersect` elems colorHM]
    availColors = colors L.\\ neighborColors

safeMaximum :: Ord a => a -> [a] -> a
safeMaximum def [] = def
safeMaximum _ l = L.maximum l

testIntGraph :: NodeGraph -> InterferenceGraph
testIntGraph graph = createInterferenceGraph graph $ createLiveOut graph $ createGenKillSets graph
