module Mini.RegAlloc
  ( GenSet
  , KillSet
  , GenSetLookup
  , KillSetLookup
  , createGenKillSets
  , findGenAndKill
  , LiveOutLookup
  , InterferenceGraph
  ) where

import Data.HashMap.Strict hiding (filter, null, foldl, foldr, foldl')
import Data.Graph hiding (Node)
import qualified Data.List as L
import Data.Array hiding ((!))

import Mini.Iloc.Types
import Mini.CFG

type GenSet = [Reg]
type KillSet = [Reg]

-- maps a node to the list of registers in its gen set
type GenSetLookup = HashMap Vertex GenSet
-- maps a node to the list of registers in its kill set
type KillSetLookup = HashMap Vertex KillSet

-- finds the gen and kill sets of a node graph
createGenKillSets :: NodeGraph -> (GenSetLookup, KillSetLookup)
createGenKillSets (graph, vertToNodeHM) =
  (fromList vertGenTups, fromList vertKillTups)
  where
    vertGenTups = Prelude.map (\(vertex, (gen, kill)) -> (vertex, gen)) vertGenKillTups
    vertKillTups = Prelude.map (\(vertex, (gen, kill)) -> (vertex, kill)) vertGenKillTups
    vertGenKillTups = Prelude.map (\(vertex, node) -> (vertex, findGenAndKill node)) vertNodeTups
    vertNodeTups = toList vertToNodeHM

-- takes a node and returns its gen and kill set
findGenAndKill :: Node -> (GenSet, KillSet)
findGenAndKill (Node _ iloc) = findGenAndKillHelper iloc [] []
  where
    findGenAndKillHelper [] genSet killSet = (genSet, killSet)
    findGenAndKillHelper (x:rest) genSet killSet = findGenAndKillHelper rest nextGenSet nextKillSet
      where
        nextGenSet = (L.union genSet (srcRegs L.\\ killSet))
        nextKillSet = (L.union killSet dstRegs)
        srcRegs = getSrcRegs x
        dstRegs = getDstRegs x

type LiveOutLookup = HashMap Vertex [Reg]

createLiveOut :: NodeGraph ->  GenSetLookup -> KillSetLookup -> LiveOutLookup
createLiveOut (nodeGraph, vertToNodeHM) vertToGenHM vertToKillHM =
  actuallyCreateLiveOut startingLiveOutHM vertToGenHM vertToKillHM (nodeGraph, vertToNodeHM)
  where
    startingLiveOutHM = fromList $ L.map (\x -> (x, [])) $ vertices nodeGraph

actuallyCreateLiveOut :: LiveOutLookup -> GenSetLookup -> KillSetLookup -> NodeGraph -> LiveOutLookup
actuallyCreateLiveOut stuffSoFar genSetHM killSetHM (nodeGraph, vertToNodeHM)
  | (L.sort newStuff) == (L.sort (toList stuffSoFar)) = stuffSoFar
  | otherwise = actuallyCreateLiveOut (fromList newStuff) genSetHM killSetHM (nodeGraph, vertToNodeHM)
  where
    vertices = Prelude.map fst (toList stuffSoFar)
    newStuff = Prelude.map (\x -> (x, getLiveOutOfVert x stuffSoFar (nodeGraph, vertToNodeHM) genSetHM killSetHM)) vertices
    getLiveOutOfVert vert liveOutHM (graph, hm) genSetHM killSetHM =
      L.foldl' (L.union) [] (L.map (\x -> L.union (genSetHM ! x) ((liveOutHM ! x) L.\\ (killSetHM ! x))) successors)
      where
        successors = getSuccessors (graph, hm) vert

type InterferenceGraph = Graph

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
    newGraph = (buildG (bounds graph) (filteredOldEdges ++ newEdges))

-- uses heuristic to pick the next vertex to pull out of interference graph
-- assumes graph is not empty
pickNextVertex :: InterferenceGraph -> Vertex
pickNextVertex graph = head $ vertices graph -- TODO: pick better heuristic

type Color = Int
colors = [42..0xDEADBEEF] -- TODO: implement me
spillColor = -1

type ColorLookup = HashMap Vertex Color

theEmptyGraph = (buildG (0, 0) []) 

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
    neighborColors = [ colorHM ! n | n <- neighbors `L.intersect` (Data.HashMap.Strict.elems colorHM) ]
    availColors = colors L.\\ neighborColors
