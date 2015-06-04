module Mini.RegAlloc
  ( testIntGraph
  , getRegLookup
  , colorGraph
  , createInterferenceGraph
  , createLiveOut
  , createGenKillSets
  , deconstructInterferenceGraph
  , reconstructInterferenceGraph
  ) where

import Control.Arrow
import Data.HashMap.Strict hiding (filter, null, foldl, foldr)
import Data.Graph hiding (Node)
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Array hiding ((!), elems)
import Data.Maybe
import Data.Tuple (swap)
import Prelude hiding (map)

import Debug.Trace

import Mini.Asm.Types
import Mini.Iloc.Types
import Mini.Graph
import Mini.Stack

regVertList :: [(AsmReg, Vertex)]
regVertList = [ (Rax, -1)
              , (Rbx, -2)
              , (Rcx, -3)
              , (Rdx, -4)
              , (Rsi, -5)
              , (Rdi, -6)
              , (R8, -7)
              , (R9, -8)
              , (R10, -9)
              , (R11, -10)
              , (R12, -11)
              , (R13, -12)  -- Spill Reg
              , (R14, -13)  -- Spill Reg
              , (R15, -14)  -- Spill Reg
              , (Rsp, -15)   -- Not assignable
              , (Rbp, -16)   -- Not assignable
              , (Rip, -17) ] -- Not assignable

vertRegList :: [(Vertex, AsmReg)]
vertRegList = L.map swap regVertList

-- registers we will read from for this instruction
getSrcRegs :: Iloc -> [AsmReg]
getSrcRegs iloc = getSrcAsmRegs iloc ++ fmap RegNum (getSrcIlocRegs iloc)

-- registers we will write to for this instruction
getDstRegs :: Iloc -> [AsmReg]
getDstRegs iloc = getDstAsmRegs iloc ++ fmap RegNum (getDstIlocRegs iloc)

type RegSet = Set.Set AsmReg

type GenSet = RegSet
type KillSet = RegSet

-- maps a node to the list of registers in its gen set
type GenSetLookup = HashMap Vertex GenSet
-- maps a node to the list of registers in its kill set
type KillSetLookup = HashMap Vertex KillSet

type GenKillLookup = HashMap Vertex (GenSet, KillSet)

-- finds the gen and kill sets of a node graph
createGenKillSets :: IlocGraph -> GenKillLookup
createGenKillSets = map findGenAndKill . snd

-- takes a node and returns its gen and kill set
findGenAndKill :: IlocNode -> (GenSet, KillSet)
findGenAndKill (Node _ iloc) = findGenAndKillHelper iloc Set.empty Set.empty
  where
    findGenAndKillHelper [] genSet killSet = (genSet, killSet)
    findGenAndKillHelper (x:rest) genSet killSet = findGenAndKillHelper rest nextGenSet nextKillSet
      where
        nextGenSet = genSet `Set.union` (srcRegs Set.\\ killSet)
        nextKillSet = killSet `Set.union` dstRegs
        srcRegs = Set.fromList $ getSrcRegs x
        dstRegs = Set.fromList $ getDstRegs x

type LiveOutLookup = HashMap Vertex RegSet

createLiveOut :: IlocGraph ->  GenKillLookup -> LiveOutLookup
createLiveOut (nodeGraph, vertToNodeHM) lookup' =
  actuallyCreateLiveOut startingLiveOutHM lookup' (nodeGraph, vertToNodeHM)
  where
    startingLiveOutHM = fromList $ L.map (\x -> (x, Set.empty)) $ vertices nodeGraph

{- Switch hashmap parameter to Set -}
actuallyCreateLiveOut :: LiveOutLookup -> GenKillLookup -> IlocGraph -> LiveOutLookup
actuallyCreateLiveOut stuffSoFar gkLookup ng
  | newStuff == Set.fromList (toList stuffSoFar) = stuffSoFar
  | otherwise = actuallyCreateLiveOut (fromList $ Set.toList newStuff) gkLookup ng
  where
    verts = Set.fromList $ keys stuffSoFar
    newStuff = Set.map (\x -> (x, getLiveOutOfVert x stuffSoFar ng gkLookup)) verts
    getLiveOutOfVert vert liveOutHM (graph, hm) lookup' =
      Set.foldl' Set.union Set.empty (Set.map (\x -> fst (lookup' ! x) `Set.union` ((liveOutHM ! x) Set.\\ snd (lookup' ! x))) successors)
      where
        successors = Set.fromList $ getSuccessors graph vert

type InterferenceGraph = Graph

createInterferenceGraph :: IlocGraph -> LiveOutLookup -> InterferenceGraph
createInterferenceGraph (_, nodeHash) lookup =
        foldlWithKey' foldFun theEmptyGraph nodeHash
    where foldFun graph key node = fst $ foldr foldIntGraph (graph, lookup ! key)
                                        $ getData node

foldIntGraph :: Iloc -> (InterferenceGraph, RegSet) -> (InterferenceGraph, RegSet)
foldIntGraph insn (graph, liveNow) = (newGraph, newLiveNow)
    where targetRegs = getDstRegs insn 
          sourceRegSet = Set.fromList $ getSrcRegs insn
          newLiveNow = Set.union sourceRegSet $ Set.filter (`notElem` targetRegs) liveNow
          newGraph = connectVertices graph (regToVert targetRegs) $ regToVert $ Set.toList liveNow

regToVert :: [AsmReg] -> [Vertex]
regToVert = L.map mapFun 
    where mapFun (RegNum r) = r
          mapFun r = fromMaybe (error $ "Invalid vertex: " ++ show r) 
                            $ r `L.lookup` regVertList 

connectVertices :: Graph -> [Vertex] -> [Vertex] -> Graph
connectVertices graph src dest = buildG newBnds newEdges
    where newBnds = newBounds (newBounds (bounds graph) src) dest
          newEdges = edges graph `L.union` L.concatMap (`createEdges` dest) src
          createEdges reg = L.nub . L.map (\dest -> (reg, dest)) . filter(/= reg)

newBounds :: Bounds -> [Vertex] -> Bounds
newBounds (oldLower, oldUpper) verts = (newLower, newUpper)
    where newUpper = max vertMax oldUpper
          newLower = min vertMin oldLower
          vertMax = safeMaximum oldUpper verts
          vertMin = safeMinimum oldLower verts

type DeconstructionStack = Stack (Vertex, [Vertex])

deconstructInterferenceGraph :: InterferenceGraph -> DeconstructionStack
deconstructInterferenceGraph graph = actuallyDeconstructInterferenceGraph (graph, vertices graph) (Stack [])

actuallyDeconstructInterferenceGraph :: (InterferenceGraph, [Vertex]) -> DeconstructionStack -> DeconstructionStack
actuallyDeconstructInterferenceGraph (graph, verts) stack
  | null verts = stack
  | nextVertex == 0 = actuallyDeconstructInterferenceGraph newGraph stack
  | otherwise = actuallyDeconstructInterferenceGraph newGraph newStack
  where
    nextVertex = pickNextVertex verts graph
    neighbors = getNeighbors graph nextVertex
    newStack = push stack (nextVertex, neighbors)
    newGraph = (removeVertex graph nextVertex, nextVertex `L.delete` verts)

-- uses heuristic to pick the next vertex to pull out of interference graph
-- assumes graph is not empty
pickNextVertex :: [Vertex] -> InterferenceGraph -> Vertex
-- pickNextVertex vs _ =  last vs
pickNextVertex verts graph
    | not $ null unconstrained = pickBest unconstrained
    | not $ null constrained = pickBest constrained
    | otherwise = head verts
    where (unconstrained, constrained) = L.partition partFun $ filter (>0) verts
          partFun x = length (getNeighbors graph x) < length colors
          pickBest = head


-- = last verts -- TODO: pick better heuristic

makeUndirected :: Graph -> Graph
makeUndirected dirGraph = buildG (bounds dirGraph) $ L.nub duppedEdges
    where duppedEdges = L.foldl' (\xs x -> swap x:x:xs) [] $ edges dirGraph

type Color = Int

colors :: [Color]
colors = [(-1),(-2)..(-11)]

spillColor :: Color
spillColor = 0

type ColorLookup = HashMap Vertex Color

theEmptyGraph :: Graph
theEmptyGraph = buildG (initVertex, initVertex) []

reconstructInterferenceGraph :: DeconstructionStack -> ColorLookup
reconstructInterferenceGraph stack = actuallyReconstruct stack theEmptyGraph empty

actuallyReconstruct :: DeconstructionStack -> InterferenceGraph -> ColorLookup -> ColorLookup
actuallyReconstruct stack graph colorHM
    | emptyStack stack = colorHM
    | otherwise = actuallyReconstruct rest newInterferenceGraph newHM
  where ((nextVert, neighbors), rest) = pop stack
        newEdges = [ (nextVert, nextNeighbor) | nextNeighbor <- neighbors ]
        newInterferenceGraph = addEdges graph newEdges
        newHM = insert nextVert color colorHM
        color = if nextVert > spillColor
                    then pickColor nextVert newInterferenceGraph colorHM
                    else nextVert 

-- picks the first color that not used by any of its neighbors
pickColor :: Vertex -> InterferenceGraph -> ColorLookup -> Color
pickColor nextVert graph colorHM
  | null availColors = spillColor
  | otherwise = head availColors
  where
    neighbors = getNeighbors graph nextVert
    forcedColor = filter (<0) neighbors
    coloredNeighbors = [ colorHM ! n | n <- filter (>0) neighbors]
    neighborColors = forcedColor `L.union` coloredNeighbors
    availColors = colors L.\\ neighborColors

safeMaximum :: Ord a => a -> [a] -> a
safeMaximum def [] = def
safeMaximum _ l = L.maximum l

safeMinimum :: Ord a => a -> [a] -> a
safeMinimum def [] = def
safeMinimum _ l = L.minimum l

colorGraph :: InterferenceGraph -> ColorLookup
colorGraph = reconstructInterferenceGraph . deconstructInterferenceGraph 

getRegLookup :: IlocGraph -> RegLookup
getRegLookup graph = fst $ foldlWithKey' foldFun (empty, 1) colorLookup
    where colorLookup = colorGraph intGraph
          intGraph = createInterferenceGraph graph loLookup
          loLookup = createLiveOut graph gkLookup
          gkLookup = createGenKillSets graph
          foldFun (hash, nextLocal) key clr = 
            if clr == 0
                then (insert key (LocalReg nextLocal) hash, nextLocal + 1)
                else (insert key (fromMaybe 
                        (error $ "Invalid vertex: " ++ show clr)
                            $ clr `L.lookup` vertRegList) hash, nextLocal)

testIntGraph :: IlocGraph -> InterferenceGraph
testIntGraph graph = createInterferenceGraph graph $ createLiveOut graph $ createGenKillSets graph
