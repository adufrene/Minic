module Mini.RegAlloc
  ( testIntGraph
  , getRegLookup
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
import Mini.CFG

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
getSrcRegs (Add r1 r2 r3) = [RegNum r1, RegNum r2, RegNum r3]
getSrcRegs (Addi r1 _ r3) = [RegNum r1, RegNum r3]
getSrcRegs (Div r1 r2 r3) = [RegNum r1, RegNum r2, RegNum r3, Rdx, Rax]
getSrcRegs (Mult r1 r2 r3) = [RegNum r1, RegNum r2, RegNum r3]
getSrcRegs (Multi r1 _ r2) = [RegNum r1, RegNum r2]
getSrcRegs (Sub r1 r2 r3) = [RegNum r1, RegNum r2, RegNum r3]
getSrcRegs (Rsubi r1 _ _) = [RegNum r1]

getSrcRegs (And r1 r2 _) = [RegNum r1, RegNum r2]
getSrcRegs (Or r1 r2 _) = [RegNum r1, RegNum r2]
getSrcRegs (Xori r1 _ _) = [RegNum r1]

getSrcRegs (Comp r1 r2) = [RegNum r1, RegNum r2]
getSrcRegs (Compi r1 _) = [RegNum r1]

getSrcRegs Cbreq{} = []
getSrcRegs Cbrge{} = []
getSrcRegs Cbrgt{} = []
getSrcRegs Cbrle{} = []
getSrcRegs Cbrlt{} = []
getSrcRegs Cbrne{} = []
getSrcRegs Jumpi{} = []
getSrcRegs (Brz r1 _ _) = [RegNum r1]

getSrcRegs (Loadai r1 _ _) = [RegNum r1]
getSrcRegs Loadglobal{} = []
getSrcRegs (Loadinargument _ _ i) = getArgRegister i
getSrcRegs Loadret{} = []
getSrcRegs Computeformaladdress{} = []
getSrcRegs Restoreformal{} = []
getSrcRegs Computeglobaladdress{} = []

getSrcRegs (Storeai r1 r2 _) = [RegNum r1, RegNum r2]
getSrcRegs (Storeglobal r1 _) = [RegNum r1]
getSrcRegs (Storeinargument r1 _ _) = [RegNum r1]
getSrcRegs (Storeoutargument r1 _) = [RegNum r1]
getSrcRegs (Storeret r1) = [RegNum r1]

getSrcRegs Call{} = argRegs
getSrcRegs RetILOC = []

getSrcRegs New{} = [Rax, Rdi]
getSrcRegs (Del r1) = [Rdi, RegNum r1]

getSrcRegs (PrintILOC r1) = [Rdi, Rsi, Rax, RegNum r1]
getSrcRegs (Println r1) = [RegNum r1, Rdi, Rsi, Rax]
getSrcRegs ReadILOC{} = [Rdi, Rsi, Rax]

getSrcRegs (Mov r1 _) = [RegNum r1]
getSrcRegs Movi{} = []
getSrcRegs (Moveq r1 r2) = [RegNum r1, RegNum r2]
getSrcRegs (Movge r1 r2) = [RegNum r1, RegNum r2]
getSrcRegs (Movgt r1 r2) = [RegNum r1, RegNum r2]
getSrcRegs (Movle r1 r2) = [RegNum r1, RegNum r2]
getSrcRegs (Movlt r1 r2) = [RegNum r1, RegNum r2]
getSrcRegs (Movne r1 r2) = [RegNum r1, RegNum r2]

getSrcRegs PrepArgs{} = []
getSrcRegs UnprepArgs{} = []

getSrcRegs iloc = error $ "unexpected input " ++ show iloc

-- registers we will write to for this instruction
getDstRegs :: Iloc -> [AsmReg]
getDstRegs (Add _ _ r3) = [RegNum r3]
getDstRegs (Addi _ _ r2) = [RegNum r2]
getDstRegs (Div _ _ r3) = [RegNum r3, Rax, Rdx]
getDstRegs (Mult _ _ r3) = [RegNum r3]
getDstRegs (Multi _ _ r2) = [RegNum r2]
getDstRegs (Sub _ _ r3) = [RegNum r3]
getDstRegs (Rsubi _ _ r2) = [RegNum r2]

getDstRegs (And _ _ r3) = [RegNum r3]
getDstRegs (Or _ _ r3) = [RegNum r3]
getDstRegs (Xori _ _ r2) = [RegNum r2]

getDstRegs Comp{} = []
getDstRegs Compi{} = []

getDstRegs Cbreq{} = []
getDstRegs Cbrge{} = []
getDstRegs Cbrgt{} = []
getDstRegs Cbrle{} = []
getDstRegs Cbrlt{} = []
getDstRegs Cbrne{} = []
getDstRegs Jumpi{} = []
getDstRegs Brz{} = []

getDstRegs (Loadai _ _ r2) = [RegNum r2]
getDstRegs (Loadglobal _ r1) = [RegNum r1]
getDstRegs (Loadinargument _ _ r1) = [RegNum r1]
getDstRegs (Loadret r1) = [RegNum r1]
getDstRegs (Computeformaladdress _ _ r1) = [RegNum r1]
getDstRegs Restoreformal{} = []
getDstRegs (Computeglobaladdress _ r1) = [RegNum r1]

getDstRegs Storeai{} = []
getDstRegs Storeglobal{} = []
getDstRegs Storeinargument{} = []
getDstRegs (Storeoutargument _ i) = getArgRegister i
getDstRegs Storeret{} = []

getDstRegs Call{} = callerSaved
getDstRegs RetILOC = []

getDstRegs (New _ r1) = RegNum r1 : callerSaved
getDstRegs Del{} = callerSaved

getDstRegs PrintILOC{} = [] ++ callerSaved
getDstRegs Println{} = [] ++ callerSaved
getDstRegs (ReadILOC r) = RegNum r : callerSaved

getDstRegs (Mov _ r2) = [RegNum r2]
getDstRegs (Movi _ r1) = [RegNum r1]
getDstRegs (Moveq _ r) = [RegNum r]
getDstRegs (Movge _ r) = [RegNum r]
getDstRegs (Movgt _ r) = [RegNum r]
getDstRegs (Movle _ r) = [RegNum r]
getDstRegs (Movlt _ r) = [RegNum r]
getDstRegs (Movne _ r) = [RegNum r]

getDstRegs PrepArgs{} = []
getDstRegs UnprepArgs{} = []

getDstRegs iloc = error $ "unexpected input " ++ show iloc

getArgRegister :: Immed -> [AsmReg]
getArgRegister i
    | i < numArgRegs = [argRegs !! i]
    | otherwise = []

type GenSet = Set.Set AsmReg
type KillSet = Set.Set AsmReg

-- maps a node to the list of registers in its gen set
type GenSetLookup = HashMap Vertex GenSet
-- maps a node to the list of registers in its kill set
type KillSetLookup = HashMap Vertex KillSet

type GenKillLookup = HashMap Vertex (GenSet, KillSet)

-- finds the gen and kill sets of a node graph
createGenKillSets :: NodeGraph -> GenKillLookup
createGenKillSets = map findGenAndKill . snd

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

type LiveOutLookup = HashMap Vertex (Set.Set AsmReg)

createLiveOut :: NodeGraph ->  GenKillLookup -> LiveOutLookup
createLiveOut (nodeGraph, vertToNodeHM) lookup' =
  actuallyCreateLiveOut startingLiveOutHM lookup' (nodeGraph, vertToNodeHM)
  where
    startingLiveOutHM = fromList $ L.map (\x -> (x, Set.empty)) $ vertices nodeGraph

{- Switch hashmap parameter to Set -}
actuallyCreateLiveOut :: LiveOutLookup -> GenKillLookup -> NodeGraph -> LiveOutLookup
actuallyCreateLiveOut stuffSoFar gkLookup ng
  | newStuff == Set.fromList (toList stuffSoFar) = stuffSoFar
  | otherwise = actuallyCreateLiveOut (fromList $ Set.toList newStuff) gkLookup ng
  where
    verts = Set.fromList $ keys stuffSoFar -- Set.map fst (Set.fromList $ toList stuffSoFar)
    newStuff = Set.map (\x -> (x, getLiveOutOfVert x stuffSoFar ng gkLookup)) verts
    getLiveOutOfVert vert liveOutHM (graph, hm) lookup' =
      Set.foldl' Set.union Set.empty (Set.map (\x -> fst (lookup' ! x) `Set.union` ((liveOutHM ! x) Set.\\ snd (lookup' ! x))) successors)
      where
        successors = Set.fromList $ getSuccessors (graph, hm) vert

type InterferenceGraph = Graph

createInterferenceGraph :: NodeGraph -> LiveOutLookup -> InterferenceGraph
createInterferenceGraph (_, nodeHash) lookup =
        makeUndirected $ foldlWithKey' foldFun theEmptyGraph nodeHash
    where foldFun graph key node = fst $ foldr foldIntGraph (graph, lookup ! key)
                                        $ getIloc node

foldIntGraph :: Iloc -> (Graph, Set.Set AsmReg) -> (Graph, Set.Set AsmReg)
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

type DeconstructionStack = [(Vertex, [Vertex])]

deconstructInterferenceGraph :: InterferenceGraph -> DeconstructionStack
deconstructInterferenceGraph = flip actuallyDeconstructInterferenceGraph []

actuallyDeconstructInterferenceGraph :: InterferenceGraph -> DeconstructionStack -> DeconstructionStack
actuallyDeconstructInterferenceGraph graph stack
  | emptyGraph graph = stack
  | nextVertex == 0 = actuallyDeconstructInterferenceGraph newGraph stack
  | otherwise = actuallyDeconstructInterferenceGraph newGraph newStack
  where
    nextVertex = pickNextVertex graph
    neighbors = getNeighbors graph nextVertex
    newStack = push stack (nextVertex, neighbors)
    newGraph = removeVertex graph nextVertex

-- uses heuristic to pick the next vertex to pull out of interference graph
-- assumes graph is not empty
pickNextVertex :: InterferenceGraph -> Vertex
pickNextVertex graph = last $ vertices graph -- TODO: pick better heuristic

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
actuallyReconstruct [] _ colorHM = colorHM
actuallyReconstruct ((nextVert, neighbors):rest) interferenceGraph colorHM =
  actuallyReconstruct rest newInterferenceGraph newHM
  where
    newEdges = [ (nextVert, nextNeighbor) | nextNeighbor <- neighbors ]
    newInterferenceGraph = addEdges interferenceGraph newEdges
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

getRegLookup :: NodeGraph -> RegLookup
getRegLookup graph = fst $ foldlWithKey' foldFun (empty, 1) colorLookup
    where colorLookup = colorGraph intGraph
          intGraph = createInterferenceGraph graph loLookup
          loLookup = createLiveOut graph gkLookup
          gkLookup = createGenKillSets graph
          foldFun (hash, nextLocal) key clr = 
            if clr == 0
                then (insert key (BaseOffset $ -nextLocal) hash, nextLocal + 1)
                else (insert key (fromMaybe 
                        (error $ "Invalid vertex: " ++ show clr)
                            $ clr `L.lookup` vertRegList) hash, nextLocal)

testIntGraph :: NodeGraph -> InterferenceGraph
testIntGraph graph = createInterferenceGraph graph $ createLiveOut graph $ createGenKillSets graph
