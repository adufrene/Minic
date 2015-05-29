module Mini.RegAlloc
  ( testIntGraph
  , getRegLookup
  , getSrcIlocRegs
  , getDstIlocRegs
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

-- asm registers we will read from for this instuction
getSrcAsmRegs :: Iloc -> [AsmReg]
getSrcAsmRegs (Div r1 r2 r3) = [Rdx, Rax]

getSrcAsmRegs (Loadinargument _ i _) = getArgRegister i

getSrcAsmRegs Call{} = argRegs

getSrcAsmRegs New{} = [Rax, Rdi]
getSrcAsmRegs (Del r1) = [Rdi]

getSrcAsmRegs (PrintILOC r1) = [Rdi, Rsi, Rax]
getSrcAsmRegs (Println r1) = [Rdi, Rsi, Rax]
getSrcAsmRegs ReadILOC{} = [Rdi, Rsi, Rax]

getSrcAsmRegs _ = []

-- iloc registers we will read from for this instuction
getSrcIlocRegs :: Iloc -> [Reg]
getSrcIlocRegs (Add r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Addi r1 _ r3) = [r1, r3]
getSrcIlocRegs (Div r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Mult r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Multi r1 _ r2) = [r1, r2]
getSrcIlocRegs (Sub r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Rsubi r1 _ _) = [r1]

getSrcIlocRegs (And r1 r2 _) = [r1, r2]
getSrcIlocRegs (Or r1 r2 _) = [r1, r2]
getSrcIlocRegs (Xori r1 _ _) = [r1]

getSrcIlocRegs (Comp r1 r2) = [r1, r2]
getSrcIlocRegs (Compi r1 _) = [r1]

getSrcIlocRegs Cbreq{} = []
getSrcIlocRegs Cbrge{} = []
getSrcIlocRegs Cbrgt{} = []
getSrcIlocRegs Cbrle{} = []
getSrcIlocRegs Cbrlt{} = []
getSrcIlocRegs Cbrne{} = []
getSrcIlocRegs Jumpi{} = []
getSrcIlocRegs (Brz r1 _ _) = [r1]

getSrcIlocRegs (Loadai r1 _ _) = [r1]
getSrcIlocRegs Loadglobal{} = []
getSrcIlocRegs (Loadinargument _ _ i) = []
getSrcIlocRegs Loadret{} = []
getSrcIlocRegs Computeformaladdress{} = []
getSrcIlocRegs Restoreformal{} = []
getSrcIlocRegs Computeglobaladdress{} = []

getSrcIlocRegs (Storeai r1 r2 _) = [r1, r2]
getSrcIlocRegs (Storeglobal r1 _) = [r1]
getSrcIlocRegs (Storeinargument r1 _ _) = [r1]
getSrcIlocRegs (Storeoutargument r1 _) = [r1]
getSrcIlocRegs (Storeret r1) = [r1]

getSrcIlocRegs Call{} = []
getSrcIlocRegs RetILOC = []

getSrcIlocRegs New{} = []
getSrcIlocRegs (Del r1) = [r1]

getSrcIlocRegs (PrintILOC r1) = [r1]
getSrcIlocRegs (Println r1) = [r1]
getSrcIlocRegs ReadILOC{} = []

getSrcIlocRegs (Mov r1 _) = [r1]
getSrcIlocRegs Movi{} = []
getSrcIlocRegs (Moveq r1 r2) = [r1, r2]
getSrcIlocRegs (Movge r1 r2) = [r1, r2]
getSrcIlocRegs (Movgt r1 r2) = [r1, r2]
getSrcIlocRegs (Movle r1 r2) = [r1, r2]
getSrcIlocRegs (Movlt r1 r2) = [r1, r2]
getSrcIlocRegs (Movne r1 r2) = [r1, r2]

getSrcIlocRegs PrepArgs{} = []
getSrcIlocRegs UnprepArgs{} = []

getSrcIlocRegs iloc = error $ "unexpected input " ++ show iloc

-- registers we will write to for this instruction
getDstRegs :: Iloc -> [AsmReg]
getDstRegs iloc = getDstAsmRegs iloc ++ fmap RegNum (getDstIlocRegs iloc)

-- get the asm dest registers
getDstAsmRegs :: Iloc -> [AsmReg]
getDstAsmRegs (Div _ _ r3) = [Rax, Rdx]

getDstAsmRegs (Storeoutargument _ i) = getArgRegister i

getDstAsmRegs Call{} = callerSaved

getDstAsmRegs (New _ r1) = callerSaved
getDstAsmRegs Del{} = callerSaved

getDstAsmRegs PrintILOC{} = callerSaved
getDstAsmRegs Println{} = callerSaved
getDstAsmRegs (ReadILOC r) = callerSaved

getDstAsmRegs iloc = []

-- get the iloc dest registers
getDstIlocRegs :: Iloc -> [Reg]
getDstIlocRegs (Add _ _ r3) = [r3]
getDstIlocRegs (Addi _ _ r2) = [r2]
getDstIlocRegs (Div _ _ r3) = [r3]
getDstIlocRegs (Mult _ _ r3) = [r3]
getDstIlocRegs (Multi _ _ r2) = [r2]
getDstIlocRegs (Sub _ _ r3) = [r3]
getDstIlocRegs (Rsubi _ _ r2) = [r2]

getDstIlocRegs (And _ _ r3) = [r3]
getDstIlocRegs (Or _ _ r3) = [r3]
getDstIlocRegs (Xori _ _ r2) = [r2]

getDstIlocRegs Comp{} = []
getDstIlocRegs Compi{} = []

getDstIlocRegs Cbreq{} = []
getDstIlocRegs Cbrge{} = []
getDstIlocRegs Cbrgt{} = []
getDstIlocRegs Cbrle{} = []
getDstIlocRegs Cbrlt{} = []
getDstIlocRegs Cbrne{} = []
getDstIlocRegs Jumpi{} = []
getDstIlocRegs Brz{} = []

getDstIlocRegs (Loadai _ _ r2) = [r2]
getDstIlocRegs (Loadglobal _ r1) = [r1]
getDstIlocRegs (Loadinargument _ _ r1) = [r1]
getDstIlocRegs (Loadret r1) = [r1]
getDstIlocRegs (Computeformaladdress _ _ r1) = [r1]
getDstIlocRegs Restoreformal{} = []
getDstIlocRegs (Computeglobaladdress _ r1) = [r1]

getDstIlocRegs Storeai{} = []
getDstIlocRegs Storeglobal{} = []
getDstIlocRegs Storeinargument{} = []
getDstIlocRegs (Storeoutargument _ i) = []
getDstIlocRegs Storeret{} = []

getDstIlocRegs Call{} = []
getDstIlocRegs RetILOC = []

getDstIlocRegs (New _ r1) = [r1]
getDstIlocRegs Del{} = []

getDstIlocRegs PrintILOC{} = []
getDstIlocRegs Println{} = []
getDstIlocRegs (ReadILOC r) = [r]

getDstIlocRegs (Mov _ r2) = [r2]
getDstIlocRegs (Movi _ r1) = [r1]
getDstIlocRegs (Moveq _ r) = [r]
getDstIlocRegs (Movge _ r) = [r]
getDstIlocRegs (Movgt _ r) = [r]
getDstIlocRegs (Movle _ r) = [r]
getDstIlocRegs (Movlt _ r) = [r]
getDstIlocRegs (Movne _ r) = [r]

getDstIlocRegs PrepArgs{} = []
getDstIlocRegs UnprepArgs{} = []

getDstIlocRegs iloc = error $ "unexpected input " ++ show iloc

getArgRegister :: Immed -> [AsmReg]
getArgRegister i
    | i < numArgRegs = [argRegs !! i]
    | otherwise = []

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
