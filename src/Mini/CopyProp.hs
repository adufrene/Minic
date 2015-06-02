module Mini.CopyProp
  ( copyPropOptimize
  , doCopyProp
  ) where


import Mini.Iloc.Types
import Mini.RegAlloc
import Mini.CFG
import Mini.Graph

import Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Data.Graph hiding (Node)
import Data.List as L

import Debug.Trace

type CopySet = Set.Set (Reg, Reg) -- {(src, dst)..} where each src is replaced with dst
type CopyInSet = CopySet
type GenSet = CopySet
type KillSet = Set.Set Reg

type GenKillLookup = HashMap Vertex (GenSet, KillSet)
type CopyInLookup = HashMap Vertex CopyInSet

type CopyPropBaggage = (GenKillLookup, CopyInLookup)

-- find the vertices in a NodeGraph
getVertices :: IlocGraph -> [Vertex]
getVertices (_, vertToNodeHM) = keys vertToNodeHM

-- find all the predecessors of a given node
getNodeGraphPredecessors :: IlocGraph -> Vertex -> Set.Set Vertex
getNodeGraphPredecessors nodeGraph@(graph, _) vertex = (Set.fromList [start | (start, end) <- edges graph, end == vertex, start `elem` (getVertices nodeGraph)])

-- applies copy propegation to a list of node graphs, can be turned off with a flag
doCopyProp :: [IlocGraph] -> Bool -> [IlocGraph]
doCopyProp graphs False = graphs
doCopyProp graphs True = L.map copyPropOptimize graphs

-- applies the copy propegation optimization to the given node graph
copyPropOptimize :: IlocGraph -> IlocGraph
copyPropOptimize nodeGraph@(graph, vertToNodeHM) = (graph, optimizedNodes)
  where
    optimizedNodes = HM.fromList [(vert, applyCopyPropForThisNode vert) | vert <- nodeVertices]
    nodeVertices = getVertices nodeGraph
    vertPreds = [(vert, getNodeGraphPredecessors nodeGraph vert) | vert <- nodeVertices]
    genKillHM = createGenKillLookup nodeGraph
    copyInHM = createCopyInLookup nodeGraph genKillHM
    baggage = (genKillHM, copyInHM)
    applyCopyPropForThisNode = applyCopyProp nodeGraph baggage

-- applies copy propegation to a given node
-- requires gen, kill, and copyIn sets
applyCopyProp :: IlocGraph -> CopyPropBaggage -> Vertex -> IlocNode
applyCopyProp (_, vertToNodeHM) (genKillHM, copyInHM) vert = (Node label optimizedInsns)
  where
    node@(Node label insns) = vertToNodeHM ! vert
    optimizedInsns = doIt (copyInHM ! vert) insns
    doIt :: CopySet -> [Iloc] -> [Iloc]
    doIt _ [] = []
    doIt copyNow (insn:rest) = nextInsn:(doIt nextCopyNow rest)
      where
        optimizedInsn = doReplacements insn copyNow
        dstRegs = Set.fromList $ getDstIlocRegs insn
        filteredCopyNow = copyNow `copiesNotKilledBy` dstRegs
        (nextInsn, nextCopyNow) = handleMov optimizedInsn filteredCopyNow
    handleMov :: Iloc -> CopySet -> (Iloc, CopySet)
    handleMov (Mov r1 r2) copyNow = ((Mov r1 r2), (r2, r1) `Set.insert` copyNow)
    handleMov insn copyNow = (insn, copyNow)

-- finds the copyIn set for each node
-- requires gen/kill sets
createCopyInLookup :: IlocGraph -> GenKillLookup -> CopyInLookup
createCopyInLookup nodeGraph genKillHM = doIt startingCopyInHM
  where
    -- copyIn is initialized to the empty set for each node
    startingCopyInHM = HM.fromList [(vert, Set.empty) | vert <- getVertices nodeGraph]
    -- recalculate copyIn for each node until nothing changes
    doIt copyInHM
      | nextCopyInHM == copyInHM = copyInHM
      | otherwise = doIt nextCopyInHM
      where
        verts = keys copyInHM
        nextCopyInHM = HM.fromList [(v, createCopyIn v nodeGraph (genKillHM, copyInHM)) | v <- verts]

-- finds the copyIn set of a given node relative to its predecessors
createCopyIn :: Vertex -> IlocGraph -> CopyPropBaggage -> CopyInSet
createCopyIn vert nodeGraph@(graph, _) (genKillHM, copyInHM) = foldIntersection predStuff
  where
    preds = Set.toList $ getNodeGraphPredecessors nodeGraph vert
    predStuff = L.map findPredStuff preds
    findPredStuff v = gen `Set.union` (copyIn `copiesNotKilledBy` kill)
      where
        (gen, kill) = genKillHM ! v
        copyIn = copyInHM ! v

-- folds a list of sets with intersection
-- retuns the empty set if argument list is empty
foldIntersection :: (Ord a) => [Set.Set a] -> Set.Set a
foldIntersection [] = Set.empty
foldIntersection (xs:rest) = L.foldl' Set.intersection xs rest

-- finds the gen/kill sets for each node
createGenKillLookup :: IlocGraph -> GenKillLookup
createGenKillLookup = HM.map createGenKill . snd

-- finds the gen/kill set of a given node
createGenKill :: IlocNode -> (GenSet, KillSet)
createGenKill (Node _ insns) = doIt Set.empty Set.empty (L.reverse insns)
  where
    doIt gen kill insns@(nextInsn@(Mov r1 r2):rest)
      | (r1 `Set.notMember` kill) && (r2 `Set.notMember` kill) = doIt nextGen nextKill rest
      | otherwise = doIt_noNewCopy gen kill insns
      where
        nextKill = Set.insert r2 kill
        filteredGen = gen `copiesNotKilledBy` nextKill
        nextGen = (r1, r2) `Set.insert` filteredGen
    doIt gen kill insns = doIt_noNewCopy gen kill insns
    doIt_noNewCopy gen kill [] = (gen, kill)
    doIt_noNewCopy gen kill (insn:rest) = doIt gen nextKill rest
      where
        dstRegs = Set.fromList $ getDstIlocRegs insn
        nextKill = kill `Set.union` dstRegs

-- filter copyIn set by kill set
copiesNotKilledBy :: CopySet -> KillSet -> CopySet
copiesNotKilledBy copyIn kill = Set.filter (\(src, dst) -> (src `Set.notMember` kill) && (dst `Set.notMember` kill)) copyIn

-- filter copyIn set by a killed register
copiesNotKilledByReg :: CopySet -> Reg -> CopySet
copiesNotKilledByReg copyIn kill = Set.filter (\(src, dst) -> ((src /= kill) && (dst /= kill))) copyIn

-- apply all of our copy replacements to a given iloc
doReplacements :: Iloc -> CopySet -> Iloc
doReplacements iloc copies = iloc `mapToSrcRegs` (replaceReg copies)

-- apply all the replacements in a copy set to a given register
replaceReg :: CopySet -> Reg -> Reg
replaceReg copyNow reg = Set.foldl' (\reg (src, dst) -> if reg == src then dst else reg) reg copyNow
