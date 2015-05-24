module Mini.CopyProp
  ( copyPropOptimize
  , doCopyProp
  )
  where

import Mini.Iloc.Types
import Mini.RegAlloc
import Mini.CFG

import Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Data.Graph hiding (Node)
import Data.List as L

type CopySet = Set.Set (Reg, Reg) -- {(src, dst)..}
type CopyInSet = CopySet
type GenSet = CopyInSet
type KillSet = Set.Set Reg

type GenKillLookup = HashMap Vertex (GenSet, KillSet)
type CopyInLookup = HashMap Vertex CopyInSet

type CopyPropBaggage = (GenKillLookup, CopyInLookup)

-- applies copy propegation to a list of node graphs, can be turned off with a flag
doCopyProp :: [NodeGraph] -> Bool -> [NodeGraph]
doCopyProp graphs False = graphs
doCopyProp graphs True = L.map copyPropOptimize graphs

-- applies the copy propegation optimization to the given node graph
copyPropOptimize :: NodeGraph -> NodeGraph
copyPropOptimize nodeGraph@(graph, vertToNodeHM) = (graph, optimizedNodes)
  where
    optimizedNodes = HM.fromList [(vert, applyCopyProp vert nodeGraph baggage) | vert <- getVertices nodeGraph]
    genKillHM = createGenKillLookup nodeGraph
    copyInHM = createCopyInLookup nodeGraph genKillHM
    baggage = (genKillHM, copyInHM)

-- applies copy propegation to a given node
-- requires gen, kill, and copyIn sets
applyCopyProp :: Vertex -> NodeGraph -> CopyPropBaggage -> Node
applyCopyProp vert (_, vertToNodeHM) (genKillHM, copyInHM) = Node label optimizedIloc -- TODO
  where
    node@(Node label iloc) = vertToNodeHM ! vert
    optimizedIloc = doIt iloc (snd (genKillHM ! vert)) (copyInHM ! vert)
    doIt :: [Iloc] -> KillSet -> CopyInSet -> [Iloc]
    doIt [] _ _ = []
    doIt (insn@(Mov r1 r2):rest) kill copyIn = optimizedInsn : (doIt rest newKill newNewCopyIn)
      where
        newKill = kill `Set.union` (Set.fromList (getDstIlocRegs insn))
        newCopyIn = copyIn `copiesNotKilledBy` newKill
        optimizedInsn = doReplacements insn newCopyIn
        newNewCopyIn = Set.insert (r1, r2) newCopyIn
    doIt (insn:rest) kill copyIn = optimizedInsn : (doIt rest newKill newCopyIn)
      where
        newKill = kill `Set.union` (Set.fromList (getDstIlocRegs insn))
        newCopyIn = copyIn `copiesNotKilledBy` newKill
        optimizedInsn = doReplacements insn newCopyIn

-- finds the copyIn set for each node
-- requires gen/kill sets
createCopyInLookup :: NodeGraph -> GenKillLookup -> CopyInLookup
createCopyInLookup nodeGraph genKillHM = doIt (getVertices nodeGraph) HM.empty
  where
    doIt :: [Vertex] -> CopyInLookup -> CopyInLookup
    doIt verts copyInHM
      | nextCopyInHM == copyInHM = copyInHM
      | otherwise = doIt verts nextCopyInHM
      where
        nextCopyInHM = HM.fromList [(v, createCopyIn v nodeGraph (genKillHM, copyInHM)) | v <- verts]

-- finds the copyIn set of a given node relative to its predecessors
createCopyIn :: Vertex -> NodeGraph -> CopyPropBaggage -> CopyInSet
createCopyIn vert nodeGraph (genKillHM, copyInHM) = Set.foldl' Set.difference Set.empty predStuff
  where
    preds = getPredecessors nodeGraph vert
    predStuff = Set.map findPredStuff preds
    findPredStuff v = gen `Set.union` (copyIn `copiesNotKilledBy` kill)
      where
        (gen, kill) = genKillHM ! v
        copyIn = copyInHM ! v

-- finds the gen/kill sets for each node
createGenKillLookup :: NodeGraph -> GenKillLookup
createGenKillLookup = HM.map createGenKill . snd

-- finds the gen/kill set of a given node
createGenKill :: Node -> (GenSet, KillSet)
createGenKill (Node _ insns) = doIt (L.reverse insns) Set.empty Set.empty
  where
    doIt [] gen kill = (gen, kill)
    doIt (insn@(Mov r1 r2):rest) gen kill
      | (r1 `Set.notMember` kill) && (r2 `Set.notMember` kill) = doIt rest (Set.insert (r1, r2) gen) kill
      | otherwise = doIt rest gen (kill `Set.union` (Set.fromList (getDstIlocRegs insn)))
    doIt (insn:rest) gen kill = doIt rest gen (kill `Set.union` (Set.fromList (getDstIlocRegs insn)))

-- filter copyIn set by kill set
copiesNotKilledBy :: CopyInSet -> KillSet -> CopyInSet
copiesNotKilledBy copyIn kill = Set.filter (\(src, dst) -> (src `Set.notMember` kill) && (dst `Set.notMember` kill)) copyIn

doReplacements :: Iloc -> CopySet -> Iloc
doReplacements iloc copies = doIt iloc (Set.toList copies)
  where
    doIt :: Iloc -> [(Reg, Reg)] -> Iloc
    doIt iloc [] = iloc
    doIt insn ((src, dst):rest) = doIt newInsn rest
      where
        newInsn = insn `mapToRegs` (\r -> if r == dst then src else r)

-- find the verticesr in a NodeGraph
-- TODO: this func is sketchy
getVertices :: NodeGraph -> [Vertex]
getVertices (_, vertToNodeHM) = keys vertToNodeHM
