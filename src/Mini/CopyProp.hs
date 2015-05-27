module Mini.CopyProp
  ( copyPropOptimize
  , doCopyProp
  )
  where

import Mini.Iloc.Types
import Mini.RegAlloc
import Mini.CFG
import Mini.Graph

import Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Data.Graph hiding (Node)
import Data.List as L

import Debug.Trace

type CopySet = Set.Set (Reg, Reg) -- {(src, dst)..}
type CopyInSet = CopySet
type GenSet = CopySet
type KillSet = Set.Set Reg

type GenKillLookup = HashMap Vertex (GenSet, KillSet)
type CopyInLookup = HashMap Vertex CopyInSet

type CopyPropBaggage = (GenKillLookup, CopyInLookup)

-- find the vertices in a NodeGraph
getVertices :: IlocGraph -> [Vertex]
getVertices (_, vertToNodeHM) = keys vertToNodeHM

getNodeGraphPredecessors :: IlocGraph -> Vertex -> Set.Set Vertex
getNodeGraphPredecessors nodeGraph@(graph, _) vertex = (Set.fromList [start | (start, end) <- edges graph, end == vertex, start `elem` (getVertices nodeGraph)])

-- applies copy propegation to a list of node graphs, can be turned off with a flag
doCopyProp :: [IlocGraph] -> Bool -> [IlocGraph]
doCopyProp graphs False =
  trace ("not gonna do the copyProp to a list of iloc graphs")
  graphs
doCopyProp graphs True =
  trace ("gonna do the copyProp to a list of iloc graphs")
  L.map copyPropOptimize graphs

-- applies the copy propegation optimization to the given node graph
copyPropOptimize :: IlocGraph -> IlocGraph
copyPropOptimize nodeGraph@(graph, vertToNodeHM) =
  (graph, optimizedNodes)
  where
    optimizedNodes =
      --trace ("copyInHM " ++ (show copyInHM)) $ trace ("genKillHM " ++ (show genKillHM))
      trace ("nodeVertices: " ++ (show nodeVertices))
      HM.fromList [(vert, applyCopyProp vert nodeGraph baggage) | vert <- nodeVertices]
    nodeVertices = getVertices nodeGraph
    genKillHM = createGenKillLookup nodeGraph
    copyInHM = createCopyInLookup nodeGraph genKillHM
    baggage = (genKillHM, copyInHM)

-- applies copy propegation to a given node
-- requires gen, kill, and copyIn sets
applyCopyProp :: Vertex -> IlocGraph -> CopyPropBaggage -> IlocNode
applyCopyProp vert (_, vertToNodeHM) (genKillHM, copyInHM) =
  --trace ("node: " ++ (show node) ++ ", optimizedIloc: " ++ (show optimizedIloc))
  (Node label optimizedIloc) -- TODO
  --node
  where
    node@(Node label iloc)
      | vert `HM.member` vertToNodeHM = vertToNodeHM ! vert
      | otherwise = error "shit is fucked up"
    optimizedIloc
      | vert `HM.member` copyInHM = doIt iloc (copyInHM ! vert)
      | otherwise = error "shit is fucked up 2"
    doIt :: [Iloc] -> CopySet -> [Iloc]
    doIt [] _ = []
    -- doIt (iloc@(Mov r1 r2):rest) copyNow = nextIloc:(doIt rest nextCopyNow)
    --   where
    --     optimizedIloc@(Mov r1' r2') = doReplacements iloc copyNow
    --     dstRegs = Set.fromList [r2]
    --     filteredCopyNow = copyNow `copiesNotKilledBy` dstRegs
    --     (nextIloc, nextCopyNow) = ((Mov r1' r1'), ((r1', r2') `Set.insert` filteredCopyNow))
    doIt (iloc:rest) copyNow = nextIloc:(doIt rest nextCopyNow)
      where
        optimizedIloc = doReplacements iloc copyNow
        dstRegs = Set.fromList (getDstIlocRegs optimizedIloc)
        filteredCopyNow = copyNow `copiesNotKilledBy` dstRegs
        (nextIloc, nextCopyNow) = (optimizedIloc, filteredCopyNow)
        --addToCopyNow optimizedIloc filteredCopyNow

    -- addToCopyNow (Mov r1 r2) copyNow = ((Mov r1 r1), ((r1, r2) `Set.insert` copyNow))
    -- addToCopyNow insn copyNow = (insn, copyNow)

-- finds the copyIn set for each node
-- requires gen/kill sets
createCopyInLookup :: IlocGraph -> GenKillLookup -> CopyInLookup
createCopyInLookup nodeGraph genKillHM = doIt (getVertices nodeGraph) HM.empty
  where
    doIt :: [Vertex] -> CopyInLookup -> CopyInLookup
    doIt verts copyInHM
      | nextCopyInHM == copyInHM =
        --trace ("verts: " ++ (show verts)) $ trace ("caluclated nextCopyInHM: " ++ (show nextCopyInHM))
        copyInHM
      | otherwise =
        --trace ("verts: " ++ (show verts)) $ trace ("caluclated nextCopyInHM: " ++ (show nextCopyInHM))
        doIt verts nextCopyInHM
      where
        nextCopyInHM = HM.fromList [(v, createCopyIn v nodeGraph (genKillHM, copyInHM)) | v <- verts]

-- finds the copyIn set of a given node relative to its predecessors
createCopyIn :: Vertex -> IlocGraph -> CopyPropBaggage -> CopyInSet
createCopyIn vert nodeGraph@(graph, _) (genKillHM, copyInHM) = 
  --trace ("predStuff: " ++ (show predStuff))
  Set.foldl' Set.intersection first predStuff
  where
    first
      | Set.null predStuff = Set.empty
      | otherwise = L.head $ Set.toList predStuff
    preds = getNodeGraphPredecessors nodeGraph vert
    predStuff = Set.map findPredStuff preds
    findPredStuff v = 
      --trace ("gen: " ++ (show gen) ++ "\n kill: " ++ (show kill) ++ "copyIn: " ++ (show copyIn))
      gen `Set.union` (copyIn `copiesNotKilledBy` kill)
      where
        (gen, kill) = genKillHM ! v
        copyIn
          | v `HM.member` copyInHM = copyInHM ! v
          | otherwise = Set.empty

-- finds the gen/kill sets for each node
createGenKillLookup :: IlocGraph -> GenKillLookup
createGenKillLookup = HM.map createGenKill . snd

-- finds the gen/kill set of a given node
createGenKill :: IlocNode -> (GenSet, KillSet)
createGenKill (Node _ insns) = doIt (L.reverse insns) Set.empty Set.empty
  where
    doIt [] gen kill = (gen, kill)
    doIt (insn@(Mov r1 r2):rest) gen kill
      | (r1 `Set.notMember` kill) && (r2 `Set.notMember` kill) = doIt rest (Set.insert (r1, r2) gen) kill
      | otherwise = doIt rest gen (kill `Set.union` (Set.fromList (getDstIlocRegs insn)))
    doIt (insn:rest) gen kill = doIt rest gen (kill `Set.union` (Set.fromList (getDstIlocRegs insn)))

-- filter copyIn set by kill set
copiesNotKilledBy :: CopySet -> KillSet -> CopySet
copiesNotKilledBy copyIn kill = Set.filter (\(src, dst) -> (src `Set.notMember` kill) && (dst `Set.notMember` kill)) copyIn

-- apply all of our copy replacements to a given iloc
doReplacements :: Iloc -> CopySet -> Iloc
doReplacements iloc copies =
  --trace ("hello from doReplacements. iloc: " ++ (show iloc) ++ (", copies: ") ++ (show copies))
  doIt iloc (Set.toList copies)
  where
    doIt :: Iloc -> [(Reg, Reg)] -> Iloc
    doIt iloc [] = iloc
    doIt insn ((src, dst):rest) = doIt newInsn rest
      where
        newInsn =
          insn `mapToSrcRegs` (\r -> if r == dst then trace ("replacing " ++ (show r) ++ " with " ++ (show src)) src else r)
          --insn `mapToRegs` (\r -> if r == dst then (trace ("replacing " ++ (show dst) ++ " with " ++ (show src) ++ " in " ++ (show insn)) src) else  (trace ("no replacement for: " ++ (show insn)) r))
