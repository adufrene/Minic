module Mini.CFG where

import Data.Array
import Data.Graph
import Data.HashMap.Strict
import Mini.Iloc
import Mini.Types

-- Node will keep statements/iloc in reverse order
-- i.e. elements will be prepended to given node
-- MUCH more efficient
type Node = [Statement] -- [Iloc]

-- Entry point will be Vertex 0?
-- Exit point will be Vertex -1?
type NodeGraph = (Graph, HashMap Vertex Node)
type RegHash = HashMap Id Reg

emptyNode :: Node
emptyNode = []

createCFGS :: Program -> [NodeGraph]
createCFGS = (fmap functionToCFG) . getFunctions

functionToCFG :: Function -> NodeGraph
functionToCFG func = undefined

-- Append 2nd graph to first one, 
-- creating edge from vertices arguments to Graph 2's Vertex 0's child
-- and transposing all of Graph 2's vertices by Graph 1's upper bound
-- If Graph 2 doesn't have a '0 child' then two graphs will not have an
-- edge connecting them
appendGraph :: NodeGraph -> NodeGraph -> [Vertex] -> NodeGraph
appendGraph (graph1, map1) (graph2, map2) vertices = 
        (buildG newBounds newEdges, newHash)
    where g1Upper = snd $ bounds graph1
          newUpperB = g1Upper + (snd $ bounds graph2)
          newBounds = (-1, newUpperB)
          newEdges2 = fmap mapFun (edges graph2)
          replacedEdges = concat $ fmap replaceFun $ 
            Prelude.filter((==0) . fst) newEdges2
          newEdges = edges graph1 ++ 
            Prelude.filter ((/=0) . fst) newEdges2 ++ replacedEdges
          newMap2 = fromList $ fmap (\(k, v)->(k+g1Upper,v)) $ 
            toList map2
          newHash = map1 `union` newMap2
          mapFun = \(v, out) -> (moveVertex v, moveVertex out)
          replaceFun = \(_, v) -> fmap (\x -> (x,v)) vertices
          moveVertex v = if not $ v `elem` [-1, 0]
                             then v + g1Upper
                             else v

-- RegHash will be important once we translate statements to iloc
-- Do we need to return a RegHash?
buildBlock :: Node -> [Statement] -> RegHash -> NodeGraph -- (NodeGraph, RegHash)
buildBlock node (stmt:rest) hash = 
        case stmt of
            Block body -> buildBlock node (body ++ rest) hash
            Cond _ guard thenBlock elseBlock -> undefined
            Loop _ guard body -> undefined
            Ret _ expr -> pointToExit 
            _ -> buildBlock newNode rest hash
    where (successorGraph, newHash) = buildBlock emptyNode rest hash
          pointToExit = (buildG (-1,1) [(0,1), (1,-1)], singleton 1 newNode)
          newNode = stmt:node
