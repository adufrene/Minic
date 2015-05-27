module Mini.Graph where

import Control.Arrow

import Data.Array hiding ((!))
import Data.Graph hiding (Node)
import Data.HashMap.Strict
import qualified Data.List as L

data Node a = Node { getLabel :: String
                               , getData :: [a] }

instance (Show a) => Show (Node a) where
        show (Node label a) = unlines strs
            where labelStr = label ++ ":"
                  strs = labelStr : fmap ((++) "\t" . show) a

-- Entry point will be Vertex 0
-- Exit point will be Vertex -1
type NodeGraph a = (Graph, HashMap Vertex (Node a))

showNodeGraph :: Show a => NodeGraph a -> String
showNodeGraph (graph, hash) = L.concatMap mapFun $ topSort graph
    where mapFun x = if x /= entryVertex
                         then show $ hash ! x
                         else []

initVertex :: Vertex
initVertex = 1

exitVertex :: Vertex
exitVertex = -1

entryVertex :: Vertex
entryVertex = 0

emptyNode :: String -> Node a
emptyNode name = Node name []

addToNode :: Node a -> [a] -> Node a
addToNode (Node name old) new = Node name (old ++ new)

mapNode :: ([a] -> [b]) -> Node a -> Node b
mapNode f (Node l a) = Node l (f a)

defaultBounds :: Bounds
defaultBounds = (exitVertex, initVertex)

-- Append 2nd graph to first one, 
-- creating edge from vertices arguments to Graph 2's Vertex 0's child
-- and transposing all of Graph 2's vertices by Graph 1's upper bound
-- If Graph 2 doesn't have a '0 child' then two graphs will not have an
-- edge connecting them
appendGraph :: NodeGraph a -> [Vertex] -> NodeGraph a -> NodeGraph a
appendGraph (graph1, map1) vertices (graph2, map2) = 
        (buildG (exitVertex, newUpperB) newEdges, map1 `union` newMap2)
    where g1Upper = snd $ bounds graph1
          newUpperB = g1Upper + snd (bounds graph2)
          newEdges2 = fmap (moveVertex *** moveVertex) (edges graph2)
          filtered = L.partition ((==entryVertex) . fst) newEdges2
          replacedEdges = concat $ replaceFun <$> fst filtered
          newEdges = edges graph1 ++ snd filtered ++ replacedEdges
          newMap2 = fromList ((\(k, v)->(k+g1Upper,v)) <$> toList map2)
          replaceFun (_, v) = (\x -> (x,v)) <$> vertices
          moveVertex v = if v `notElem` [exitVertex, entryVertex]
                             then v + g1Upper
                             else v

fromNode :: Node a -> NodeGraph a
fromNode node = (buildG defaultBounds [(entryVertex,initVertex)], 
                    singleton initVertex node)

addEdge :: NodeGraph a -> Edge -> NodeGraph a
addEdge (graph, hash) edge = (buildG (bounds graph) (edge:edges graph), hash)

getSuccessors :: Graph -> Vertex -> [Vertex]
getSuccessors graph vert = [end | (start, end) <- edges graph, start == vert]

getPredecessors :: Graph -> Vertex -> [Vertex]
getPredecessors graph vert = [start | (start, end) <- edges graph, end == vert]

-- get all the neighbors of a given vertex in a graph
-- two vertices are neighbors if they share an edge
getNeighbors :: Graph -> Vertex -> [Vertex]
getNeighbors graph = combine . (getSuccessors graph &&& getPredecessors graph)
    where combine (x, y) = x ++ y

-- determines if a graph is empty
-- empty graph has no edges
emptyGraph :: Graph -> Bool
emptyGraph graph = L.null $ vertices graph

-- adds a list of edges to a given graph
addEdges :: Graph -> [Edge] -> Graph
addEdges graph newEdges = buildG newBnds $ edges graph ++ newEdges
  where newBnds = (minimum verts, maximum verts)
        verts = getVerticesFromEdges newEdges ++ vertices graph

-- get all the vertices that are in the supplied list of edges
getVerticesFromEdges :: [Edge] -> [Vertex]
getVerticesFromEdges edges = L.map fst edges ++ L.map snd edges

-- take out all edges that touch a given vertex
removeVertex :: Graph -> Vertex -> Graph
removeVertex graph toKill = buildG newBounds remainingEdges
  where remainingEdges = [(v1, v2) | (v1, v2) <- edges graph, toKill `L.notElem` [v1,v2]]
        (lower, upper) = bounds graph
        newBounds
            | toKill == lower = (lower + 1, upper)
            | toKill == upper = (lower, upper - 1)
            | otherwise = (lower, upper)
