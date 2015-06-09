module Mini.Graph where

import Control.Applicative
import Control.Arrow

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

data Node a = Node { getLabel :: String
                   , getData :: [a] }

instance (Show a) => Show (Node a) where
        show (Node label a) = unlines strs
            where labelStr = label ++ ":"
                  strs = labelStr : fmap ((++) "\t" . show) a

-- Entry point will be Vertex 0
-- Exit point will be Vertex -1
type NodeGraph a = Graph (Node a)

showGraph :: Show a => Graph a -> String
showGraph graph = L.concatMap mapFun $ topSort graph
    where mapFun x = if x /= entryVertex
                         then show $ graph `at` x
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

-- Append 2nd graph to first one, 
-- creating edge from vertices arguments to Graph 2's Vertex 0's child
-- and transposing all of Graph 2's vertices by Graph 1's upper bound
-- If Graph 2 doesn't have a '0 child' then two graphs will not have an
-- edge connecting them
appendGraph :: NodeGraph a -> [Vertex] -> NodeGraph a -> NodeGraph a
appendGraph graph1 verts graph2 = L.foldl' linkGraphs newGraph verts
    where newGraph = HM.foldlWithKey' foldFun graph1 graph2
          transpose = (+) (maximum $ vertices graph1)
          g2Children = fmap transpose $ graph2 `getSuccessors` entryVertex
          linkGraphs graph v = L.foldl' (\g -> curry (addEdge g) v) graph
                g2Children
          foldFun graph v (node, succ) =
                if v `elem` [entryVertex, exitVertex]
                    then graph
                    else graph `insertNode` (transpose v, node) `addEdges` L.map (\x -> (transpose v, transpose x)) succ

addEdges :: Graph a -> [Edge] -> Graph a
addEdges = L.foldl' addEdge

addEdge :: Graph a -> Edge -> Graph a
addEdge graph (from, to) = HM.adjust (second (to:)) from graph

-- get all the neighbors of a given vertex in a graph
-- two vertices are neighbors if they share an edge
getNeighbors :: Graph a -> Vertex -> [Vertex]
getNeighbors graph = combine . (getSuccessors graph &&& getPredecessors graph)
    where combine (x, y) = x ++ y

getSuccessors :: Graph a -> Vertex -> [Vertex]
getSuccessors g v = if v `HM.member` g
                        then snd $ g HM.! v
                        else [] --error $ "getSuccessors: Vertex: " ++ show v  ++ " not found in graph"

getPredecessors :: Graph a -> Vertex -> [Vertex]
getPredecessors g v = HM.foldlWithKey' foldFun [] g
    where foldFun vs v' (_, to) = if v `elem` to then v':vs else vs

-- determines if a graph is empty
-- empty graph has no edges
emptyGraph :: Graph a -> Bool
emptyGraph graph = L.null $ vertices graph

vertices :: Graph a -> [Vertex]
vertices = HM.keys

edges :: Graph a -> [Edge]
edges = HM.foldlWithKey' foldFun []
    where foldFun edgs from (_, to) = L.map (\x -> (from, x)) to ++ edgs

{-
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
-}

removeVertex :: Graph a -> Vertex -> Graph a
removeVertex graph vert = HM.map mapFun $ HM.delete vert graph
    where mapFun = second (filter (/=vert))

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph a = HM.HashMap Vertex (a, [Vertex])

mkEmpty :: Graph a
mkEmpty = HM.empty

-- singleton :: Label -> Node a -> Graph a
-- singleton = HM.singleton

insertNode :: Graph a -> (Vertex, a) -> Graph a
insertNode g (k, v) = HM.insert k (v, []) g

insertIfMissing :: Graph a -> (Vertex, a) -> Graph a
insertIfMissing g node@(k, _) = 
        if k `HM.member` g
             then g
             else g `insertNode` node

{-
addEdge :: Graph a -> Edge -> Graph a
addEdge g (from, to) = HM.adjust (second (to:)) from g

addEdgeError :: Graph a -> Edge -> Graph a
addEdgeError g edge@(from, _)
    | from `HM.member` g = addEdge g edge
    | otherwise = error $ "Label " ++ show from ++ " does not exist in graph"

removeEdge :: Graph a -> Edge -> Graph a
removeEdge g (from, to) = HM.adjust (second $ filter (/= to)) from g

removeNode :: Graph a -> Label -> Graph a
removeNode = flip HM.delete

removeNodeAndEdges :: Graph a -> Label -> Graph a
removeNodeAndEdges g l = HM.map (second $ filter (/= l)) newG
    where newG = removeNode g l

getSuccessors :: Graph a -> Label -> [Label]
getSuccessors g = snd . (HM.!) g

getPredecessors :: Graph a -> Label -> [Label]
getPredecessors g l = HM.foldlWithKey' foldFun [] g
    where foldFun preds k (_, out) = if l `elem` out
                                         then k:preds
                                         else preds

getEdges :: Graph a -> [(Label, Label)]
getEdges = HM.foldlWithKey' foldFun []
    where foldFun edges k (_, out) = L.map (\x -> (k, x)) out ++ edges

getNodes :: Graph a -> [(Label, a)]
getNodes = HM.foldlWithKey' (\ns k n -> (k, fst n):ns) []

getLabels :: Graph a -> [Label]
getLabels = HM.keys

-}

mapGraph :: (a -> b) -> Graph a -> Graph b
mapGraph mapFun = HM.map $ first mapFun

mapGraphWithKey :: (Vertex -> a -> b) -> Graph a -> Graph b
mapGraphWithKey mapFun = HM.mapWithKey (\k v -> first (mapFun k) v)

foldlGraph' :: (a -> b -> a) -> a -> Graph b -> a
foldlGraph' foldFun = HM.foldl' (\acc v -> foldFun acc $ fst v)

foldlGraphWithKey' :: (a -> Vertex -> b -> a) -> a -> Graph b -> a
foldlGraphWithKey' foldFun = HM.foldlWithKey' (\acc k v -> foldFun acc k $ fst v)

makeTup :: a -> [b] -> c -> (a, (c, [b]))
makeTup v succ x = (v, (x, succ))

mapGraphAccumL :: (a -> b -> (a, c)) -> a -> Graph b -> (a, Graph c)
mapGraphAccumL mapFun = mapGraphAccum accumFunc
        where accumFunc acc (v, (x, succ)) = second (makeTup v succ)
                    $ mapFun acc x

mapGraphKeyAccumL :: (a -> Vertex -> b -> (a, c)) -> a -> Graph b -> (a, Graph c)
mapGraphKeyAccumL mapFun = mapGraphAccum accumFunc
        where accumFunc acc (v, (x, succ)) = second (makeTup v succ)
                    $ mapFun acc v x

mapGraphAccum :: (a -> (Vertex, (b, [Vertex])) -> (a, (Vertex, (c, [Vertex])))) 
        -> a -> Graph b -> (a, Graph c)
mapGraphAccum accumFunc start graph = second HM.fromList accumedList
    where graphList = HM.toList graph
          accumedList = L.mapAccumL accumFunc start graphList 

getLookup :: Graph a -> HM.HashMap Vertex a
getLookup = HM.map fst

{-

mapWithLabel :: (Label -> Node a -> Node b) -> Graph a -> Graph b
mapWithLabel = HM.mapWithKey

-}

adjustGraph :: (a -> a) -> Vertex -> Graph a -> Graph a
adjustGraph adjustFun = HM.adjust $ first adjustFun

at :: Graph a -> Vertex -> a
at g v = if v `HM.member` g
             then fst $ g HM.! v
             else error $ "at: Vertex: " ++ show v ++ " not found in graph"

{-
-
adjustLabel :: Graph a -> Label -> Label -> Graph a
adjustLabel g old new = HM.insert new node $ HM.delete old g
    where node = g HM.! old
    -}

topSort :: Graph a -> [Vertex]
topSort = vertices

-- member :: Vertex -> Graph a -> Bool
-- member = HM.member
