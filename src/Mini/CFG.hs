module Mini.CFG where

import Data.Graph
import Data.HashMap.Strict
import Mini.Iloc
import Mini.Types

type Node = [Statement] -- [Iloc]
type NodeGraph = (Graph, HashMap Id Vertex)
type RegHash = HashMap Id Reg

createCFGS :: Program -> [NodeGraph]
createCFGS = (fmap functionToCFG) . getFunctions

functionToCFG :: Function -> NodeGraph
functionToCFG func = undefined

concatGraphs :: NodeGraph -> NodeGraph -> NodeGraph
concatGraphs g1 g2 = undefined

buildBlock :: Node -> [Statement] -> RegHash -> NodeGraph
buildBlock node stmts hash = undefined
