module Mini.CFG 
    ( showNodeGraph
    , NodeGraph
    , createGraphs
    , entryVertex
    , exitVertex
    , getIloc
    , getLabel
    ) where

{-
- Functions for converting a function/list of statements into a Control
- Flow Graph made up of iloc instructions. Assumptions made in this module:
-
- Yes NodeGraph means that graph is guaranteed to hit an explicit return
- statement
-
- No NodeGraph means that at least one path taken in graph will result in
- no explicit return statement
-
- The '0' Vertex is the entry node to a function, and will point to the
- first node that will be run
-
- The '-1' Vertex is the exit node to a function, and anything pointing to
- it will return from the function
-
- Any True/False forks in graph will follow convention where true branch is
- the lower-numbered vertex of the two children. False branch will be the
- higher-numbered child.
-
- A node is a list of iloc instructions or statements in reverse order
-}

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.Monad
import Data.Array hiding ((!))
import Data.Either
import Data.Graph hiding (Node)
import Data.List (foldl')
import Data.Maybe
import Data.HashMap.Strict hiding (filter, null, foldl, foldr, foldl')
import Mini.Iloc.Types
import Mini.Iloc.Expr
import Mini.Iloc.Stmt
import Mini.Types
import Mini.TypeCheck

data Node = Node { getLabel :: Label
                 , getIloc :: [Iloc]
                 }

instance Show Node where
  show (Node label iloc) = 
    unlines strs
    where
      labelStr = label ++ ":"
      strs = labelStr : fmap (\x -> "\t" ++ show x) iloc

-- Entry point will be Vertex 0
-- Exit point will be Vertex -1
type NodeGraph = (Graph, HashMap Vertex Node)
type ReturnBlock = YesNo NodeGraph
type LabelNum = Int
type LabelReg = (LabelNum, Reg)
type NumAndGraph = (LabelReg, ReturnBlock)

showNodeGraph :: NodeGraph -> String
showNodeGraph (graph, vertToNodeHM) =
  concat strs
  where
    sortedVerts = topSort graph
    strs = fmap (\x -> if x `notElem` [0,-1]
                        then show (vertToNodeHM ! x)
                        else []) sortedVerts

label :: LabelReg -> LabelNum
label = fst

reg :: LabelReg -> Reg
reg = snd

initVertex :: Vertex
initVertex = 1

exitVertex :: Vertex
exitVertex = -1

entryVertex :: Vertex
entryVertex = 0

createLabel :: LabelNum -> Label
createLabel num = "L" ++ show num

emptyNode :: Label -> Node
emptyNode name = Node name []

addToNode :: Node -> [Iloc] -> Node
addToNode (Node name iloc) insns = Node name (iloc ++ insns)

defaultBounds :: Bounds
defaultBounds = (exitVertex, initVertex)

createGraphs :: GlobalEnv -> Program -> [NodeGraph]
createGraphs global = snd . foldl' foldFun (1,[]) . getFunctions
    where foldFun (nextLabel, ngs) fun = 
            ngs `app` functionToGraph fun nextLabel global
          app xs (label, x) = (label, x:xs)

replaceRets :: NodeGraph -> Function -> NodeGraph
replaceRets (g, hash) fun = (g, insert exitVertex retNode newHash)
        where retLabel = getFunId fun ++ "_ret"
              newHash = fromList (mapFun <$> toList hash)
              mapFun (k,v) = (k, if k `elem` retNodes
                                  then transformNode v
                                  else v)
              transformNode (Node label iloc) = Node label $ init iloc ++ [jmpIloc]
              jmpIloc = Jumpi retLabel
              retNode = Node retLabel [RetILOC]
              retNodes = fmap fst $ filter ((==exitVertex) . snd) $ edges g

addRet :: NodeGraph -> NodeGraph
addRet (graph, hash) =  if functionReturns
                           then (graph, hash) 
                           else (graph, adjust adjustFun endVert hash) 
                                    `addEdge` (endVert, exitVertex)
    where adjustFun (Node label iloc) = Node label $ iloc ++ [RetILOC]
          endVert = snd $ bounds graph
          functionReturns = last (getIloc $ hash ! endVert) == RetILOC

functionToGraph :: Function -> LabelNum -> GlobalEnv -> (LabelNum, NodeGraph)
functionToGraph func nextLabel global = (label *** addRet . fromYesNo) numGraph
    where argNode = emptyNode (getFunId func) `addToNode` argIloc
          (nextNum, regHash, locals) = 
            foldl' localFoldFun argsHashes $ getFunDeclarations func
          localFoldFun (reg,rHash,lHash) (Declaration _ dType dId) =
              (reg+1, insert dId reg rHash, insert dId dType lHash)
          (argsHashes, argIloc) = foldl' argFoldFun ((1, empty, empty), []) $  
            getFunParameters func
          argFoldFun ((reg,rHash,lHash),iloc) (Field _ fType fId) =
              ((reg+1, insert fId reg rHash, insert fId fType lHash), 
                iloc ++ [loadArg reg])
          loadArg reg = Loadinargument (getFunId func) (reg - 1) reg
          numGraph = stmtsToGraph argNode (getFunBody func) 
            (nextLabel, nextNum) (global, locals, regHash)

-- Append 2nd graph to first one, 
-- creating edge from vertices arguments to Graph 2's Vertex 0's child
-- and transposing all of Graph 2's vertices by Graph 1's upper bound
-- If Graph 2 doesn't have a '0 child' then two graphs will not have an
-- edge connecting them
appendGraph :: NodeGraph -> [Vertex] -> NodeGraph -> NodeGraph
appendGraph (graph1, map1) vertices (graph2, map2) = 
        (buildG (exitVertex, newUpperB) newEdges, map1 `union` newMap2)
    where g1Upper = snd $ bounds graph1
          newUpperB = g1Upper + snd (bounds graph2)
          newEdges2 = fmap mapFun (edges graph2)
          replacedEdges = concat 
            (replaceFun <$> filter((==entryVertex) . fst) newEdges2)
          newEdges = edges graph1 ++ 
            filter ((/=entryVertex) . fst) newEdges2 ++ replacedEdges
          newMap2 = fromList 
            ((\(k, v)->(k+g1Upper,v)) <$> toList map2)
          mapFun = moveVertex *** moveVertex 
          replaceFun (_, v) = (\x -> (x,v)) <$> vertices
          moveVertex v = if v `notElem` [exitVertex, entryVertex]
                             then v + g1Upper
                             else v

-- RegHash will be important once we translate statements to iloc
-- Need a better function name
-- Yes means we will return in this graph
-- No means we may not return in this graph
stmtsToGraph :: Node -> [Statement] -> LabelReg -> Baggage -> NumAndGraph -- (NodeGraph, RegHash)
stmtsToGraph node [] nexts _ = (nexts, No $ fromNode node)
stmtsToGraph node (stmt:rest) nexts baggage = 
        case stmt of
            Block body -> stmtsToGraph node (body ++ rest) nexts baggage
            Cond{} -> createGraph createCondGraph
            Loop{} -> createGraph createLoopGraph
            Ret _ expr -> (nexts, Yes pointToExit)
            _ -> stmtsToGraph newNode rest (label nexts, newReg) baggage
    where successorGraph = 
            stmtsToGraph startNode rest (nextLabel + 1, reg nexts) baggage
          nextLabel = label nexts
          startNode = emptyNode $ createLabel nextLabel
          pointToExit = 
            (buildG defaultBounds 
                [(entryVertex,initVertex), (initVertex,exitVertex)],
                singleton initVertex newNode)
          (newIloc, newReg) = stmtToIloc stmt baggage $ reg nexts
          newNode = node `addToNode` newIloc
          createGraph f = f stmt node successorGraph baggage

-- Yes means we will return in this graph
-- No means we may not return in this graph
createCondGraph :: Statement -> Node -> NumAndGraph -> Baggage -> NumAndGraph
createCondGraph (Cond _ guard thenBlock maybeElseBlock) node (nexts, nextG) baggage = 
        linkGraphs ifThenGraph elseNumGraph nextG thenNexts
    where newNode = node `addToNode` guardInsns `addToNode` [brInsn]
          brInsn = Brz newReg falseLabel (getGraphLabel thenGraph) 
          falseLabel = getGraphLabel (maybe nextG snd elseNumGraph)
          (guardInsns, newReg) = evalExpr guard baggage $ reg nexts
          nextLabel = label nexts
          (thenNexts, thenGraph) = graphFromBlock thenBlock (nextLabel, newReg + 1)
          elseNumGraph = graphFromBlock <$> maybeElseBlock <*> pure thenNexts
          ifThenGraph = appendIf <$> thenGraph
          appendIf = appendGraph (fromNode newNode) [initVertex]
          graphFromBlock block next = 
            addJump (newGraph block next) (getGraphLabel nextG)
          newGraph block nextStuff =
            stmtsToGraph (emptyNode $ createLabel $ label nextStuff) 
                (getBlockStmts block) (label nextStuff + 1, reg nextStuff) baggage 

linkGraphs :: ReturnBlock -> Maybe NumAndGraph -> ReturnBlock -> LabelReg -> NumAndGraph
linkGraphs ifThenGraph Nothing nextGraph nexts =
        (nexts, appendGraph <$> ifThenGraph <*> pure (initVertex:secVert) <*> nextGraph)
    where secVert = yesNo (const []) (\g -> [graphEnd $ pure g]) ifThenGraph 
linkGraphs ifThenGraph (Just (elseNexts, elseGraph)) nextGraph _ =
        (elseNexts, yesNo Yes (\g -> appendGraph g ifVertices <$> nextGraph) ifGraph)
    where ifGraph = appendGraph <$> ifThenGraph <*> pure [initVertex] <*> elseGraph
          thenVertex = [graphEnd ifThenGraph | isNo ifThenGraph]
          elseVertex = [graphEnd ifGraph | isNo elseGraph]
          ifVertices = elseVertex ++ thenVertex

createLoopGraph :: Statement -> Node -> NumAndGraph -> Baggage -> NumAndGraph
createLoopGraph (Loop _ guard body) node (nexts, nextGraph) baggage = 
        ((label bodyNext, guard2Reg + 1), if isNo trueGraph
            then appendGraph <$> cyclicTG <*> pure [initVertex, graphEnd cyclicTG] <*> nextGraph
            else appendGraph <$> trueGraph <*> pure [initVertex] <*> nextGraph)
    where newNode = node `addToNode` guardInsns `addToNode` [brInsn] 
          (guardInsns, newReg) = evalExpr guard baggage $ reg nexts
          brInsn = Brz newReg contLabel bodyLabel
          bodyLabel = getGraphLabel unGuarded
          contLabel = getGraphLabel nextGraph
          nextLabel = label nexts
          startGraph = fromNode newNode
          startNode = emptyNode $ createLabel nextLabel
          -- Update bodyGraph's endNode to include guard
          (bodyNext, unGuarded) = 
            stmtsToGraph startNode (getBlockStmts body) (nextLabel + 1, newReg + 1) baggage
          (guard2Insns, guard2Reg) = evalExpr guard baggage $ reg bodyNext
          bodyGraph = 
            addGuard <$> unGuarded <*> pure guard2Insns <*> 
                pure guard2Reg <*> pure bodyLabel <*> pure contLabel
          trueGraph = appendGraph startGraph [initVertex] <$> bodyGraph
          trueChild = getChild initVertex trueGraph 
          cyclicTG = addEdge <$> trueGraph <*> pure (graphEnd trueGraph, trueChild)

addGuard :: NodeGraph -> [Iloc] -> Reg -> Label -> Label -> NodeGraph
addGuard (graph, hash) newIloc reg trueLabel falseLabel = 
        (graph, adjust adjustFun endVertex hash) 
    where endVertex = graphEnd $ pure (graph, hash)
          brIloc = Brz reg falseLabel trueLabel
          adjustFun (Node label iloc) = Node label $ iloc ++ newIloc ++ [brIloc]

getGraphLabel :: ReturnBlock -> Label
getGraphLabel graph = getLabel $ snd (fromYesNo graph) ! getChild entryVertex graph

getChild:: Vertex -> ReturnBlock -> Vertex
getChild v = snd . head . filter ((==v) . fst) . edges . fst . fromYesNo

addJump :: NumAndGraph -> Label -> NumAndGraph
addJump ret@(_, Yes _) _ = ret
addJump (lr, No (graph, hash)) label = (lr, No (graph, adjusted))
    where adjusted = adjust adjustFun endVertex hash
          endVertex = graphEnd $ pure (graph, hash)
          brIloc = Jumpi label
          adjustFun (Node label iloc) = Node label $ iloc ++ [brIloc]

graphEnd :: ReturnBlock -> Vertex
graphEnd g = snd $ bounds $ fst $ fromYesNo g

fromNode :: Node -> NodeGraph
fromNode node = (buildG defaultBounds [(entryVertex,initVertex)], 
                    singleton initVertex node)

addEdge :: NodeGraph -> Edge -> NodeGraph
addEdge (graph, hash) edge = (buildG (bounds graph) (edge:edges graph), hash)

