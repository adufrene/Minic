module Mini.CFG ( createGraphs ) where

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
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Maybe
import Data.HashMap.Strict hiding (filter, null, foldl, foldr, foldl')

import Mini.Graph
import Mini.Iloc.Types
import Mini.Iloc.Expr
import Mini.Iloc.Stmt
import Mini.Types
import Mini.TypeCheck

import Debug.Trace

type ReturnBlock = YesNo IlocGraph
type LabelNum = Int
type LabelReg = (LabelNum, Reg)
type NumAndGraph = (LabelReg, ReturnBlock)

label :: LabelReg -> LabelNum
label = fst

reg :: LabelReg -> Reg
reg = snd

createLabel :: LabelNum -> Label
createLabel num = "L" ++ show num

createGraphs :: GlobalEnv -> Program -> [(Reg, IlocGraph)]
createGraphs global = snd . L.foldr foldFun (1,[]) . getFunctions
    where foldFun fun (nextLabel, gs) = (fst . fst) &&& ((:gs) . first snd)
                    $ functionToGraph fun nextLabel global

replaceRets :: Function -> IlocGraph -> IlocGraph
replaceRets fun (g, hash) = (g, insert exitVertex retNode newHash)
        where retLabel = getFunId fun ++ "_ret"
              newHash = fromList (mapFun <$> toList hash)
              mapFun (k,v) = (k, if k `elem` retNodes
                                  then transformNode v
                                  else v)
              transformNode (Node label iloc) = Node label $ init iloc ++ [jmpIloc]
              jmpIloc = Jumpi retLabel
              retNode = Node retLabel [RetILOC]
              retNodes = fmap fst $ filter ((==exitVertex) . snd) $ edges g

addRet :: IlocGraph -> IlocGraph
addRet (graph, hash) =  if functionReturns
                           then (graph, hash) 
                           else (graph, adjust adjustFun endVert hash) 
                                    `addEdge` (endVert, exitVertex)
    where adjustFun (Node label iloc) = Node label $ iloc ++ [RetILOC]
          endVert = snd $ bounds graph
          functionReturns = last (getData $ hash ! endVert) == RetILOC

functionToGraph :: Function -> LabelNum -> GlobalEnv -> (LabelReg, IlocGraph)
functionToGraph func nextLabel global = second (replaceRets func) stripContext
    where stripContext = second fromYesNo numGraph
          argNode = emptyNode (getFunId func) `addToNode` argIloc
          (nextNum, regHash, locals) = 
            L.foldl' localFoldFun argsHashes $ getFunDeclarations func
          localFoldFun (reg,rHash,lHash) (Declaration _ dType dId) =
              (reg+1, insert dId reg rHash, insert dId dType lHash)
          (argsHashes, argIloc) = L.foldl' argFoldFun ((1, empty, empty), []) $  
            getFunParameters func
          argFoldFun ((reg,rHash,lHash),iloc) (Field _ fType fId) =
              ((reg+1, insert fId reg rHash, insert fId fType lHash), 
                iloc ++ [loadArg reg])
          loadArg reg = Loadinargument (getFunId func) (reg - 1) reg
          numGraph = stmtsToGraph argNode (getFunBody func) 
            (nextLabel, nextNum) (global, locals, regHash)

-- RegHash will be important once we translate statements to iloc
-- Need a better function name
-- Yes means we will return in this graph
-- No means we may not return in this graph
stmtsToGraph :: IlocNode -> [Statement] -> LabelReg -> Baggage -> NumAndGraph -- (NodeGraph, RegHash)
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
createCondGraph :: Statement -> IlocNode -> NumAndGraph -> Baggage -> NumAndGraph
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
linkGraphs ifThenGraph Nothing nextGraph nexts = {-trace ("linking:\n" ++ show ifThenGraph ++ "and\n" ++ show nextGraph) -}(nexts, appendGraph <$> ifThenGraph <*> pure (initVertex:secVert) <*> nextGraph)
    where secVert = yesNo (const []) (\g -> [graphEnd $ pure g]) ifThenGraph 
linkGraphs ifThenGraph (Just (elseNexts, elseGraph)) nextGraph _ =
        (elseNexts, yesNo Yes (\g -> appendGraph g ifVertices <$> nextGraph) ifGraph)
    where ifGraph = appendGraph <$> ifThenGraph <*> pure [initVertex] <*> elseGraph
          thenVertex = [graphEnd ifThenGraph | isNo ifThenGraph]
          elseVertex = [graphEnd ifGraph | isNo elseGraph]
          ifVertices = elseVertex ++ thenVertex

createLoopGraph :: Statement -> IlocNode -> NumAndGraph -> Baggage -> NumAndGraph
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

addGuard :: IlocGraph -> [Iloc] -> Reg -> Label -> Label -> IlocGraph
addGuard (graph, hash) newIloc reg trueLabel falseLabel = 
        (graph, adjust adjustFun endVertex hash) 
    where endVertex = graphEnd $ pure (graph, hash)
          brIloc = Brz reg falseLabel trueLabel
          adjustFun (Node label iloc) = Node label $ iloc ++ newIloc ++ [brIloc]

getGraphLabel :: ReturnBlock -> Label
getGraphLabel graph = getLabel $ snd (fromYesNo graph) ! getChild entryVertex graph

getChild :: Vertex -> ReturnBlock -> Vertex
getChild v = snd . head . filter ((==v) . fst) . edges . fst . fromYesNo

addJump :: NumAndGraph -> Label -> NumAndGraph
addJump ret@(_, Yes _) _ = ret
addJump ret@(lr, No (graph, hash)) label = {-trace ("Adding jump to " ++ show ret) $ -}(lr, No (graph, adjusted))
    where adjusted = adjust adjustFun endVertex hash
          endVertex = graphEnd $ pure (graph, hash)
          brIloc = Jumpi label
          adjustFun (Node label iloc) = Node label $ iloc ++ [brIloc]

graphEnd :: ReturnBlock -> Vertex
graphEnd g = snd $ bounds $ fst $ fromYesNo g


