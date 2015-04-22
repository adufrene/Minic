module Mini.CFG where

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
import Mini.Iloc
import Mini.Types
import Mini.TypeCheck
import Data.List (elemIndex)

data YesNo a = Yes a | No a deriving (Show)

instance Functor YesNo where
        fmap f (Yes a) = Yes $ f a
        fmap f (No a) = No $ f a

instance Monad YesNo where
        return = Yes
        (Yes x) >>= f = f x
        (No x) >>= f = No $ fromYesNo $ f x

instance Applicative YesNo where
        pure = return
        (<*>) = ap

-- Node will keep statements/iloc in reverse order
-- i.e. elements will be prepended to given node
-- MUCH more efficient
data Node = Node { getLabel :: Label
                 , getIloc :: [Iloc]
                 } deriving (Show)

-- Entry point will be Vertex 0
-- Exit point will be Vertex -1
type NodeGraph = (Graph, HashMap Vertex Node)
type RegHash = HashMap Id Reg
type ReturnBlock = YesNo NodeGraph
type LabelNum = Int
type ExprIloc = ([Iloc], Reg)
type Baggage = (GlobalEnv, LocalEnv, RegHash)
type LabelReg = (LabelNum, Reg)
type NumAndGraph = (LabelReg, ReturnBlock)

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
addToNode (Node name iloc) insns = Node name (insns ++ iloc)

defaultBounds :: Bounds
defaultBounds = (exitVertex, initVertex)

createGraphs :: GlobalEnv -> Program -> [NodeGraph]
createGraphs global = snd . foldl' foldFun (1,[]) . getFunctions
    where foldFun (nextLabel, ngs) fun = 
            ngs `app` functionToGraph fun nextLabel global
          app xs (label, x) = (label, x:xs)

functionToGraph :: Function -> LabelNum -> GlobalEnv -> (LabelNum, NodeGraph)
functionToGraph func nextLabel global = 
        (label $ fst numGraph, fromYesNo $ snd numGraph)
    where argNode = emptyNode $ getFunId func-- Start node by storing args in regs
          (nextNum, regHash, locals) = 
            foldl' localFoldFun argsHashes $ getFunDeclarations func
          localFoldFun (reg,rHash,lHash) (Declaration _ dType dId) =
              (reg+1, insert dId reg rHash, insert dId dType lHash)
-- Assumes registers start over between functions
          argsHashes = foldl' argFoldFun (1, empty, empty) $  
            getFunParameters func
          argFoldFun (reg,rHash,lHash) (Field _ fType fId) =
              (reg+1, insert fId reg rHash, insert fId fType lHash)
          numGraph = stmtsToGraph argNode (getFunBody func) 
            (nextLabel, nextNum) (global, locals, regHash)

{-
converts an expression to ILOC and supplies register where result is

params:
  Expression - expression to evaluate as ILOC
  RegHash - maps local vars to the register they live in
  GlobalEnv - environment for global vars
  LocalEnv - environment for local vars
  Reg - next register to use when we need a new register

returns:
  The evaluated Iloc and the register containing the result of this expression

assumes no registers are used greater than result register
-}
evalExpr :: Expression -> Baggage -> Reg -> ExprIloc
evalExpr expr@BinExp{} = evalBinopExpr expr
evalExpr expr@UExp{} = evalUopExpr expr
evalExpr expr@DotExp{} = evalDotExpr expr
evalExpr expr@InvocExp{} = evalInvocExpr expr
evalExpr expr@IdExp{} = \(_, _, regHash) reg -> evalIdExpr expr regHash reg
evalExpr (IntExp _ val) = \_ nextReg -> ([Movi val nextReg], nextReg)
evalExpr (TrueExp _) = \_ nextReg -> ([Movi 1 nextReg], nextReg)
evalExpr (FalseExp _) = \_ nextReg -> ([Movi 0 nextReg], nextReg)
evalExpr expr@NewExp{} = \(global, local, _) reg -> evalNewExpr expr global local reg
evalExpr expr@NullExp{} = \_ nextReg -> ([Movi 0 nextReg], nextReg)

evalBinopExpr :: Expression -> Baggage ->Reg -> ExprIloc
evalBinopExpr (BinExp _ binop lhs rhs) baggage nextReg =
   (lhsIloc ++ rhsIloc ++ binopExprs, resultReg)
   where
      (lhsIloc, lhsReg) = evalExpr lhs baggage nextReg
      (rhsIloc, rhsReg) = evalExpr rhs baggage (lhsReg + 1)
      resultReg = (rhsReg + 1)
      binopExprs
        | binop == "+" = [Add lhsReg rhsReg resultReg]
        | binop == "-" = [Sub lhsReg rhsReg resultReg]
        | binop == "*" = [Mult lhsReg rhsReg resultReg]
        | binop == "/" = [Div lhsReg rhsReg resultReg]
        | binop == "<" = [ Movi 0 resultReg
                         , Comp lhsReg rhsReg
                         , Movlt 1 resultReg]
        | binop == "<=" = [ Movi 0 resultReg
                          , Comp lhsReg rhsReg
                          , Movle 1 resultReg]
        | binop == ">" = [ Movi 0 resultReg
                         , Comp lhsReg rhsReg
                         , Movgt 1 resultReg]
        | binop == ">=" = [ Movi 0 resultReg
                          , Comp lhsReg rhsReg
                          , Movge 1 resultReg]
        | binop == "==" = [ Movi 0 resultReg
                          , Comp lhsReg rhsReg
                          , Moveq 1 resultReg]
        | binop == "!=" = [ Movi 0 resultReg
                          , Comp lhsReg rhsReg
                          , Movne 1 resultReg]
        | binop == "&&" = [ Movi 1 resultReg
                          , Compi lhsReg 0
                          , Moveq 0 resultReg
                          , Compi rhsReg 0
                          , Moveq 0 resultReg]
        | binop == "||" = [ Movi 0 resultReg
                          , Compi lhsReg 0
                          , Movne 1 resultReg
                          , Compi rhsReg 0
                          , Moveq 1 resultReg]
        | otherwise = error $ "don't know what to do with " ++ binop

evalUopExpr :: Expression -> Baggage -> Reg -> ExprIloc
evalUopExpr (UExp _ op operand) baggage nextReg =
   (operandIloc ++ uopIloc, resultReg)
   where
      (operandIloc, operandReg) = evalExpr operand baggage nextReg
      resultReg = operandReg + 1
      uopIloc
         | op == "-" = [Multi operandReg (-1) resultReg]
         | op == "!" = [ Movi 0 resultReg
                       , Compi operandReg 0
                       , Moveq 1 resultReg ]
         | otherwise = error $ "unexpected uop: " ++ op

evalDotExpr :: Expression -> Baggage -> Reg -> ExprIloc
evalDotExpr (DotExp _ leftExpr dotId) bag@(globals, locals, regHash) nextReg =
  (recurIloc ++ currIloc, resultReg)
  where
    (recurIloc, leftReg) = evalLeft leftExpr
    currIloc = [Loadai leftReg fieldIdx resultReg]
    structType = getExprTypeOrDieTrying leftExpr globals locals
    structFields = (getStructHash globals) ! structType
    fieldIdx = fromJust $ elemIndex dotId $ fmap getFieldId structFields
    resultReg = leftReg + 1

    evalLeft leftExpr@(IdExp _ theId) = evalIdExpr leftExpr regHash nextReg
    evalLeft dotExpr = evalDotExpr dotExpr bag nextReg

getExprTypeOrDieTrying :: Expression -> GlobalEnv -> LocalEnv -> Type
getExprTypeOrDieTrying expr globs locs = extractTypeFromEither $ getExprType expr globs locs
  where
    extractTypeFromEither (Right t) = t
    extractTypeFromEither _ = error $ "died trying to get expression type"

evalInvocExpr :: Expression -> Baggage -> Reg -> ExprIloc
evalInvocExpr (InvocExp _ invocId args) baggage nextReg =
  (argsIloc ++ outArgIloc ++ callIloc, retReg)
  where
    (argsIloc, argsRegs) = evalInvocArgs args [] [] baggage nextReg
    outArgIloc = [Storeoutargument (argsRegs !! idx) idx | idx <- [0..((length argsRegs) - 1)]]
    callIloc = [ Call invocId
               , Loadret retReg ]
    retReg = 1 + (last argsRegs)

evalInvocArgs :: Arguments -> [Iloc] -> [Reg] -> Baggage -> Reg -> ([Iloc], [Reg])
evalInvocArgs (arg:rest) currIloc currRegs baggage nextReg =
  evalInvocArgs rest (currIloc ++ argIloc) (currRegs ++ [argReg]) baggage  (argReg + 1)
  where
    (argIloc, argReg) = evalExpr arg baggage nextReg
evalInvocArgs _ currIloc currRegs _ _ = (currIloc, currRegs)
  

evalIdExpr :: Expression -> RegHash -> Reg -> ExprIloc
evalIdExpr (IdExp _  theId) regHash nextReg
  | isLocal = evalLocalIdExpr
  | otherwise = evalGlobalIdExpr
  where
    isLocal = theId `member` regHash
    evalLocalIdExpr = ([Mov varReg nextReg], nextReg)
      where
        varReg = regHash ! theId
    evalGlobalIdExpr = ([Loadglobal theId nextReg], nextReg)

evalNewExpr :: Expression -> GlobalEnv -> LocalEnv -> Reg -> ExprIloc
evalNewExpr (NewExp _ newId) global local nextReg =
  ([New numWords nextReg], nextReg)  
  where numWords = length $ (getStructHash global) ! newId
--         fromJust $ Data.HashMap.Strict.lookup newId structHash -- FIX --

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
            _ -> stmtsToGraph newNode rest nexts baggage
    where successorGraph = 
            stmtsToGraph startNode rest (nextLabel + 1, nextReg) baggage
          nextLabel = label nexts
          nextReg = reg nexts
          startNode = emptyNode $ createLabel nextLabel
          pointToExit = 
            (buildG defaultBounds 
                [(entryVertex,initVertex), (initVertex,exitVertex)],
                singleton initVertex newNode)
          newNode = node `addToNode` stmtToIloc stmt baggage nextReg
          createGraph f = f stmt node successorGraph baggage

-- Yes means we will return in this graph
-- No means we may not return in this graph
createCondGraph :: Statement -> Node -> NumAndGraph -> Baggage -> NumAndGraph
createCondGraph (Cond _ guard thenBlock maybeElseBlock) node (nexts, nextG) baggage = 
        ((thenLabel,newReg), linkGraphs ifThenGraph elseNumGraph nextG)
    where newNode = node `addToNode` guardInsns --(exprToIloc guard baggage) -- finish node with guard
          (guardInsns, newReg) = evalExpr guard baggage $ reg nexts
          nextLabel = label nexts
          ((thenLabel, thenReg), thenGraph) = graphFromBlock thenBlock (nextLabel, newReg + 1)
          elseNumGraph = graphFromBlock <$> maybeElseBlock <*> pure (thenLabel, thenReg)
          ifThenGraph = appendIf <$> thenGraph
          appendIf = appendGraph (fromNode newNode) [initVertex]
          graphFromBlock block nextStuff = 
            stmtsToGraph (emptyNode $ createLabel $ label nextStuff) 
                (getBlockStmts block) (label nextStuff + 1, reg nextStuff) baggage 

linkGraphs :: ReturnBlock -> Maybe NumAndGraph -> ReturnBlock -> YesNo NodeGraph
linkGraphs ifThenGraph Nothing nextGraph =
        appendGraph <$> ifThenGraph <*> pure (initVertex:secVert) <*> nextGraph
    where secVert = yesNo (const []) (\g -> [graphEnd $ pure g]) ifThenGraph 
linkGraphs ifThenGraph (Just (elseNum, elseGraph)) nextGraph =
        yesNo Yes (\g -> appendGraph g ifVertices <$> nextGraph) ifGraph
    where ifGraph = appendGraph <$> ifThenGraph <*> pure [initVertex] <*> elseGraph
          thenVertex = [graphEnd ifThenGraph | isNo ifThenGraph]
          elseVertex = [graphEnd ifGraph | isNo elseGraph]
          ifVertices = elseVertex ++ thenVertex

graphEnd :: ReturnBlock -> Vertex
graphEnd g = snd $ bounds $ fst $ fromYesNo g

createLoopGraph :: Statement -> Node -> NumAndGraph -> Baggage -> NumAndGraph
createLoopGraph (Loop _ guard body) node (nexts, nextGraph) baggage = 
        (bodyNum, if isNo trueGraph
            then appendGraph <$> cyclicTG <*> pure [initVertex, graphEnd cyclicTG] <*> nextGraph
            else appendGraph <$> trueGraph <*> pure [initVertex] <*> nextGraph)
    where newNode = node `addToNode` [] -- guard iloc
          nextLabel = label nexts
          nextReg = reg nexts
          startGraph = fromNode newNode
          startNode = emptyNode $ createLabel nextLabel
          -- Update bodyGraph's endNode to include guard
          (bodyNum, bodyGraph) = 
            stmtsToGraph startNode (getBlockStmts body) (nextLabel + 1, nextReg) baggage
          trueGraph = appendGraph startGraph [initVertex] <$> bodyGraph
          trueChild = snd $ head $ filter ((==initVertex) . fst) $ 
            edges $ fst $ fromYesNo trueGraph
          cyclicTG = addEdge <$> trueGraph <*> pure (graphEnd trueGraph, trueChild)

fromNode :: Node -> NodeGraph
fromNode node = (buildG defaultBounds [(entryVertex,initVertex)], 
                    singleton initVertex node)

addEdge :: NodeGraph -> Edge -> NodeGraph
addEdge (graph, hash) edge = (buildG (bounds graph) (edge:edges graph), hash)

-- If 3rd arg is yes, run 1st function
-- else run 2nd function
yesNo :: (a -> b) -> (a -> b) -> YesNo a -> b
yesNo f _ (Yes a) = f a
yesNo _ f (No a) = f a

isYes :: YesNo a -> Bool
isYes (Yes _) = True
isYes (No _) = False

isNo :: YesNo a -> Bool
isNo = not . isYes

fromYesNo :: YesNo a -> a
fromYesNo (Yes x) = x
fromYesNo (No x) = x

stmtToIloc :: Statement -> Baggage -> Reg -> [Iloc]
stmtToIloc stmt@Ret{} = retToIloc stmt 
stmtToIloc _ = \_ _ -> []

retToIloc :: Statement -> Baggage -> Reg -> [Iloc]
retToIloc (Ret _ expr) baggage nextReg  = 
        RetILOC : retInsn ++ exprInsns
    where (exprInsns, reg) = 
            maybe ([],-1) (\e -> evalExpr e baggage nextReg) expr
          retInsn = if null exprInsns then [] else [Storeret reg]

lValToIloc :: LValue -> Baggage -> Reg -> ExprIloc
lValToIloc _ _ _ = ([], 0)
-- lValToIloc (LValue _ name Nothing) hash _ = ([], hash ! name)
-- lValToIloc (LValue _ name (Just lval)) nextReg = 
--         recur (hash ! name) lval (nextReg + 1)
--     where recur reg (LValue _ newName (Just newVal)) nextReg = 
--             Loadai reg (getTypeOffset 
--           recur reg (LValue _ newName Nothing) currType nextReg = 

asgnToIloc :: Statement -> Baggage -> Reg -> [Iloc]
asgnToIloc (Asgn _ lval expr) baggage nextReg = undefined
    where (exprInsns, reg) = evalExpr expr baggage nextReg

