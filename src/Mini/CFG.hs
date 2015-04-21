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
import Data.Graph
import Data.Maybe
import Data.HashMap.Strict hiding (filter)
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
type Node = [Statement] -- [Iloc]

-- Entry point will be Vertex 0
-- Exit point will be Vertex -1
type NodeGraph = (Graph, HashMap Vertex Node)
type RegHash = HashMap Id Reg

emptyNode :: Node
emptyNode = []

initVertex :: Vertex
initVertex = 1

exitVertex :: Vertex
exitVertex = -1

entryVertex :: Vertex
entryVertex = 0

defaultBounds :: Bounds
defaultBounds = (exitVertex, initVertex)

createGraphs :: Program -> [NodeGraph]
createGraphs = fmap functionToGraph . getFunctions

functionToGraph :: Function -> NodeGraph
functionToGraph func = 
        fromYesNo $ stmtsToGraph argNode (getFunBody func) argHash
    where argNode = emptyNode -- Start node by storing args in regs
          argHash = empty -- create HashMap from args

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
evalExpr :: Expression -> RegHash -> StructHash -> GlobalEnv -> LocalEnv -> Reg -> ([Iloc], Reg)
evalExpr expr@BinExp{} = evalBinopExpr expr
evalExpr expr@UExp{} = evalUopExpr expr
evalExpr expr@DotExp{} = evalDotExpr expr
evalExpr expr@InvocExp{} = evalInvocExpr expr
evalExpr expr@IdExp{} = evalIdExpr expr
evalExpr (IntExp _ val) = \_ _ _ _ nextReg -> ([Movi val nextReg], nextReg)
evalExpr (TrueExp _) = \_ _ _ _ nextReg -> ([Movi 1 nextReg], nextReg)
evalExpr (FalseExp _) = \_ _ _ _ nextReg -> ([Movi 0 nextReg], nextReg)
evalExpr expr@NewExp{} = evalNewExpr expr
evalExpr expr@NullExp{} = \_ _ _ _ nextReg -> ([Movi 0 nextReg], nextReg)

evalBinopExpr (BinExp _ binop lhs rhs) regHash structHash globals locals nextReg =
   (lhsIloc ++ rhsIloc ++ binopExprs, resultReg)
   where
      (lhsIloc, lhsReg) = evalExpr lhs regHash structHash globals locals nextReg
      (rhsIloc, rhsReg) = evalExpr rhs regHash structHash globals locals (lhsReg + 1)
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

evalUopExpr (UExp _ op operand) regHash structHash globals locals nextReg =
   (operandIloc ++ uopIloc, resultReg)
   where
      (operandIloc, operandReg) = evalExpr operand regHash structHash globals locals nextReg
      resultReg = operandReg + 1
      uopIloc
         | op == "-" = [Multi operandReg (-1) resultReg]
         | op == "!" = [ Movi 0 resultReg
                       , Compi operandReg 0
                       , Moveq 1 resultReg ]
         | otherwise = error $ "unexpected uop: " ++ op

evalDotExpr (DotExp _ leftExpr dotId) regHash structHash globals locals nextReg =
  (recurIloc ++ currIloc, resultReg)
  where
    (recurIloc, leftReg) = evalLeft leftExpr
    currIloc = [Loadai leftReg fieldIdx resultReg]
    structType = getExprTypeOrDieTrying leftExpr globals locals
    structFields = structHash ! structType
    fieldIdx = fromJust $ elemIndex dotId $ fmap getFieldId structFields
    resultReg = leftReg + 1

    evalLeft leftExpr@(IdExp _ theId) = evalIdExpr leftExpr regHash structHash globals locals nextReg
    evalLeft dotExpr = evalDotExpr dotExpr regHash structHash globals locals nextReg

getExprTypeOrDieTrying :: Expression -> GlobalEnv -> LocalEnv -> Type
getExprTypeOrDieTrying expr globs locs = extractTypeFromEither $ getExprType expr globs locs
  where
    extractTypeFromEither (Right t) = t
    extractTypeFromEither _ = error $ "died trying to get expression type"

evalInvocExpr (InvocExp _ invocId args) regHash structHash globals locals nextReg =
  (argsIloc ++ outArgIloc ++ callIloc, retReg)
  where
    (argsIloc, argsRegs) = evalInvocArgs args [] [] nextReg
    outArgIloc = [Storeoutargument (argsRegs !! idx) idx | idx <- [0..((length argsRegs) - 1)]]
    callIloc = [ Call invocId
               , Loadret retReg ]
    retReg = 1 + (last argsRegs)

    evalInvocArgs :: Arguments -> [Iloc] -> [Reg] -> Int -> ([Iloc], [Reg])
    evalInvocArgs (arg:rest) currIloc currRegs nextReg =
      evalInvocArgs rest (currIloc ++ argIloc) (currRegs ++ [argReg]) (argReg + 1)
      where
        (argIloc, argReg) = evalExpr arg regHash structHash globals locals nextReg
    evalInvocArgs _ currIloc currRegs _ =
      (currIloc, currRegs)

evalIdExpr (IdExp _  theId) regHash _ _ _ nextReg
  | isLocal = evalLocalIdExpr
  | otherwise = evalGlobalIdExpr
  where
    isLocal = theId `member` regHash
    evalLocalIdExpr = ([Mov varReg nextReg], nextReg)
      where
        varReg = regHash ! theId
    evalGlobalIdExpr = ([Loadglobal theId nextReg], nextReg)

evalNewExpr (NewExp _ newId) _ structHash _ _ nextReg =
  ([New numWords nextReg], nextReg)  
  where numWords = length $ fromJust $ Data.HashMap.Strict.lookup newId structHash

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
stmtsToGraph :: Node -> [Statement] -> RegHash -> YesNo NodeGraph -- (NodeGraph, RegHash)
stmtsToGraph node [] hash = No $ fromNode node
stmtsToGraph node (stmt:rest) hash = 
        case stmt of
            Block body -> stmtsToGraph node (body ++ rest) hash
            Cond{} -> createCondGraph stmt node successorGraph hash
            Loop{} -> createLoopGraph stmt node successorGraph hash
            Ret _ expr -> Yes pointToExit
            _ -> stmtsToGraph newNode rest hash
    where successorGraph = stmtsToGraph emptyNode rest hash
          pointToExit = 
            (buildG defaultBounds 
                [(entryVertex,initVertex), (initVertex,exitVertex)],
                singleton initVertex newNode)
          newNode = stmt:node

-- Yes means we will return in this graph
-- No means we may not return in this graph
createCondGraph :: Statement -> Node -> YesNo NodeGraph -> RegHash -> YesNo NodeGraph
createCondGraph stmt@(Cond _ guard thenBlock maybeElseBlock) node nextGraph hash = 
        linkGraphs ifThenGraph elseGraph nextGraph
    where newNode = stmt:node -- finish node with guard
          thenGraph = graphFromBlock thenBlock
          elseGraph = graphFromBlock <$> maybeElseBlock
          ifThenGraph = appendIf <$> graphFromBlock thenBlock
          appendIf = appendGraph (fromNode newNode) [initVertex]
          graphFromBlock block = stmtsToGraph emptyNode (getBlockStmts block) hash

linkGraphs :: YesNo NodeGraph -> Maybe (YesNo NodeGraph) -> YesNo NodeGraph -> YesNo NodeGraph
linkGraphs ifThenGraph Nothing nextGraph =
        appendGraph <$> ifThenGraph <*> pure (initVertex:secVert) <*> nextGraph
    where secVert = runIf (const []) (\g -> [graphEnd $ pure g]) ifThenGraph 
linkGraphs ifThenGraph (Just elseGraph) nextGraph =
        runIf Yes (\g -> appendGraph g ifVertices <$> nextGraph) ifGraph
    where ifGraph = appendGraph <$> ifThenGraph <*> pure [initVertex] <*> elseGraph
          thenVertex = [graphEnd ifThenGraph | isNo ifThenGraph]
          elseVertex = [graphEnd ifGraph | isNo elseGraph]
          ifVertices = elseVertex ++ thenVertex

graphEnd :: YesNo NodeGraph -> Vertex
graphEnd g = snd $ bounds $ fst $ fromYesNo g

createLoopGraph :: Statement -> Node -> YesNo NodeGraph -> RegHash -> YesNo NodeGraph 
createLoopGraph stmt@(Loop _ guard body) node nextGraph hash = 
        if isNo trueGraph
            then appendGraph <$> cyclicTG <*> pure [initVertex, graphEnd cyclicTG] <*> nextGraph
            else appendGraph <$> trueGraph <*> pure [initVertex] <*> nextGraph
    where newNode = stmt:node
          startGraph = fromNode newNode
          -- Update bodyGraph's endNode to include guard
          bodyGraph = stmtsToGraph emptyNode (getBlockStmts body) hash
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
runIf :: (a -> b) -> (a -> b) -> YesNo a -> b
runIf f _ (Yes a) = f a
runIf _ f (No a) = f a

isYes :: YesNo a -> Bool
isYes (Yes _) = True
isYes (No _) = False

isNo :: YesNo a -> Bool
isNo = not . isYes

fromYesNo :: YesNo a -> a
fromYesNo (Yes x) = x
fromYesNo (No x) = x

stmtToIloc :: Statement -> RegHash -> ([Iloc], RegHash)
-- stmtToIloc stmt@Ret{} hash = RetToIloc stmt hash
stmtToIloc stmt hash = undefined

exprToIloc :: Reg -> Expression -> RegHash -> ([Iloc], RegHash)
exprToIloc reg expr hash = undefined

-- RetToIloc :: Statement -> RegHash -> ([Iloc], RegHash)
-- RetToIloc (Ret _ expr) hash = (exprInsns ++ [RetILOC], exprHash)
--     where (exprInsns, exprHash) = maybe ([],hash) 
--             (flip (exprToIloc retReg) hash) expr
