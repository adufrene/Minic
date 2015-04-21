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
type NumAndGraph = (LabelNum, ReturnBlock)

type Baggage = (GlobalEnv, LocalEnv, RegHash)

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
        (fst numGraph, fromYesNo $ snd numGraph)
    where argNode = emptyNode $ getFunId func-- Start node by storing args in regs
          (nextNum, regHash, locals) = 
            foldl' localFoldFun argsHashes $ getFunDeclarations func
          localFoldFun (reg,rHash,lHash) (Declaration _ dType dId) =
              (reg+1, insert dId reg rHash, insert dId dType lHash)
          argsHashes = foldl' argFoldFun (nextLabel, empty, empty) $ 
            getFunParameters func
          argFoldFun (reg,rHash,lHash) (Field _ fType fId) =
              (reg+1, insert fId reg rHash, insert fId fType lHash)
          numGraph = stmtsToGraph argNode (getFunBody func) 
            nextNum (global, locals, regHash)

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
stmtsToGraph :: Node -> [Statement] -> LabelNum -> Baggage -> NumAndGraph -- (NodeGraph, RegHash)
stmtsToGraph node [] num _ = (num, No $ fromNode node)
stmtsToGraph node (stmt:rest) nextLabel baggage = 
        case stmt of
            Block body -> stmtsToGraph node (body ++ rest) nextLabel baggage
            Cond{} -> createGraph createCondGraph
            Loop{} -> createGraph createLoopGraph
            Ret _ expr -> (nextLabel, Yes pointToExit)
            _ -> stmtsToGraph newNode rest nextLabel baggage
    where successorGraph = 
            stmtsToGraph startNode rest (nextLabel + 1) baggage
          startNode = emptyNode $ createLabel nextLabel
          pointToExit = 
            (buildG defaultBounds 
                [(entryVertex,initVertex), (initVertex,exitVertex)],
                singleton initVertex newNode)
          newNode = node `addToNode` stmtToIloc stmt baggage
          createGraph f = f stmt node successorGraph baggage

-- Yes means we will return in this graph
-- No means we may not return in this graph
createCondGraph :: Statement -> Node -> NumAndGraph -> Baggage -> NumAndGraph
createCondGraph (Cond _ guard thenBlock maybeElseBlock) node (num, nextG) baggage = 
        linkGraphs ifThenGraph elseNumGraph thenLabel nextG
    where newNode = node `addToNode` [] --(exprToIloc guard baggage) -- finish node with guard
          (thenLabel, thenGraph) = graphFromBlock thenBlock num
          elseNumGraph = graphFromBlock <$> maybeElseBlock <*> pure thenLabel
          ifThenGraph = appendIf <$> thenGraph
          appendIf = appendGraph (fromNode newNode) [initVertex]
          graphFromBlock block label = 
            stmtsToGraph (emptyNode $ createLabel label) 
                (getBlockStmts block) (label + 1) baggage 

linkGraphs :: ReturnBlock -> Maybe NumAndGraph -> LabelNum -> ReturnBlock -> NumAndGraph
linkGraphs ifThenGraph Nothing num nextGraph =
        (num, appendGraph <$> ifThenGraph <*> pure (initVertex:secVert) <*> nextGraph)
    where secVert = yesNo (const []) (\g -> [graphEnd $ pure g]) ifThenGraph 
linkGraphs ifThenGraph (Just (elseNum, elseGraph)) num nextGraph =
        (elseNum, yesNo Yes (\g -> appendGraph g ifVertices <$> nextGraph) ifGraph)
    where ifGraph = appendGraph <$> ifThenGraph <*> pure [initVertex] <*> elseGraph
          thenVertex = [graphEnd ifThenGraph | isNo ifThenGraph]
          elseVertex = [graphEnd ifGraph | isNo elseGraph]
          ifVertices = elseVertex ++ thenVertex

graphEnd :: ReturnBlock -> Vertex
graphEnd g = snd $ bounds $ fst $ fromYesNo g

createLoopGraph :: Statement -> Node -> NumAndGraph -> Baggage -> NumAndGraph
createLoopGraph (Loop _ guard body) node (nextNum, nextGraph) baggage = 
        (bodyNum, if isNo trueGraph
            then appendGraph <$> cyclicTG <*> pure [initVertex, graphEnd cyclicTG] <*> nextGraph
            else appendGraph <$> trueGraph <*> pure [initVertex] <*> nextGraph)
    where newNode = node `addToNode` [] -- guard iloc
          startGraph = fromNode newNode
          startNode = emptyNode $ createLabel nextNum
          -- Update bodyGraph's endNode to include guard
          (bodyNum, bodyGraph) = 
            stmtsToGraph startNode (getBlockStmts body) (nextNum + 1) baggage
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

stmtToIloc :: Statement -> Baggage -> [Iloc]
-- stmtToIloc stmt@Ret{} hash = RetToIloc stmt hash
stmtToIloc stmt hash = []

exprToIloc :: Expression -> Baggage -> ([Iloc], Reg)
exprToIloc expr hash = ([],0)

retToIloc :: Statement -> Baggage -> [Iloc]
retToIloc (Ret _ expr) hash = RetILOC : retInsn ++ exprInsns
    where (exprInsns, reg) = maybe ([],-1) (`exprToIloc` hash) expr
          retInsn = if null exprInsns then [] else [Storeret reg]

lValToIloc :: LValue -> Baggage -> Reg -> ([Iloc], Reg)
lValToIloc _ _ _ = ([], 0)
-- lValToIloc (LValue _ name Nothing) hash _ = ([], hash ! name)
-- lValToIloc (LValue _ name (Just lval)) nextReg = 
--         recur (hash ! name) lval (nextReg + 1)
--     where recur reg (LValue _ newName (Just newVal)) nextReg = 
--             Loadai reg (getTypeOffset 
--           recur reg (LValue _ newName Nothing) currType nextReg = 

asgnToIloc :: Statement -> Baggage -> [Iloc]
asgnToIloc (Asgn _ lval expr) hash = undefined
    where (exprInsns, reg) = exprToIloc expr hash

