module TypeCheck where

import Mini.Types
import Data.HashMap.Lazy
import Data.Maybe

arithBinops      = ["+", "-", "*", "/"]
relationalBinops = ["<", ">", "<=", ">="]
equalityBinops   = ["==", "!="]
boolBinops       = ["&&", "||"]

intBinops    = arithBinops ++ relationalBinops ++ equalityBinops
structBinops = equalityBinops

arithUops = ["-"]
boolUOps  = ["!"]

intUops = arithUops

intType = "int"
boolType = "bool"
nullType = "null"

printError :: HasLines a => a -> String -> b
printError lineItem errMsg = error $ getLineString lineItem ++ ":" ++ errMsg

readTypes :: Program -> HashMap Id [Field]
readTypes prog = foldl foldFun empty $ getTypes prog
    where foldFun hash td@(TypeDef _ id fields) = 
              let newHash = insert id fields hash
                  isValid = all (\field -> getFieldType field `member` newHash) fields
              in if isValid then newHash else printError td "Type contains field of undeclared type" 

readDecls :: Program -> HashMap Id [Field] -> HashMap Id Type
readDecls prog hash = foldl foldFun empty $ getDeclarations prog
    where foldFun newHash dec@(Declaration _ decType id)
            | decType == intType || decType == boolType = insert id decType newHash
            | otherwise = if decType `member` hash 
                            then insert id decType newHash 
                            else printError dec "use of undefined type"

readFuncs :: Program -> (HashMap Id [Field], HashMap Id Type) -> HashMap Id Type
readFuncs prog (typeHash, decHash) = empty

getExprType :: Expression -> GlobalEnv -> LocalEnv -> Type
getExprType exp@BinExp{} = getBinExpType exp 
getExprType exp@UExp{} = getUExpType exp
getExprType exp@DotExp{} = getDotExpType exp
getExprType exp@InvocExp{} = getInvocFuncType exp
getExprType exp@IdExp{} = getIdExpType exp
getExprType IntExp{} = \_ _ -> intType
getExprType TrueExp{} = \_ _ -> boolType
getExprType FalseExp{} = \_ _ -> boolType
getExprType exp@NewExp{} = getNewExpType exp
getExprType NullExp{} = \_ _ -> nullType

-- We can't compare against null?
getBinExpType :: Expression -> GlobalEnv -> LocalEnv -> Type
getBinExpType exp@(BinExp _ op lft rht) global local
    | op `elem` boolBinops = checkTypes boolType
    | op `elem` intBinops = checkTypes intType
    | op `elem` structBinops = checkTypes lftType -- Allows bool == bool, shouldn't?
    | otherwise = printError exp "invalid binary operator"
    where lftType = getExprType lft global local
          rhtType = getExprType rht global local
          checkTypes exprType = 
            if all (==exprType) [lftType, rhtType] 
                then exprType 
                else printError exp "expected " ++ exprType ++ ", found " ++ lftType ++ " and " ++ rhtType

getUExpType :: Expression -> GlobalEnv -> LocalEnv -> Type
getUExpType exp@(UExp _ op opnd) global local
    | op `elem` boolUOps = validateOpnd boolType
    | op `elem` intUops = validateOpnd intType
    | otherwise = printError exp "unrecognized unary operator: " ++ op
    where opndType = getExprType opnd global local
          validateOpnd typeString =
              if opndType == typeString
                  then typeString
                  else printError exp "Expected " ++ typeString ++ ", found " ++ opndType

getDotExpType :: Expression -> GlobalEnv -> LocalEnv -> Type
getDotExpType exp@(DotExp _ lft id) global local = 
        getType $ foldl foldFun Nothing structFields
    where dotErr = printError exp $ "Unrecognized id " ++ id
          lftType = getExprType lft global local
          typeHash = getTypesHash global
          getType Nothing = dotErr 
          getType (Just typeStr) = typeStr
          foldFun (Just thing) _ = Just thing
          foldFun Nothing field = if getFieldId field == id
                                      then Just $ getFieldType field
                                      else Nothing
          structFields = if lftType `member` typeHash
                             then typeHash ! lftType
                             else dotErr

getInvocFuncType :: Expression -> GlobalEnv -> LocalEnv -> Type
getInvocFuncType exp@(InvocExp _ id args) global local
    | doesntExist = printError exp "Undefined function " ++ id
    | funcParamTypes == evaledArgs = funcType
    | otherwise = printError exp "Invalid arguments"
    where funcHash = getFuncsHash global
          doesntExist = not $ id `member` funcHash
          func = funcHash ! id
          funcParamTypes = fst func
          evaledArgs = fmap (\x -> getExprType x global local) args
          funcType = snd func


getIdExpType :: Expression -> GlobalEnv -> LocalEnv -> Type
getIdExpType exp@(IdExp _ id) global local
    | localContains = local ! id 
    | globalContains = globalVars ! id
    | otherwise = printError exp "Undefined id " ++ id
    where localContains = id `member` local
          globalContains = id `member` globalVars
          globalVars = getDecsHash global

getNewExpType :: Expression -> GlobalEnv -> LocalEnv -> Type
getNewExpType exp global _ 
    | id `member` typeHash = id
    | otherwise = printError exp "Undefined type" ++ id
    where id = getNewId exp
          typeHash = getTypesHash global

checkStatements :: GlobalEnv -> LocalEnv -> String -> [Statement] -> Maybe String
checkStatements glob loc expect stmts =
   checkRet $ actuallyCheckStatements stmts
      where
         checkRet :: (Maybe String, Maybe Int) -> Maybe String
         checkRet (Nothing, _) = Nothing
         checkRet (Just theType, line)
            | (theType == expect) = (Just expect)
            | otherwise = error $ lineStr ++ "statement returns " ++ theType ++ " expected " ++ expect
               where
                  lineStr
                     | isJust line = (show (fromJust line)) ++ ": "
                     | otherwise = ""

         recur :: [Statement] -> Maybe String
         recur recurStmts = checkStatements glob loc expect recurStmts

         checkGuard :: Expression -> Int -> String
         checkGuard expr line
            | exprType /= "bool" = error $ (show line) ++ ": non-boolean guard"
            | otherwise = exprType
               where exprType = getExprType expr glob loc

         actuallyCheckStatements :: [Statement] -> (Maybe String, Maybe Int)
         actuallyCheckStatements ((Ret line Nothing):_) = ((Just "void"), (Just line))
         actuallyCheckStatements ((Ret line (Just expr)):_) =
            ((Just (getExprType expr glob loc)), (Just line))
         actuallyCheckStatements ((Cond line expr (Block ifStmts) Nothing):rest) =
            let ifStmtsType = recur ifStmts;
                guardType = checkGuard expr line
                in (recur rest, Just line)
         actuallyCheckStatements ((Cond line expr (Block ifStmts) (Just (Block elseStmts))):rest)
            | (isJust ifType) && (ifType == elseType) = (ifType, Just line)
            | otherwise = (recur rest, Just line)
               where
                  guardType = checkGuard expr line
                  ifType = recur ifStmts
                  elseType = recur elseStmts
         actuallyCheckStatements ((Cond line _ _ _):_) = error $ (show line) ++ ": bad conditional"
         actuallyCheckStatements ((Loop line expr (Block whileStmts)):rest) =
            let whileStmtsType = recur whileStmts;
                guardType = checkGuard expr line
                in (recur rest, Just line)
         actuallyCheckStatements ((Loop line _ _):_) = error $ (show line) ++ ": bad loop"
         -- TODO: Asgn, Print, Read, Delete, Invocation
         actuallyCheckStatements (_:rest) = (recur rest, Nothing)
         actuallyCheckStatements [] = (Nothing, Nothing)
