module Mini.TypeCheck where

import Mini.Types
import Data.HashMap.Strict
import Data.Maybe
import Data.List (find)

arithBinops      = ["+", "-", "*", "/"]
relationalBinops = ["<", ">", "<=", ">="]
equalityBinops   = ["==", "!="]
boolBinops       = ["&&", "||"]

arithUops = ["-"]
boolUOps  = ["!"]

intUops = arithUops

intType = "int"
boolType = "bool"
nullType = "null"

mainId = "main"

checkTypes :: Program -> GlobalEnv
checkTypes (Program types decls funcs) = 
        let typeHash = readTypes types
            decHash = readDecls typeHash decls
            funcHash = readFuncs typeHash decHash funcs
            main = funcHash ! mainId 
        in if mainId `member` funcHash && Prelude.null (fst main) && snd main == intType
               then GlobalEnv typeHash decHash funcHash
               else error "Missing function 'int main()'"

printError :: HasLines a => a -> String -> b
printError lineItem errMsg = error $ "Line " ++ getLineString lineItem ++ ": " ++ errMsg

readTypes :: [TypeDef] -> HashMap Id [Field]
readTypes = foldl foldFun empty 
    where foldFun hash td@(TypeDef _ id fields) = 
              let newHash = insert id fields hash
                  badField = find (\(Field _ fType _) -> fType /= intType && fType /= boolType 
                                  && not (fType `member` newHash)) fields
              in if isNothing badField 
                     then newHash 
                     else printError (fromJust badField) "Type contains field of undeclared type" 

readDecls :: HashMap Id [Field] -> [Declaration] -> HashMap Id Type
readDecls hash = foldl foldFun empty 
    where foldFun newHash dec@(Declaration _ decType id)
            | decType == intType || decType == boolType = insert id decType newHash
            | otherwise = if decType `member` hash 
                            then insert id decType newHash 
                            else printError dec "use of undefined type"

readFuncs :: HashMap Id [Field] -> HashMap Id Type -> [Function] -> HashMap Id ([Type], Type)
readFuncs typeHash decHash = foldl foldFun empty
    where foldFun hash fun@(Function line id params decls body expectRet) =
            let newHash = insert id (fmap getFieldType params, expectRet) hash
                localsWOutArgs = readDecls typeHash decls
                locals = foldl (\hash (Field _ fType id) -> insert id fType hash) localsWOutArgs params
                global = GlobalEnv typeHash decHash newHash
                maybeRetType = checkStatements global locals body
                retType
                  | isJust maybeRetType = fromJust maybeRetType
                  | otherwise = "void"
            in
              if retType /= expectRet 
                  then printError fun $ "function returns " ++ retType ++ " expected " ++ expectRet
                  else newHash

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
    | op `elem` boolBinops = checkTypes boolType boolType -- bool
    | op `elem` equalityBinops = checkTypes lftType boolType -- int & struct
    | op `elem` relationalBinops = checkTypes intType boolType
    | op `elem` arithBinops = checkTypes intType intType -- int
    | otherwise = printError exp "invalid binary operator"
    where lftType = getExprType lft global local
          rhtType = getExprType rht global local
          checkTypes exprType retType = 
            if all (==exprType) [lftType, rhtType] 
                then retType 
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

-- determines if all the elements of given list are equal
allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) xs

-- takes a list of types, are returns true if they are all the same type, excluding Nothing's
sameTypes :: [Maybe String] -> Bool
sameTypes types = allEqual [fromJust x | x <- types, isJust x]

-- takes an environment and a list of statments and returns the type that the statements will return
-- returns Nothing is the statments don't return and does type checking along the way
checkStatements :: GlobalEnv -> LocalEnv -> [Statement] -> Maybe String
checkStatements globs locs = recur 
  where
    getExprTypeHelper expr = getExprType expr globs locs

    recur :: [Statement] -> Maybe String
    recur (Ret _ Nothing:_) = Just "void"
    recur (Ret _ (Just expr):_) = Just $ getExprTypeHelper expr
    recur (Cond line expr (Block ifStmts) Nothing:rest)
      | getExprTypeHelper expr /= boolType = error $ show line ++  ": non-boolean guard"
      | sameTypes [ifBlockType, restType] = restType
      | otherwise = error $ show line ++ ": if block returns " ++ fromJust ifBlockType ++
          ", code after if statment returns" ++ fromJust restType
        where
          ifBlockType = recur ifStmts
          restType = recur rest
    recur (Cond line expr (Block ifStmts) (Just (Block elseStmts)):rest)
      | getExprTypeHelper expr /= boolType = error $ show line ++  ": non-boolean guard"
      | not $ sameTypes [ifBlockType, elseBlockType, restType] =
          error $ show line ++ ": if block, else block, and preceding code do not return save types"
      | all isJust [ifBlockType, elseBlockType] = ifBlockType
      | isNothing restType = error $ show line ++ ": does not return in all paths"
      | otherwise = restType
        where
          ifBlockType = recur ifStmts
          elseBlockType = recur elseStmts
          restType = recur rest
    recur (Cond line _ _ _:_) = error $ show line ++ ": bad conditional"
    recur (Loop line expr (Block whileStmts):rest)
      | getExprTypeHelper expr /= boolType = error $ show line ++  ": non-boolean guard"
      | sameTypes [whileBlockType, restType] = restType
      | otherwise = error $ show line ++ ": while block returns " ++ fromJust whileBlockType ++
          ", code after loop returns " ++ fromJust restType
        where
          whileBlockType = recur whileStmts
          restType = recur rest
    recur (Loop line _ _:_) = error $ show line ++ ": bad loop"
    -- TODO: Asgn, Print, Read, Delete, Invocation
    recur (_:rest) = recur rest
    recur [] = Nothing
