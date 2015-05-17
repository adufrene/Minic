module Mini.TypeCheck 
    ( checkTypes
    , getExprType
    , ErrType
    ) where

import Mini.Types
import Control.Monad
import Data.HashMap.Strict
import Data.Maybe
import Data.List (find)

type ErrType = String
type StatementRet = Either ErrType (YesNo (Maybe Type))

arithBinops :: [String]
arithBinops      = ["+", "-", "*", "/"]

relationalBinops :: [String]
relationalBinops = ["<", ">", "<=", ">="]

equalityBinops :: [String]
equalityBinops   = ["==", "!="]

boolBinops :: [String]
boolBinops       = ["&&", "||"]

arithUops :: [String]
arithUops = ["-"]

boolUOps :: [String]
boolUOps  = ["!"]

intUops :: [String]
intUops = arithUops

mainId :: String
mainId = "main"

getTypeString :: Maybe Type -> Type
getTypeString Nothing = voidType
getTypeString (Just justType) = justType

checkTypes :: Program -> Either ErrType GlobalEnv
checkTypes (Program structs decls funcs) = do
        structHash <- readStructs structs
        decHash <- readDecls structHash decls
        funcHash <- readFuncs structHash decHash funcs
        let main = funcHash ! mainId
        if mainId `member` funcHash && Prelude.null (fst main) && snd main == intType
            then Right $ GlobalEnv structHash decHash funcHash
            else Left "Missing function 'fun main() int'"

createError :: HasLines a => a -> String -> Either ErrType b
createError lineItem errMsg = Left $ "Line " ++ getLineString lineItem ++ ": " ++ errMsg

readStructs :: [TypeDef] -> Either ErrType StructHash
readStructs = foldM foldFun empty
    where foldFun hash (TypeDef _ typeId fields) = 
              let newHash = insert typeId fields hash
                  badField = find (\(Field _ fType _) -> fType /= intType && fType /= boolType 
                                  && not (fType `member` newHash)) fields
              in if isNothing badField 
                     then Right newHash 
                     else createError (fromJust badField) "Type contains field of undeclared type" 

readDecls :: StructHash -> [Declaration] -> Either ErrType DecHash
readDecls hash = foldM foldFun empty 
    where foldFun newHash dec@(Declaration _ decType decId)
            | decType `elem` [intType, boolType] = Right $ insert decId decType newHash
            | otherwise = if decType `member` hash 
                            then Right $ insert decId decType newHash 
                            else createError dec "use of undefined type"

readFuncs :: StructHash -> DecHash -> [Function] -> Either ErrType FunHash
readFuncs structHash decHash = foldM foldFun empty
    where createGlobal = GlobalEnv structHash decHash 
          createLocal = foldl (\hash (Field _ fType fieldId) -> insert fieldId fType hash)
          foldFun hash fun@(Function _ fieldId params decls _ expectRet) = do
            localsWOutArgs <- readDecls structHash decls
            let newHash = insert fieldId (fmap getFieldType params, expectRet) hash
            retType <- checkFunctionBody fun (createGlobal newHash) (createLocal localsWOutArgs params)
            if retType /= expectRet && retType /= nullType
                then createError fun $ "function returns " ++ retType ++ " expected " ++ expectRet
                else Right newHash

getFuncType :: HasLines a => a -> String -> Arguments -> GlobalEnv -> LocalEnv -> Either ErrType Type
getFuncType lineItem funcId args global local 
    | doesntExist = createError lineItem $ "Undefined function " ++ funcId
    | otherwise = do 
                     evaledArgs <- mapM (\x -> getExprType x global local) args
                     if funcParamTypes `matches` evaledArgs
                        then Right funcType
                        else createError lineItem "Invalid arguments"
    where funcHash = getFuncsHash global
          doesntExist = not $ funcId `member` funcHash
          func = funcHash ! funcId
          funcParamTypes = fst func
          funcType = snd func


getExprType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getExprType expr@BinExp{} = getBinExpType expr 
getExprType expr@UExp{} = getUExpType expr
getExprType expr@DotExp{} = getDotExpType expr
getExprType expr@InvocExp{} = getInvocFuncType expr
getExprType expr@IdExp{} = getIdExpType expr
getExprType IntExp{} = \_ _ -> Right intType
getExprType TrueExp{} = \_ _ -> Right boolType
getExprType FalseExp{} = \_ _ -> Right boolType
getExprType expr@NewExp{} = getNewExpType expr
getExprType NullExp{} = \_ _ -> Right nullType

getBinExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getBinExpType expr@(BinExp _ op lft rht) global local
    | op `elem` boolBinops = checkBinTypes [boolType] boolType -- bool
    | op `elem` equalityBinops = lftType >>= (\x -> checkBinTypes [x, nullType] boolType) -- int or struct
    | op `elem` relationalBinops = checkBinTypes [intType] boolType
    | op `elem` arithBinops = checkBinTypes [intType] intType -- int
    | otherwise = createError expr "invalid binary operator"
    where lftType = getExprType lft global local
          rhtType = getExprType rht global local
          checkBinTypes expTypes retType = do
              lftM <- lftType
              rhtM <- rhtType
              if all (`elem` expTypes) [lftM, rhtM] 
                  then Right retType 
                  else createError expr $ "expected " ++ show expTypes 
                        ++ ", found " ++ lftM ++ " and " ++ rhtM

getUExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getUExpType expr@(UExp _ op opnd) global local
    | op `elem` boolUOps = validateOpnd boolType
    | op `elem` intUops = validateOpnd intType
    | otherwise = createError expr $ "unrecognized unary operator: " ++ op
    where validateOpnd typeString = do
              opndType <- getExprType opnd global local
              if opndType == typeString
                  then Right typeString
                  else createError expr $ "Expected " ++ typeString ++ ", found " ++ opndType

getDotExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getDotExpType expr@(DotExp _ lft dotId) global local = do
        lftType <- getExprType lft global local
        fields <- if lftType `member` structHash
                    then Right $ structHash ! lftType
                    else dotErr
        let maybeField = foldl foldFun Nothing fields
        getType maybeField
    where dotErr = createError expr $ "Unrecognized id " ++ dotId
          structHash = getStructHash global
          getType Nothing = dotErr
          getType (Just typeStr) = Right typeStr
          foldFun (Just thing) _ = Just thing
          foldFun Nothing field = if getFieldId field == dotId
                                      then Just $ getFieldType field
                                      else Nothing

matches :: [Type] -> [Type] -> Bool
expected `matches` given = all equalOrNull $ zip expected given
    where equalOrNull (x,y) = y == nullType || x == y

getInvocFuncType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getInvocFuncType expr@(InvocExp _ invocId args) = getFuncType expr invocId args

getIdExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getIdExpType expr@(IdExp _ expId) = getIdType expId expr

getIdType :: HasLines a => String -> a -> GlobalEnv -> LocalEnv -> Either ErrType Type
getIdType name lineItem global local
    | localContains = Right $ local ! name 
    | globalContains = Right $ globalVars ! name
    | otherwise = createError lineItem ("Undefined id " ++ name)
    where localContains = name `member` local
          globalContains = name `member` globalVars
          globalVars = getDecsHash global

getLValType :: LValue -> GlobalEnv -> LocalEnv -> Either ErrType Type
getLValType val@(LValue _ lvalId Nothing) globs locs = getIdType lvalId val globs locs
getLValType val@(LValue _ theId (Just lval)) globs locs = do
        targetType <- getLValType lval globs locs
        targFields <- targetFields targetType
        theId `memberType` targFields
    where structHash = getStructHash globs
          maybeMember = find (\(Field _ _ fieldId) -> fieldId == theId) 
          err errId = createError val $ "undefined id " ++ errId
          memberType fieldId fields = maybe (err fieldId) (Right . getFieldType) $ maybeMember fields
--           targetType = getLValType lval globs locs
          targetFields structType = if structType `member` structHash
                                     then Right $ structHash ! structType
                                     else createError val $ "Unexpected type for id " ++ theId

getNewExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getNewExpType expr global _ 
    | newId `member` structHash = Right newId
    | otherwise = createError expr $ "Undefined type" ++ newId
    where newId = getNewId expr
          structHash = getStructHash global

-- determines if all the elements of given list are equal
allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) xs

-- takes a list of types, are returns true if they are all the same type, excluding Nothing's
sameTypes :: [Maybe String] -> Bool
sameTypes types = allEqual [fromJust x | x <- types, isJust x]

checkReturn :: [Statement] -> GlobalEnv -> LocalEnv -> Either ErrType Type
checkReturn (Ret _ Nothing:_) _ _ = Right voidType
checkReturn (Ret _ (Just expr):_) global local = getExprType expr global local

validateGuard :: Expression -> Statement -> GlobalEnv -> LocalEnv -> StatementRet
validateGuard condGuard (Block stmts) global local = do
        guardType <- getExprType condGuard global local
        if guardType == boolType
            then checkStatements stmts global local
            else createError condGuard "non-boolean guard"

validateReturn :: HasLines l => l -> Maybe Type -> [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
validateReturn line expectRet nextStmts global local = do
        nextRet <- checkStatements nextStmts global local
        compareReturns expectRet nextRet
    where compareReturns Nothing ret = Right ret 
          compareReturns (Just t1) (Yes (Just ret)) =
               if t1 == ret || nullType `elem` [t1, ret]
                then Right $ Yes $ Just $ fromMaybe nullType $ (/= nullType) `find` [t1, ret]
                else createError line $ "Function returns " ++ t1 ++ " and " ++ ret
          compareReturns (Just t1) (No ret) = 
               case ret of
                   Just t2 -> if t1 /= t2 && nullType `notElem` [t1, t2]
                                  then createError line $ "Function returns " ++ t1 ++ " and " ++ t2
                                  else Right $ No $ Just $ fromMaybe nullType $ (/= nullType) `find` [t1, t2]
                   Nothing -> Right $ No (Just t1)

-- Return: 
-- Nothing means needs return after stmt, 
-- Just means function returns within conditional
checkCond :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkCond (stmt@(Cond _ condGuard thenBlock Nothing):rest) global local = do
        thenType <- validateGuard condGuard thenBlock global local
        validateReturn stmt (fromYesNo thenType) rest global local
checkCond (stmt@(Cond _ condGuard thenBlock (Just elseBlock)):rest) global local = do
        thenType <- validateGuard condGuard thenBlock global local
        elseType <- checkStatements (getBlockStmts elseBlock) global local
        compareTypes thenType elseType 
    where compareTypes (Yes (Just t)) (No _) = validateReturn stmt (Just t) rest global local
          compareTypes (No _) (Yes (Just t)) = validateReturn stmt (Just t) rest global local
          compareTypes (No _) (No _) = validateReturn stmt Nothing rest global local
          compareTypes (Yes (Just t1)) (Yes (Just t2)) = 
            if t1 == t2 || nullType `elem` [t1, t2]
              then Right $ Yes $ Just $ fromMaybe nullType $ (/= nullType) `find` [t1, t2]
              else createError stmt $ "Function returns " ++ t1 ++ " and " ++ t2

checkLoop :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkLoop (stmt@(Loop _ loopGuard body):rest) global local = do
        expectRet <- validateGuard loopGuard body global local
        validateReturn stmt (fromYesNo expectRet) rest global local

checkAsgn :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkAsgn (stmt@(Asgn _ lval expr):rest) global local = do
        expType <- getExprType expr global local
        lValType <- getLValType lval global local
        if lValType == expType || expType == nullType
            then checkStatements rest global local
            else createError stmt $ "assigning " ++ expType ++ " to " ++ lValType

checkInvoc :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkInvoc (stmt@(InvocSt _ invocId args):rest) global local =  do
        getFuncType stmt invocId args global local
        checkStatements rest global local

checkPrint :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkPrint (stmt@(Print _ expr _):rest) global local = do
        expType <- getExprType expr global local
        if expType == intType
            then checkStatements rest global local
            else createError stmt "print requires an integer parameter"

checkRead :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkRead (stmt@(Read _ lval):rest) global local = do
        lValType <- getLValType lval global local 
        if lValType == intType
            then checkStatements rest global local
            else createError stmt "must read into int lval"

checkDelete :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkDelete (Delete _ expr:rest) global local = do
        expType <- getExprType expr global local
        if expType `elem` [intType, boolType]
            then createError expr "cannot delete integer or boolean"
            else checkStatements rest global local

checkBlock :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkBlock (Block stmts:rest) = checkStatements (stmts ++ rest)

checkFunctionBody :: Function -> GlobalEnv -> LocalEnv -> Either ErrType Type
checkFunctionBody fun global local = do
        funType <- checkStatements (getFunBody fun) global local
        if isNo funType && fromYesNo funType /= Just voidType && 
                isJust (fromYesNo funType)
            then Left $ "Function " ++ getFunId fun ++ " is missing return at end"
            else Right $ getTypeString $ fromYesNo funType

-- takes an environment and a list of statments and returns the type that the statements will return
-- returns Nothing is the statments don't return and does type checking along the way
checkStatements :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkStatements [] _ _ = Right $ No Nothing
checkStatements stmts@(stmt:_) global local = delegateStmt stmt stmts global local

delegateStmt :: Statement -> [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
delegateStmt stmt@Ret{} = \x y z -> checkReturn x y z >>= Right . Yes . Just
delegateStmt stmt@Cond{} = checkCond 
delegateStmt stmt@Loop{} = checkLoop 
delegateStmt stmt@Asgn{} = checkAsgn 
delegateStmt stmt@InvocSt{} = checkInvoc 
delegateStmt stmt@Print{} = checkPrint 
delegateStmt stmt@Read{} = checkRead 
delegateStmt stmt@Delete{} = checkDelete 
delegateStmt stmt@Block{} = checkBlock 
