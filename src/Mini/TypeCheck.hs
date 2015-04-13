module Mini.TypeCheck where

import Mini.Types
import Control.Monad
import Data.HashMap.Strict
import Data.Maybe
import Data.List (find)

type ErrType = String
type StatementRet = Either ErrType (Maybe Type)

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
voidType = "void"

mainId = "main"

checkTypes :: Program -> GlobalEnv
checkTypes (Program types decls funcs) = 
        let typeHash = readTypes types
            decHash = readDecls typeHash decls
            funcHash = readFuncs typeHash decHash funcs
            main = funcHash ! mainId 
        in if mainId `member` funcHash && Prelude.null (fst main) && snd main == intType
               then GlobalEnv typeHash decHash funcHash
               else error "Missing function 'fun main() int'"

createError :: HasLines a => a -> String -> Either ErrType b
createError lineItem errMsg = Left $ "Line " ++ getLineString lineItem ++ ": " ++ errMsg

readTypes :: [TypeDef] -> HashMap Id [Field]
readTypes = foldl foldFun empty 
    where foldFun hash td@(TypeDef _ id fields) = 
              let newHash = insert id fields hash
                  badField = find (\(Field _ fType _) -> fType /= intType && fType /= boolType 
                                  && not (fType `member` newHash)) fields
              in if isNothing badField 
                     then newHash 
                     else createError (fromJust badField) "Type contains field of undeclared type" 

readDecls :: HashMap Id [Field] -> [Declaration] -> HashMap Id Type
readDecls hash = foldl foldFun empty 
    where foldFun newHash dec@(Declaration _ decType id)
            | decType == intType || decType == boolType = insert id decType newHash
            | otherwise = if decType `member` hash 
                            then insert id decType newHash 
                            else createError dec "use of undefined type"

readFuncs :: HashMap Id [Field] -> HashMap Id Type -> [Function] -> HashMap Id ([Type], Type)
readFuncs typeHash decHash = foldl foldFun empty
    where foldFun hash fun@(Function line id params decls body expectRet) =
            let newHash = insert id (fmap getFieldType params, expectRet) hash
                localsWOutArgs = readDecls typeHash decls
                locals = foldl (\hash (Field _ fType id) -> insert id fType hash) localsWOutArgs params
                global = GlobalEnv typeHash decHash newHash
                maybeRetType = checkFunctionBody fun global local
                retType
                  | isJust maybeRetType = fromJust maybeRetType
                  | otherwise = "void"
            in
              if retType /= expectRet 
                  then createError fun $ "function returns " ++ retType ++ " expected " ++ expectRet
                  else newHash

getFuncType :: HasLines a => a -> Id -> Arguments -> GlobalEnv -> LocalEnv -> Either ErrType Type
getFuncType lineItem id args global local 
    | doesntExist = createError lineItem "Undefined function " ++ id
    | otherwise = do 
                     evaledArgs <- mapM (\x -> getExprType x global local) args
                     if (funcParamTypes `matches` evaledArgs)
                        then Right funcType
                        else createError lineItem "Invalid arguments"
    where funcHash = getFuncsHash global
          doesntExist = not $ id `member` funcHash
          func = funcHash ! id
          funcParamTypes = fst func
          funcType = snd func


getExprType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
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

getBinExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getBinExpType exp@(BinExp _ op lft rht) global local
    | op `elem` boolBinops = checkTypes boolType boolType -- bool
    | op `elem` equalityBinops = lftType  >>= (flip checkTypes boolType) -- int or struct
    | op `elem` relationalBinops = checkTypes intType boolType
    | op `elem` arithBinops = checkTypes intType intType -- int
    | otherwise = createError exp "invalid binary operator"
    where lftType = getExprType lft global local
          rhtType = getExprType rht global local
          checkTypes exprType retType = do
              lftM <- lftType
              rhtM <- rhtType
            if all (==exprType) [lftM, rhtM] 
                then Right retType 
                else createError exp $ "expected " ++ exprType ++ ", found " ++ lftM ++ " and " ++ rhtM

getUExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getUExpType exp@(UExp _ op opnd) global local
    | op `elem` boolUOps = validateOpnd boolType
    | op `elem` intUops = validateOpnd intType
    | otherwise = createError exp "unrecognized unary operator: " ++ op
    where validateOpnd typeString = do
              opndType <- getExprType opnd global local
              if opndType == typeString
                  then Right typeString
                  else createError exp "Expected " ++ typeString ++ ", found " ++ opndType

getDotExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getDotExpType exp@(DotExp _ lft id) global local = do
        lftType <- getExprType lft global local
        fields <- (if lftType `member` typeHash
                    then Right $ typeHash ! lftType
                    else dotErr)
        maybeField <- foldM foldFun Nothing fields
        getType maybeField
    where dotErr = createError exp $ "Unrecognized id " ++ id
          typeHash = getTypesHash global
          getType Nothing = dotErr
          getType (Just typeStr) = Right typeStr
          foldFun (Just thing) _ = Just thing
          foldFun Nothing field = if getFieldId field == id
                                      then Just $ getFieldType field
                                      else Nothing

matches :: [Type] -> [Type] -> Bool
expected `matches` given = all equalOrNull $ zip expected given
    where equalOrNull (x,y) = y == nullType || x == y

getInvocFuncType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getInvocFuncType exp@(InvocExp _ id args) = getFuncType exp id args

getIdExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getIdExpType exp@(IdExp _ id) = getIdType id exp

getIdType :: HasLines a :: Id -> a -> GlobalEnv -> LocalEnv -> Either ErrType Type
getIdType id lineItem global local
    | localContains = Right $ local ! id 
    | globalContains = Right $ globalVars ! id
    | otherwise = createError lineItem ("Undefined id " ++ id)
    where localContains = id `member` local
          globalContains = id `member` globalVars
          globalVars = getDecsHash global

getLValType :: LValue -> GlobalEnv -> LocalEnv -> Either ErrType Type
getLValType val@(LValue _ id Nothing) globs locs = getIdType id val globs locs
getLValType val@(LValue line theId (Just lval)) globs locs = do
        targetType <- getLValType lval globs locs
        targFields <- targetFields targetType
        theId `memberType` targFields
    where typesHash = getTypesHash globs
          maybeMember fields = find (\(Field _ t id) -> id == theId) fields
          err id = createError val $ "undefined id " ++ id
          memberType id fields = maybe (err id) Right . getFieldType maybeMember 
--           targetType = getLValType lval globs locs
          targetFields structType = if structType `member` typesHash
                                     then Right $ typesHash ! targetType
                                     else createError val $ "Unexpected type for id " ++ theId

getNewExpType :: Expression -> GlobalEnv -> LocalEnv -> Either ErrType Type
getNewExpType exp global _ 
    | id `member` typeHash = Right id
    | otherwise = createError exp "Undefined type" ++ id
    where id = getNewId exp
          typeHash = getTypesHash global

-- determines if all the elements of given list are equal
allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) xs

-- takes a list of types, are returns true if they are all the same type, excluding Nothing's
sameTypes :: [Maybe String] -> Bool
sameTypes types = allEqual [fromJust x | x <- types, isJust x]

checkReturn :: Statement -> GlobalEnv -> LocalEnv -> Either ErrType Type
checkReturn :: (Ret _ Nothing) _ _ = Right voidType
checkReturn :: (Ret _ Just expr) global local = getExprType expr global local

validateGuard :: Expression -> [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
validateGuard guard stmts global local = do
        guardType <- getExprType guard global local
        if guardType == boolType
            then checkStatements stmts
            else createError guard "non-boolean guard"

-- Returns Nothing means needs return after stmt, 
-- Just means function returns within conditional
checkCond :: Statement -> GlobalEnv -> LocalEnv -> StatementRet
checkCond stmt@(Cond _ guard thenBlock Nothing) global local =
        validateGuard guard thenBlock global local
checkCond stmt@(Cond _ guard thenBlock (Just elseBlock)) global local = do
        thenType <- validateGuard guard thenBlock global local
        elseType <- checkStatements elseBlock
        compareTypes thenType elseType 
    where compareTypes type1 type2 = 
          maybe (Right Nothing)  (x -> if x 
                then Right elseType 
                else createError stmt $ "function returns " ++ 
                    thenType ++ " and " ++ elseType) 
            (liftM2 (==) type1 type2)

checkLoop :: Statement -> GlobalEnv -> LocalEnv -> Either StatementRet
checkLoop stmt@(Loop _ guard stmts) global local = do
        validateGuard guard stmts global local

checkAsgn :: Statement -> GlobalEnv -> LocalEnv -> Either ErrType ()
checkAsgn stmt@(Asgn _ lval exp) global local = do
        expType <- getExprType exp global local
        lValType <- getLValType lval global local
        if lValType == expType
            then Right ()
            else createError "assigning " ++ exprType ++ " to " ++ lvalType

checkInvoc :: Statement -> GlobalEnv -> LocalEnv -> Either ErrType ()
checkInvoc stmt@(Invocation _ id args) = getFuncType stmt id args << Right ()

checkPrint :: Statement -> GlobalEnv -> LocalEnv -> Either ErrType ()
checkPrint stmt@(Print _ exp _) global local = do
        expType <- getExprType exp global local
        if expType == intType
            then Right ()
            else createError "print requires an integer parameter"

checkRead :: Statement -> GlobalEnv -> LocalEnv -> Either ErrType ()
checkRead stmt@(Read _ lval) global local = do
        lvaLType <- getLValType lval global local 
        if lValType == intType
            then Right ()
            else createError stmt "must read into int lval"

checkDelete :: Statement -> GlobalEnv -> LocalEnv -> Either ErrType ()
checkDelete stmt@(Delete _ exp) global local = do
        expType <- getExprType exp global local
        if expType `elem` [intType, boolType]
            then createError exp "cannot delete integer or boolean"
            else Right ()

checkBlock :: Statement -> GlobalEnv -> LocalEnv -> StatementRet
checkBlock (Block stmts) = checkStatements stmts

checkFunctionBody :: Function -> GlobalEnv -> LocalEnv -> Either ErrType Type
checkFunctionBody fun global local = do
        funType <- checkStatements (getFunBody fun) global local
        transformType funType
    where transformType Just type = Right type
          transformType Nothing = Right voidType

-- takes an environment and a list of statments and returns the type that the statements will return
-- returns Nothing is the statments don't return and does type checking along the way
checkStatements :: [Statement] -> GlobalEnv -> LocalEnv -> StatementRet
checkStatements :: [] _ _ = Right Nothing
checkStatements :: (stmt:stmts) global local = do
        stmtType <- delegateStmt stmt global local
        continueOrReturn stmtType stmts global local
    where continueOrReturn Nothing stmts = checkStatements stmts
          continueOrReturn justType _ = Right justType

delegateStmt :: Statement -> GlobalEnv -> LocalEnv -> StatementRet
delegateStmt stmt@Ret{} = (x y -> checkReturn stmt x y >>= Right . Just)
delegateStmt stmt@Cond{} = checkCond stmt
delegateStmt stmt@Loop{} = checkLoop stmt
delegateStmt stmt@Asgn{} = checkAsgn stmt
delegateStmt stmt@Invocation{} = retNothing $ checkInvoc stmt
delegateStmt stmt@Print{} = retNothing $ checkPrint stmt
delegateStmt stmt@Read{} = retNothing $ checkRead stmt
delegateStmt stmt@Delete{} = retNothing $ checkDelete stmt
delegateStmt stmt@Block{} = checkBlock stmt

retNothing :: Monad m => (a -> b -> m c) -> (a -> b -> StatementRet)
retNothing f = \x y -> f x y >> Right Nothing
