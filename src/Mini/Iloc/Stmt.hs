module Mini.Iloc.Stmt where

import Data.Maybe 
import Data.List -- (findIndex, elemIndex, find)
import Data.HashMap.Strict hiding (null)
import Mini.Types
import Mini.Iloc.Types
import Mini.Iloc.Expr

appIloc :: Iloc -> IlocRet -> IlocRet
appIloc newIloc (iloc, reg) = (iloc ++ [newIloc], reg)

preIloc :: Iloc -> IlocRet -> IlocRet
preIloc newIloc (iloc, reg) = (newIloc : iloc, reg)

{- Returns [Iloc] and next unused register -}
stmtToIloc :: Statement -> Baggage -> Reg -> IlocRet
stmtToIloc stmt@Ret{} = retToIloc stmt 
stmtToIloc stmt@Asgn{} = asgnToIloc stmt
stmtToIloc stmt@Print{} = printToIloc stmt
stmtToIloc stmt@Read{} = readToIloc stmt
stmtToIloc stmt@Delete{} = delToIloc stmt
stmtToIloc stmt@InvocSt{} = invocStToIloc stmt

retToIloc :: Statement -> Baggage -> Reg -> IlocRet
retToIloc (Ret _ expr) baggage nextReg  = 
        (exprInsns ++ retInsn ++ [RetILOC], reg)
    where (exprInsns, reg) = 
            maybe ([], nextReg) (\e -> evalExpr e baggage nextReg) expr
          retInsn = if null exprInsns then [] else [Storeret reg]

lValToIloc :: LValue -> Baggage -> Reg -> IlocRet
lValToIloc (LValue _ name Nothing) (_, _, regHash) expReg = 
        ([if name `member` regHash
            then Storeai expReg (regHash ! name) 0
            else Storeglobal expReg name], expReg + 1)
lValToIloc (LValue _ name (Just lval)) (global, local, regHash) expReg
    | name `member` regHash = Mov (regHash ! name) (expReg + 1) `preIloc` rest local
    | otherwise = Loadglobal name (expReg + 1) `preIloc` rest (getDecsHash global)
    where rest hash = loadOffsets (findFields name hash global) lval (expReg + 1)
          loadOffsets fields (LValue _ newName nextVal) newReg =
              case nextVal of
                  Just val -> Loadai newReg (getOffset newName fields) (newReg + 1)
                    `preIloc` loadOffsets (getFields newName fields global) val (newReg + 1)
                  Nothing -> ([Storeai expReg newReg (getOffset newName fields)], newReg + 1)

findFields :: Id -> DecHash -> GlobalEnv -> [Field]
findFields name hash global = getStructHash global ! nameType
    where nameType = hash ! name
              
getFields :: Id -> [Field] -> GlobalEnv -> [Field]
getFields name fields global = getStructHash global ! fieldType
    where fieldType = getFieldType $ fromMaybe (error "Bad Fields") maybeField 
          maybeField = ((==name) . getFieldId) `find` fields

getOffset :: Id -> [Field] -> Immed
getOffset id fields = fromMaybe (error "WTF Happened!") $
    ((==id) . getFieldId) `findIndex` fields

asgnToIloc :: Statement -> Baggage -> Reg -> IlocRet
asgnToIloc (Asgn _ lval expr) baggage nextReg = (exprInsns ++ lvalInsns, newReg)
    where (exprInsns, exprReg) = evalExpr expr baggage nextReg
          (lvalInsns, newReg) = lValToIloc lval baggage exprReg

printToIloc :: Statement -> Baggage -> Reg -> IlocRet
printToIloc (Print _ expr endl) baggage nextReg = (exprInsns ++ [printInsn], exprReg + 1)
    where (exprInsns, exprReg) = evalExpr expr baggage nextReg
          printInsn = (if endl then Println else PrintILOC) exprReg

readToIloc :: Statement -> Baggage -> Reg -> IlocRet
readToIloc (Read _ lval) baggage nextReg = (readInsn : lvalInsns, lvalReg)
    where readInsn = ReadILOC nextReg
          (lvalInsns, lvalReg) = lValToIloc lval baggage nextReg

delToIloc :: Statement -> Baggage -> Reg -> IlocRet
delToIloc (Delete _ expr) baggage nextReg = (exprInsns ++ [delInsn], exprReg + 1)
    where (exprInsns, exprReg) = evalExpr expr baggage nextReg
          delInsn = Del exprReg

invocStToIloc :: Statement -> Baggage -> Reg -> IlocRet
invocStToIloc (InvocSt _ name args) = evalInvoc name args
