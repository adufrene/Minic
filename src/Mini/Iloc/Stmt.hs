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
lValToIloc (LValue _ name (Just lval)) baggage expReg =
        (loads ++ [store], nextReg + 1)
    where store = Storeai expReg nextReg (getOffset name fields)
          (nextReg, fields, loads) = loadLeft lval baggage $ expReg + 1

loadLeft :: LValue -> Baggage -> Reg -> (Reg, [Field], [Iloc])
loadLeft (LValue _ newName Nothing) (global, local, regHash) newReg =
        if newName `member` regHash
            then (newReg, (localFindFields newName local), [Mov (regHash ! newName) newReg]) 
            else (newReg, (localFindFields newName $ getDecsHash global),
                              [Loadglobal newName newReg])
    where localFindFields name hash = findFields name hash global
loadLeft (LValue _ newName (Just val)) bag@(global, local, regHash) newReg =
        (resReg, getFields newName lFields global, leftInsns ++ [load])
    where (lReg, lFields, leftInsns) = loadLeft val bag newReg
          load = Loadai lReg (getOffset newName lFields) resReg
          resReg = lReg + 1

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
