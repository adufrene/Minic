module Mini.Iloc.Stmt where

import Mini.Types
import Mini.Iloc.Types
import Mini.Iloc.Expr

{- Returns [Iloc] and next unused register -}
stmtToIloc :: Statement -> Baggage -> Reg -> IlocRet
stmtToIloc stmt@Ret{} = retToIloc stmt 
stmtToIloc _ = \_ _ -> ([], 0)

retToIloc :: Statement -> Baggage -> Reg -> IlocRet
retToIloc (Ret _ expr) baggage nextReg  = 
        (RetILOC : retInsn ++ exprInsns, reg)
    where (exprInsns, reg) = 
            maybe ([], nextReg) (\e -> evalExpr e baggage nextReg) expr
          retInsn = if null exprInsns then [] else [Storeret reg]

lValToIloc :: LValue -> Baggage -> Reg -> IlocRet
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


