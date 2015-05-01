module Mini.Asm.Types where

import Mini.Iloc.Types

type AsmReg = Reg

data Asm = AsmAdd
         | AsmSub
         | AsmDiv
         | AsmMult
         | AsmMulti
         | AsmSub
         | AsmComp
         | AsmCompi
         | AsmBrz
         | AsmLoadai
         | AsmLoadGlobal
         | AsmLoadInArg
         | AsmLoadRet
         | AsmStoreai
         | AsmStoreGlobal
         | AsmStoreInArg
         | AsmStoreOutArg
         | AsmStoreRet
         | AsmCall
         | AsmRet
         | AsmNew
         | AsmDel
         | AsmPrint
         | AsmPrintln
         | AsmRead
         | AsmMov AsmReg AsmReg
         | AsmMovi Immed AsmReg
         | AsmCMoveq Immed AsmReg
         | AsmcMovgeq Immed AsmReg
         | AsmCMovgq Immed AsmReg
         | AsmCmovleq Immed AsmReg
         | AsmCmovlq Immed AsmReg
         | AsmCmovneq Immed AsmReg
         deriving (Eq)

regStr :: Reg -> String
regStr r = " %r" ++ show r

immStr :: Immed -> String
immStr i = " $" ++ show i

instance Show Iloc where
        show (AsmMov r1 r2) = "mov" ++ regStr r1 ++ regStr r2
        show (AsmMovi i r) = "mov" ++ immStr i ++ regStr r
        show (AsmCmoveq i r) = "cmoveq" ++ immStr i ++ regStr r
        show (AsmCmovgeq i r) = "cmovgeq" ++ immStr i ++ regStr r
        show (AsmCmovgq i r) = "cmovgq" ++ immStr i ++ regStr r
        show (AsmCmovleq i r) = "cmovleq" ++ immStr i ++ regStr r
        show (AsmCmovlq i r) = "cmovlq" ++ immStr i ++ regStr r
        show (AsmCmovneq i r) = "cmovneq" ++ immStr i ++ regStr r
        show asm = undefined

ilocToAsm :: Iloc -> [Asm]
ilocToAsm (Mov r1 r2) = [AsmMov r1 r2]
ilocToAsm (Movi i r) = [AsmMovi i r]
ilocToAsm (Moveq i r) = [AsmCmoveq i r]
ilocToAsm (Movge i r) = [AsmCmovgeq i r]
ilocToAsm (Movgt i r) = [AsmCmovgq i r]
ilocToAsm (Movle i r) = [AsmCmovleq i r]
ilocToAsm (Movlt i r) = [AsmCmovlq i r]
ilocToAsm (Movne i r) = [AsmCmovneq i r]
ilocToAsm iloc = undefined
