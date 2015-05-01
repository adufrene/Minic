{-# LANGUAGE DeriveDataTypeable #-}

module Mini.Asm.Types where

import Data.Char
import Data.Data

import Mini.Iloc.Types

type AsmReg = Reg

data AsmReg = Rax
            | Rbx
            | Rcx
            | Rdx
            | Rsp
            | Rbp
            | Rsi
            | Rdi
            | RegNum Int
            deriving (Eq, Data, Typeable)

instance Show RealReg where
        show (RegNum i) = "%r" ++ show i
        show reg = "%" ++ map toLower (toConstr reg)

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
         | AsmCall Label
         | AsmRet
         | AsmMov AsmReg AsmReg
         | AsmMovm Label AsmReg
         | AsmMovi Immed AsmReg
         | AsmCMoveq Immed AsmReg
         | AsmcMovgeq Immed AsmReg
         | AsmCMovgq Immed AsmReg
         | AsmCmovleq Immed AsmReg
         | AsmCmovlq Immed AsmReg
         | AsmCmovneq Immed AsmReg
         deriving (Eq)

wordSize :: Int
wordSize = 8

printLabel :: Label
printLabel = ".PRINT"

printlnLabel :: Label
printlnLabel = ".PRINTLN"

printf :: Label
printf = "printf"

scanLabel :: Label
scanLabel = ".SCAN"

scanf :: Label
scanf = "scanf"

free :: Label
free = "free"

malloc :: Label
malloc = "malloc"

regStr :: AsmReg -> String
regStr r = " " ++ show r

labStr :: Label -> String
labStr l = " $" ++ l

immStr :: Immed -> String
immStr i = " $" ++ show i

instance Show Iloc where
        show (AsmCall l) = "call " ++ l
        show AsmRet = "ret"
        show (AsmMov r1 r2) = "movq" ++ regStr r1 ++ regStr r2
        show (AsmMovm l r) = "movq" ++ labStr l ++ regStr r
        show (AsmMovi i r) = "movq" ++ immStr i ++ regStr r
        show (AsmCmoveq i r) = "cmoveq" ++ immStr i ++ regStr r
        show (AsmCmovgeq i r) = "cmovgeq" ++ immStr i ++ regStr r
        show (AsmCmovgq i r) = "cmovgq" ++ immStr i ++ regStr r
        show (AsmCmovleq i r) = "cmovleq" ++ immStr i ++ regStr r
        show (AsmCmovlq i r) = "cmovlq" ++ immStr i ++ regStr r
        show (AsmCmovneq i r) = "cmovneq" ++ immStr i ++ regStr r
        show asm = undefined

ilocToAsm :: Iloc -> [Asm]
ilocToAsm (Call l) = [AsmCall l]
ilocToAsm RetILOC = [AsmRet]
ilocToAsm (New i r) = createNew i r
ilocToAsm (Del r) = createDelete r
ilocToAsm (PrintILOC r) = createPrint r False
ilocToAsm (Println r) = createPrint r True
ilocToAsm (ReadILOC r) = createRead r
ilocToAsm (Mov r1 r2) = [AsmMov (RegNum r1) (RegNum r2)]
ilocToAsm (Movi i r) = [AsmMovi i $ RegNum r]
ilocToAsm (Moveq i r) = [AsmCmoveq i $ RegNum r]
ilocToAsm (Movge i r) = [AsmCmovgeq i $ RegNum r]
ilocToAsm (Movgt i r) = [AsmCmovgq i $ RegNum r]
ilocToAsm (Movle i r) = [AsmCmovleq i $ RegNum r]
ilocToAsm (Movlt i r) = [AsmCmovlq i $ RegNum r]
ilocToAsm (Movne i r) = [AsmCmovneq i $ RegNum r]
ilocToAsm iloc = undefined

createNew :: Immed -> Reg
createNew words res = [ AsmMovi (words * wordSize) Rdi
                      , AsmCall malloc
                      , AsmMov Rax (RegNum res) ]

createDelete :: Reg -> [Asm]
createDelete r = [ AsmMov (RegNum r) Rdi
                 , AsmCall free ]

createPrint :: Reg -> Bool -> [Asm]
createPrint r endl = [ AsmMovm printString Rdi
                     , AsmMov (RegNum r) Rsi
                     , AsmMovi 0 Rax
                     , AsmCall printf ]
    where printString = if endl then printlnLabel else printLabel

{- store memory location in rsi, then copy from memory to reg -}
createRead :: Reg -> [Asm]
createRead r = undefined
