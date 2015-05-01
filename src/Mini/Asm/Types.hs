{-# LANGUAGE DeriveDataTypeable #-}

module Mini.Asm.Types (Asm, programToAsm) where

import Data.Char
import Data.Data
import Data.List (intercalate)

import Mini.Iloc.Types
import Mini.CFG

data AsmArg = AsmReg AsmReg
            | AsmImmed Immed
            | AsmLabel Label
            deriving (Eq, Show)

data AsmReg = Rax
            | Rbx
            | Rcx
            | Rdx
            | Rsp
            | Rbp
            | Rsi
            | Rdi
            | R8
            | R9
            | R10
            | R11
            | R12
            | R13
            | R14
            | R15
            | RegNum Reg
            deriving (Eq, Data, Typeable)

instance Show AsmReg where
        show (RegNum i) = "r" ++ show i
        show reg = "%" ++ map toLower (show $ toConstr reg)

data Asm = AsmAdd
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
         | AsmMov AsmArg AsmReg
         | AsmCmoveq Immed AsmReg
         | AsmCmovgeq Immed AsmReg
         | AsmCmovgq Immed AsmReg
         | AsmCmovleq Immed AsmReg
         | AsmCmovlq Immed AsmReg
         | AsmCmovneq Immed AsmReg
         deriving (Eq)

programToAsm :: [NodeGraph] -> [Asm]
programToAsm graphs = undefined

functionToAsm :: NodeGraph -> [Asm]
functionToAsm (graph, hash) = undefined

wordSize :: Int
wordSize = 8

printLabel :: Label
printLabel = ".PRINT"

printlnLabel :: Label
printlnLabel = ".PRINTLN"

printf :: Label
printf = "printf"

scanVar :: Label
scanVar = ".SCANVAR"

scanLabel :: Label
scanLabel = ".SCAN"

scanf :: Label
scanf = "scanf"

free :: Label
free = "free"

malloc :: Label
malloc = "malloc"

argStr :: AsmArg -> String
argStr (AsmReg r) = regStr r
argStr (AsmImmed i) = immStr i
argStr (AsmLabel l) = labStr l

regStr :: AsmReg -> String
regStr r = " " ++ show r

labStr :: Label -> String
labStr l = " $" ++ l

immStr :: Immed -> String
immStr i = " $" ++ show i

showAsm :: String -> [String] -> String
showAsm name args = name ++ " " ++ intercalate ", " args

instance Show Asm where
        show (AsmCall l) = showAsm "call " [l]
        show AsmRet = showAsm "ret" []
        show (AsmMov r1 r2) = showAsm "movq" [argStr r1, regStr r2]
        show (AsmCmoveq i r) = showAsm "cmoveq" [immStr i, regStr r]
        show (AsmCmovgeq i r) = showAsm "cmovgeq" [immStr i, regStr r]
        show (AsmCmovgq i r) = showAsm "cmovgq" [immStr i, regStr r]
        show (AsmCmovleq i r) = showAsm "cmovleq" [immStr i, regStr r]
        show (AsmCmovlq i r) = showAsm "cmovlq" [immStr i, regStr r]
        show (AsmCmovneq i r) = showAsm "cmovneq" [immStr i, regStr r]

ilocToAsm :: Iloc -> [Asm]
ilocToAsm (Call l) = [AsmCall l]
ilocToAsm RetILOC = [AsmRet]
ilocToAsm (New i r) = createNew i r
ilocToAsm (Del r) = createDelete r
ilocToAsm (PrintILOC r) = createPrint r False
ilocToAsm (Println r) = createPrint r True
ilocToAsm (ReadILOC r) = createRead r
ilocToAsm (Mov r1 r2) = [AsmMov (AsmReg $ RegNum r1) (RegNum r2)]
ilocToAsm (Movi i r) = [AsmMov (AsmImmed i) (RegNum r)]
ilocToAsm (Moveq i r) = [AsmCmoveq i $ RegNum r]
ilocToAsm (Movge i r) = [AsmCmovgeq i $ RegNum r]
ilocToAsm (Movgt i r) = [AsmCmovgq i $ RegNum r]
ilocToAsm (Movle i r) = [AsmCmovleq i $ RegNum r]
ilocToAsm (Movlt i r) = [AsmCmovlq i $ RegNum r]
ilocToAsm (Movne i r) = [AsmCmovneq i $ RegNum r]
ilocToAsm iloc = error $ "No Asm translation for " ++ show iloc

createNew :: Immed -> Reg -> [Asm]
createNew words res = [ AsmMov (AsmImmed $ words * wordSize) Rdi
                      , AsmCall malloc
                      , AsmMov (AsmReg Rax) (RegNum res) ]

createDelete :: Reg -> [Asm]
createDelete r = [ AsmMov (AsmReg $ RegNum r) Rdi
                 , AsmCall free ]

createPrint :: Reg -> Bool -> [Asm]
createPrint r endl = [ AsmMov (AsmLabel printString) Rdi
                     , AsmMov (AsmReg $ RegNum r) Rsi
                     , AsmMov (AsmImmed 0) Rax
                     , AsmCall printf ]
    where printString = if endl then printlnLabel else printLabel

{- store memory location in rsi, then copy from memory to reg -}
createRead :: Reg -> [Asm]
createRead r = [ AsmMov (AsmLabel scanLabel) Rdi
               , AsmMov (AsmLabel scanVar) Rsi
               , AsmMov (AsmImmed 0) Rax
               , AsmCall scanf
               , AsmMov (AsmReg Rax) (RegNum r) ]
