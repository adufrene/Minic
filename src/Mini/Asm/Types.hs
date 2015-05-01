{-# LANGUAGE DeriveDataTypeable #-}

module Mini.Asm.Types (Asm, programToAsm) where

import Data.Char
import Data.Data
import Data.List (intercalate)

import Mini.Iloc.Types
import Mini.CFG

data AsmSrc = AsmSReg OffsetReg
            | AsmImmed Immed
            | AsmSLabel Label
            deriving (Eq)

data AsmDest = AsmDReg OffsetReg
             | AsmDLabel Label
             deriving (Eq)

data CompArg = CompReg AsmReg
             | CompImm Immed 
             deriving (Eq)

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

data OffsetReg = OffsetReg AsmReg Int deriving (Eq)

instance Show AsmReg where
        show (RegNum i) = "r" ++ show i
        show reg = "%" ++ map toLower (show $ toConstr reg)

instance Show OffsetReg where
        show (OffsetReg r i)
            | i /= 0 = show i ++ "(" ++ reg ++ ")"
            | otherwise = reg 
            where reg = show r

data Asm = AsmAdd
         | AsmDiv
         | AsmMult
         | AsmMulti
         | AsmSub
         | AsmComp
         | AsmCompi
         | AsmBrz
         | AsmCmp AsmReg CompArg
         | AsmJe Label
         | AsmJmp Label
         | AsmCall Label
         | AsmRet
         | AsmMov AsmSrc AsmDest
         | AsmCmoveq Immed AsmReg
         | AsmCmovgeq Immed AsmReg
         | AsmCmovgq Immed AsmReg
         | AsmCmovleq Immed AsmReg
         | AsmCmovlq Immed AsmReg
         | AsmCmovneq Immed AsmReg
         deriving (Eq)

{- Create initial global variables and other file-specific data -}
{- Tail Recursion!!!! -}
programToAsm :: [NodeGraph] -> [Asm]
programToAsm graphs = undefined

{- Create Function prologue to start -}
{- http://users.csc.calpoly.edu/~akeen/courses/csc431/handouts/references/asm.pdf -}
functionToAsm :: NodeGraph -> [Asm]
functionToAsm (graph, hash) = undefined

asmReg :: AsmReg -> OffsetReg
asmReg r = OffsetReg r 0

asmSReg :: AsmReg -> AsmSrc
asmSReg = AsmSReg . asmReg

asmDReg :: AsmReg -> AsmDest
asmDReg = AsmDReg . asmReg

numArgRegs :: Int
numArgRegs = length argRegs

argRegs :: [AsmReg]
argRegs = [Rdi, Rsi, Rdx, Rcx, R8, R9]

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

srcStr :: AsmSrc -> String
srcStr (AsmSReg r) = regStr r
srcStr (AsmImmed i) = immStr i
srcStr (AsmSLabel l) = labStr l

destStr :: AsmDest -> String
destStr (AsmDReg r) = regStr r
destStr (AsmDLabel l) = labStr l

regStr :: OffsetReg -> String
regStr r = " " ++ show r

asmRegStr :: AsmReg -> String
asmRegStr = regStr . asmReg

labStr :: Label -> String
labStr l = " $" ++ l

immStr :: Immed -> String
immStr i = " $" ++ show i

compArgStr :: CompArg -> String
compArgStr (CompReg r) = asmRegStr r
compArgStr (CompImm i) = immStr i

showAsm :: String -> [String] -> String
showAsm name args = name ++ " " ++ intercalate ", " args

instance Show Asm where
        show (AsmCmp r arg) = showAsm "cmp" [asmRegStr r, compArgStr arg]
        show (AsmJe l) = showAsm "je" [l]
        show (AsmJmp l) = showAsm "jmp" [l]
        show (AsmCall l) = showAsm "call" [l]
        show AsmRet = showAsm "ret" []
        show (AsmMov r1 r2) = showAsm "movq" [srcStr r1, destStr r2]
        show (AsmCmoveq i r) = showAsm "cmoveq" [immStr i, asmRegStr r]
        show (AsmCmovgeq i r) = showAsm "cmovgeq" [immStr i, asmRegStr r]
        show (AsmCmovgq i r) = showAsm "cmovgq" [immStr i, asmRegStr r]
        show (AsmCmovleq i r) = showAsm "cmovleq" [immStr i, asmRegStr r]
        show (AsmCmovlq i r) = showAsm "cmovlq" [immStr i, asmRegStr r]
        show (AsmCmovneq i r) = showAsm "cmovneq" [immStr i, asmRegStr r]

ilocToAsm :: Iloc -> [Asm]
ilocToAsm (Comp r1 r2) = [AsmCmp (RegNum r1) (CompReg $ RegNum r2)]
ilocToAsm (Compi r i) = [AsmCmp (RegNum r) (CompImm i)]
ilocToAsm (Brz r l1 l2) = brz r l1 l2
ilocToAsm (Loadai r1 i r2) = [AsmMov (AsmSReg $ OffsetReg (RegNum r1) i) 
                                (asmDReg $ RegNum r2)]
ilocToAsm (Loadglobal l r) = [AsmMov (AsmSLabel l) (asmDReg $ RegNum r)]
ilocToAsm (Loadinargument l i r) = loadArg i r
ilocToAsm (Loadret r) = [AsmMov (asmSReg Rax) (asmDReg $ RegNum r)]
ilocToAsm (Storeai r1 r2 i) = [AsmMov (asmSReg $ RegNum r1) 
                                (AsmDReg $ OffsetReg (RegNum r2) i)] 
ilocToAsm (Storeglobal r l) = [AsmMov (asmSReg $ RegNum r) (AsmDLabel l)]
ilocToAsm (Storeoutargument r i) = storeArg r i
ilocToAsm (Storeret r) = [AsmMov (asmSReg $ RegNum r) (asmDReg Rax)]
ilocToAsm (Call l) = [AsmCall l]
ilocToAsm RetILOC = [AsmRet]
ilocToAsm (New i r) = createNew i r
ilocToAsm (Del r) = createDelete r
ilocToAsm (PrintILOC r) = createPrint r False
ilocToAsm (Println r) = createPrint r True
ilocToAsm (ReadILOC r) = createRead r
ilocToAsm (Mov r1 r2) = [AsmMov (asmSReg $ RegNum r1) 
                            (asmDReg $ RegNum r2)]
ilocToAsm (Movi i r) = [AsmMov (AsmImmed i) (asmDReg $ RegNum r)]
ilocToAsm (Moveq i r) = [AsmCmoveq i $ RegNum r]
ilocToAsm (Movge i r) = [AsmCmovgeq i $ RegNum r]
ilocToAsm (Movgt i r) = [AsmCmovgq i $ RegNum r]
ilocToAsm (Movle i r) = [AsmCmovleq i $ RegNum r]
ilocToAsm (Movlt i r) = [AsmCmovlq i $ RegNum r]
ilocToAsm (Movne i r) = [AsmCmovneq i $ RegNum r]
ilocToAsm iloc = error $ "No Asm translation for " ++ show iloc

brz :: Reg -> Label -> Label -> [Asm]
brz r l1 l2 = [ AsmCmp (RegNum r) (CompImm 0)
              , AsmJe l1
              , AsmJmp l2 ]

loadArg :: Immed -> Reg -> [Asm]
loadArg i r
    | i < numArgRegs = [AsmMov (asmSReg $ argRegs !! i) (asmDReg $ RegNum r)]
    | otherwise = undefined

storeArg :: Reg -> Immed -> [Asm]
storeArg r i
    | i < numArgRegs = [AsmMov (asmSReg $ RegNum r) (asmDReg $ argRegs !! i)] 
    | otherwise = undefined {- need to push in reverse order n->6 -}

createNew :: Immed -> Reg -> [Asm]
createNew words res = [ AsmMov (AsmImmed $ words * wordSize) (asmDReg Rdi)
                      , AsmCall malloc
                      , AsmMov (asmSReg Rax) (asmDReg $ RegNum res) ]

createDelete :: Reg -> [Asm]
createDelete r = [ AsmMov (asmSReg $ RegNum r) (asmDReg Rdi)
                 , AsmCall free ]

createPrint :: Reg -> Bool -> [Asm]
createPrint r endl = [ AsmMov (AsmSLabel printString) (asmDReg Rdi)
                     , AsmMov (asmSReg $ RegNum r) (asmDReg Rsi)
                     , AsmMov (AsmImmed 0) (asmDReg Rax)
                     , AsmCall printf ]
    where printString = if endl then printlnLabel else printLabel

createRead :: Reg -> [Asm]
createRead r = [ AsmMov (AsmSLabel scanLabel) (asmDReg Rdi)
               , AsmMov (AsmSLabel scanVar) (asmDReg Rsi)
               , AsmMov (AsmImmed 0) (asmDReg Rax)
               , AsmCall scanf
               , AsmMov (asmSReg Rax) (asmDReg $ RegNum r) ]
