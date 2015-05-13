{-# LANGUAGE DeriveDataTypeable #-}

module Mini.Asm.Types 
        ( Asm
        , programToAsm
        , AsmReg (..)
        ) where

import Control.Applicative
import Data.Char
import Data.Data
import Data.Graph
import Data.HashMap.Strict ((!))
import Data.List (intercalate, elem)

import Mini.CFG
import Mini.Iloc.Types
import Mini.Types

data AsmSrc = AsmSOReg OffsetReg
            | AsmSReg AsmReg
            | AsmImmed Immed
            | AsmSLabel Label
            deriving (Eq)

data AsmDest = AsmDOReg OffsetReg
             | AsmDReg AsmReg
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
            | Rip
            | RegNum Reg
            deriving (Eq, Data, Typeable, Ord)

data OffsetArg = OffsetImm Immed
               | OffsetLab Label
               deriving (Eq)

instance Show OffsetArg where
        show (OffsetLab l) = l
        show (OffsetImm i) = show i

data OffsetReg = OffsetReg AsmReg OffsetArg deriving (Eq)

instance Show AsmReg where
        show (RegNum i) = "r" ++ show i
        show reg = "%" ++ map toLower (show $ toConstr reg)

instance Show OffsetReg where
        show (OffsetReg r (OffsetImm 0)) = "(" ++ show r ++ ")"
        show (OffsetReg r (OffsetImm i)) = show (wordSize * i) ++ "(" ++ show r ++ ")"
        show (OffsetReg r (OffsetLab l)) = l ++ "(" ++ show r ++ ")"

data Asm = AsmPush AsmReg
         | AsmPop AsmReg
         | AsmShift Immed AsmReg
         | AsmSection
         | AsmText
         | AsmData
         | AsmAlign
         | AsmString String
         | AsmVarSize Label Immed
         | AsmFunSize Label
         | AsmQuad Immed
         | AsmGlobal Label
         | AsmFunGlobal Label
         | AsmType Label AsmType
         | AsmAdd AsmReg CompArg
         | AsmDiv AsmReg
         | AsmMult AsmReg AsmReg
         | AsmMulti Immed AsmReg AsmReg
         | AsmSub AsmReg CompArg
         | AsmCmp CompArg AsmReg
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
         | AsmLabel Label
         deriving (Eq)

instance Show Asm where
        show (AsmPush r) = showAsm "pushq" [show r]
        show (AsmPop r) = showAsm "popq" [show r]
        show (AsmShift i r) = showAsm "sarq" [immStr i, show r]
        show AsmSection = "\t.section\t\t.rodata"
        show AsmText = "\t.text"
        show AsmData = "\t.data"
        show AsmAlign = "\t.align 8"
        show (AsmString s) = "\t.string\t\"" ++ s ++ "\""
        show (AsmVarSize l i) = showAsm ".size" [l, show i]
        show (AsmQuad i) = "\t.quad\t" ++ show i
        show (AsmGlobal l) = "\t.comm " ++ l ++ ",8,8"
        show (AsmFunGlobal l) = ".global " ++ l
        show (AsmType l t) = showAsm ".type" [l, show t]
        show (AsmFunSize l) = showAsm ".size" [l, ".-" ++ l]
        show (AsmAdd r arg) = showAsm "addq" [show r, compArgStr arg]
        show (AsmDiv r) = showAsm "idivq" [show r]
        show (AsmMult r1 r2) = showAsm "imulq" [show r1, show r2]
        show (AsmMulti i r1 r2) = showAsm "imulq" [immStr i, show r1, show r2]
        show (AsmSub r arg) = showAsm "subq" [show r, compArgStr arg]
        show (AsmCmp arg r) = showAsm "cmp " [compArgStr arg, show r]
        show (AsmJe l) = showAsm "je  " [l]
        show (AsmJmp l) = showAsm "jmp " [l]
        show (AsmCall l) = showAsm "call" [l]
        show AsmRet = showAsm "ret " []
        show (AsmMov r1 r2) = showAsm "movq" [srcStr r1, destStr r2]
        show (AsmCmoveq i r) = showAsm "cmoveq" [immStr i, show r]
        show (AsmCmovgeq i r) = showAsm "cmovgeq" [immStr i, show r]
        show (AsmCmovgq i r) = showAsm "cmovgq" [immStr i, show r]
        show (AsmCmovleq i r) = showAsm "cmovleq" [immStr i, show r]
        show (AsmCmovlq i r) = showAsm "cmovlq" [immStr i, show r]
        show (AsmCmovneq i r) = showAsm "cmovneq" [immStr i, show r]
        show (AsmLabel l) = l ++ ":"

data AsmType = FunctionType 
             | ObjectType
             deriving (Eq,Enum)

instance Show AsmType where
        show FunctionType = functionType
        show ObjectType = objectType

{- Create initial global variables and other file-specific data -}
programToAsm :: [NodeGraph] -> [Declaration] -> [Asm]
programToAsm graphs globals = createGlobals ++ bodyAsm
    where createGlobals = concat [ globalString formatLabel formatStr
                                 , globalString printlnLabel printlnStr
                                 , [AsmGlobal scanVar]
                                 , createGlobal <$> globals ]
          bodyAsm = concat $ functionToAsm <$> graphs
          createGlobal = AsmGlobal . getDecId 

globalString :: Label -> String -> [Asm]
globalString l s = [ AsmSection
                   , AsmLabel l
                   , AsmString s ]

{- Create Function prologue to start -}
{- http://users.csc.calpoly.edu/~akeen/courses/csc431/handouts/references/asm.pdf -}
functionToAsm :: NodeGraph -> [Asm]
functionToAsm nodeG@(graph, hash) = prologue ++ body ++ epilogue
    where prologue = createPrologue nodeG
          body = concat $ mapFun <$> sortedVerts -- Strip off label
          epilogue = createEpilogue nodeG
          sortedVerts = topSort graph
          sv = startVert nodeG
          mapFun x
            | x == entryVertex = []
            | x == exitVertex = labelInsn
            | otherwise = labelInsn ++ concat (ilocToAsm <$> getIloc node)
            where node = hash ! x
                  labelInsn = if x == sv then [] else [AsmLabel $ getLabel node]

createPrologue :: NodeGraph -> [Asm]
createPrologue (graph, hash) = [ AsmText
                               , AsmFunGlobal funLabel
                               , AsmType funLabel FunctionType
                               , AsmLabel funLabel
                               , AsmPush Rbp
                               , AsmMov (AsmSReg Rsp) (AsmDReg Rbp) ]
    where funLabel = funName (graph, hash) 

createEpilogue :: NodeGraph -> [Asm]
createEpilogue (graph, hash) = [ AsmMov (AsmSReg Rbp) (AsmDReg Rsp)
                               , AsmPop Rbp
                               , AsmRet
                               , AsmFunSize $ funName (graph, hash)]

functionType :: String
functionType = "@function"

objectType :: String
objectType = "@object"

numArgRegs :: Int
numArgRegs = length argRegs

argRegs :: [AsmReg]
argRegs = [Rdi, Rsi, Rdx, Rcx, R8, R9]

wordSize :: Int
wordSize = 8

formatLabel :: Label
formatLabel = ".FORMAT"

formatStr :: String
formatStr = "%ld"

printlnStr :: String
printlnStr = "%ld\\n"

printlnLabel :: Label
printlnLabel = ".PRINTLN"

printf :: Label
printf = "printf"

scanVar :: Label
scanVar = ".SCANVAR"

scanf :: Label
scanf = "scanf"

free :: Label
free = "free"

malloc :: Label
malloc = "malloc"

funName :: NodeGraph -> Label
funName ng@(_, hash) = getLabel $ hash ! startVert ng

startVert :: NodeGraph -> Vertex
startVert = snd . head . filter ((==entryVertex) . fst) . edges . fst

srcStr :: AsmSrc -> String
srcStr (AsmSOReg r) = show r
srcStr (AsmImmed i) = immStr i
srcStr (AsmSLabel l) = labStr l
srcStr (AsmSReg r) = show r

destStr :: AsmDest -> String
destStr (AsmDOReg r) = show r
destStr (AsmDLabel l) = labStr l
destStr (AsmDReg r) = show r

labStr :: Label -> String
labStr l = "$" ++ l

immStr :: Immed -> String
immStr i = "$" ++ show i

compArgStr :: CompArg -> String
compArgStr (CompReg r) = show r
compArgStr (CompImm i) = immStr i

showAsm :: String -> [String] -> String
showAsm name args = "\t" ++ name ++ "\t" ++ intercalate ", " args

ilocToAsm :: Iloc -> [Asm]
ilocToAsm (Add r1 r2 r3) = createAdd r1 r2 r3
ilocToAsm (Div r1 r2 r3) = createDiv r1 r2 r3
ilocToAsm (Mult r1 r2 r3) = createMult r1 r2 r3
ilocToAsm (Multi r1 i r2) = [ AsmMulti i (RegNum r1) (RegNum r2) ]
ilocToAsm (Sub r1 r2 r3) = createSub r1 r2 r3
ilocToAsm (Comp r1 r2) = [AsmCmp (CompReg $ RegNum r2) (RegNum r1)]
ilocToAsm (Compi r i) = [AsmCmp (CompImm i) (RegNum r)]
ilocToAsm (Jumpi l) = [AsmJmp l]
ilocToAsm (Brz r l1 l2) = brz r l1 l2
ilocToAsm (Loadai r1 i r2) = [AsmMov (AsmSOReg $ OffsetReg (RegNum r1) $ OffsetImm i) 
                                (AsmDReg $ RegNum r2)]
ilocToAsm (Loadglobal l r) = [AsmMov (AsmSLabel l) (AsmDReg $ RegNum r)]
ilocToAsm (Loadinargument l i r) = loadArg i r
ilocToAsm (Loadret r) = [AsmMov (AsmSReg Rax) (AsmDReg $ RegNum r)]
ilocToAsm (Storeai r1 r2 i) = [AsmMov (AsmSReg $ RegNum r1) 
                                (AsmDOReg $ OffsetReg (RegNum r2) $ OffsetImm i)] 
ilocToAsm (Storeglobal r l) = [AsmMov (AsmSReg $ RegNum r) (AsmDLabel l)]
ilocToAsm (Storeoutargument r i) = storeArg r i
ilocToAsm (Storeret r) = [AsmMov (AsmSReg $ RegNum r) (AsmDReg Rax)]
ilocToAsm (Call l) = [AsmCall l]
ilocToAsm RetILOC = [AsmRet]
ilocToAsm (New i r) = createNew i r
ilocToAsm (Del r) = createDelete r
ilocToAsm (PrintILOC r) = createPrint r False
ilocToAsm (Println r) = createPrint r True
ilocToAsm (ReadILOC r) = createRead r
ilocToAsm (Mov r1 r2) = [AsmMov (AsmSReg $ RegNum r1) 
                            (AsmDReg $ RegNum r2)]
ilocToAsm (Movi i r) = [AsmMov (AsmImmed i) (AsmDReg $ RegNum r)]
ilocToAsm (Moveq i r) = [AsmCmoveq i $ RegNum r]
ilocToAsm (Movge i r) = [AsmCmovgeq i $ RegNum r]
ilocToAsm (Movgt i r) = [AsmCmovgq i $ RegNum r]
ilocToAsm (Movle i r) = [AsmCmovleq i $ RegNum r]
ilocToAsm (Movlt i r) = [AsmCmovlq i $ RegNum r]
ilocToAsm (Movne i r) = [AsmCmovneq i $ RegNum r]
ilocToAsm (PrepArgs i) = [AsmSub Rsp $ CompImm $ wordSize * (i - numArgRegs) | i > numArgRegs]
ilocToAsm (UnprepArgs i) = [AsmAdd Rsp $ CompImm $ wordSize * (i - numArgRegs) | i > numArgRegs]
ilocToAsm iloc = error $ "No Asm translation for " ++ show iloc

createAdd :: Reg -> Reg -> Reg -> [Asm]
createAdd r1 r2 r3 = [ AsmMov (AsmSReg $ RegNum r1) (AsmDReg $ RegNum r3)
                     , AsmAdd (RegNum r2) (CompReg $ RegNum r3) ]

createDiv :: Reg -> Reg -> Reg -> [Asm]
createDiv r1 r2 r3 = [ AsmMov (AsmSReg $ RegNum r1) (AsmDReg Rdx)
                     , AsmShift 63 Rdx
                     , AsmMov (AsmSReg $ RegNum r1) (AsmDReg Rax)
                     , AsmDiv (RegNum r2)
                     , AsmMov (AsmSReg Rax) (AsmDReg $ RegNum r3) ]

createMult :: Reg -> Reg -> Reg -> [Asm]
createMult r1 r2 r3 = [ AsmMov (AsmSReg $ RegNum r1) (AsmDReg $ RegNum r3)
                      , AsmMult (RegNum r2) (RegNum r3) ]

createSub :: Reg -> Reg -> Reg -> [Asm]
createSub r1 r2 r3 = [ AsmMov (AsmSReg $ RegNum r1) (AsmDReg $ RegNum r3)
                     , AsmSub (RegNum r2) (CompReg $ RegNum r3) ]

brz :: Reg -> Label -> Label -> [Asm]
brz r l1 l2 = [ AsmCmp (CompImm 0) (RegNum r)
              , AsmJe l1
              , AsmJmp l2 ]

loadArg :: Immed -> Reg -> [Asm]
loadArg i r
    | i < numArgRegs = [AsmMov (AsmSReg $ argRegs !! i) (AsmDReg $ RegNum r)]
    | otherwise = [AsmMov (AsmSOReg $ OffsetReg Rbp $ OffsetImm offset) 
                    (AsmDReg $ RegNum r)]
    where offset = i - numArgRegs + 2

storeArg :: Reg -> Immed -> [Asm]
storeArg r i
    | i < numArgRegs = [AsmMov (AsmSReg $ RegNum r) (AsmDReg $ argRegs !! i)] 
    | otherwise = [AsmMov (AsmSReg $ RegNum r) (AsmDOReg $ OffsetReg Rsp 
                    $ OffsetImm offset)]
    where offset = i - numArgRegs

createNew :: Immed -> Reg -> [Asm]
createNew words res = [ AsmMov (AsmImmed $ words * wordSize) (AsmDReg Rdi)
                      , AsmCall malloc
                      , AsmMov (AsmSReg Rax) (AsmDReg $ RegNum res) ]

createDelete :: Reg -> [Asm]
createDelete r = [ AsmMov (AsmSReg $ RegNum r) (AsmDReg Rdi)
                 , AsmCall free ]

createPrint :: Reg -> Bool -> [Asm]
createPrint r endl = [ AsmMov (AsmSLabel printString) (AsmDReg Rdi)
                     , AsmMov (AsmSReg $ RegNum r) (AsmDReg Rsi)
                     , AsmMov (AsmImmed 0) (AsmDReg Rax)
                     , AsmCall printf ]
    where printString = if endl then printlnLabel else formatLabel

createRead :: Reg -> [Asm]
createRead r = [ AsmMov (AsmSLabel formatLabel) (AsmDReg Rdi)
               , AsmMov (AsmSLabel scanVar) (AsmDReg Rsi)
               , AsmMov (AsmImmed 0) (AsmDReg Rax)
               , AsmCall scanf
               , AsmMov (AsmSOReg $ OffsetReg Rip $ OffsetLab scanVar) 
                        (AsmDReg $ RegNum r) ]
--                , AsmMov (AsmSReg Rax) (AsmDReg $ RegNum r) ]
