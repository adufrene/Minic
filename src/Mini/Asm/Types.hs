{-# LANGUAGE DeriveDataTypeable #-}

module Mini.Asm.Types (Asm, programToAsm) where

import Control.Applicative
import Data.Char
import Data.Data
import Data.Graph
import Data.HashMap.Strict ((!))
import Data.List (intercalate, elem)

import Mini.CFG
import Mini.Iloc.Types
import Mini.Types

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
            | Rip
            | RegNum Reg
            deriving (Eq, Data, Typeable)

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
        show (OffsetReg r (OffsetImm 0)) = show r
        show (OffsetReg r (OffsetImm i)) = show (wordSize * i) ++ "(" ++ show r ++ ")"
        show (OffsetReg r (OffsetLab l)) = l ++ "(" ++ show r ++ ")"

data Asm = AsmPush AsmReg
         | AsmPop AsmReg
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
         | AsmAdd AsmReg AsmReg
         | AsmDiv AsmReg
         | AsmMult AsmReg AsmReg
         | AsmMulti Immed AsmReg AsmReg
         | AsmSub AsmReg AsmReg
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
        show (AsmPush r) = showAsm "pushq" [asmRegStr r]
        show (AsmPop r) = showAsm "popq" [asmRegStr r]
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
        show (AsmAdd r1 r2) = showAsm "addq" [asmRegStr r1, asmRegStr r2]
        show (AsmDiv r) = showAsm "idivq" [asmRegStr r]
        show (AsmMult r1 r2) = showAsm "imulq" [asmRegStr r1, asmRegStr r2]
        show (AsmMulti i r1 r2) = showAsm "imulq" [immStr i, asmRegStr r1, asmRegStr r2]
        show (AsmSub r1 r2) = showAsm "subq" [asmRegStr r1, asmRegStr r2]
        show (AsmCmp arg r) = showAsm "cmp" [compArgStr arg, asmRegStr r]
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
        show (AsmLabel l) = l ++ ":"

data AsmType = FunctionType 
             | ObjectType
             deriving (Eq,Enum)

instance Show AsmType where
        show FunctionType = functionType
        show ObjectType = objectType

{- Create initial global variables and other file-specific data -}
programToAsm :: GlobalEnv -> Program -> [Asm]
programToAsm env prog = createGlobals ++ bodyAsm
    where graphs = createGraphs env prog
          createGlobals = concat [ globalString formatLabel formatStr
                                 , globalString printlnLabel printlnStr
                                 , [AsmGlobal scanVar]
                                 , createGlobal <$> getDeclarations prog]
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
                               , AsmMov (asmSReg Rsp) (asmDReg Rbp) ]
    where funLabel = funName (graph, hash) 

createEpilogue :: NodeGraph -> [Asm]
createEpilogue (graph, hash) = [ AsmMov (asmSReg Rbp) (asmDReg Rsp)
                               , AsmPop Rbp
                               , AsmRet
                               , AsmFunSize $ funName (graph, hash)]

functionType :: String
functionType = "@function"

objectType :: String
objectType = "@object"

asmReg :: AsmReg -> OffsetReg
asmReg r = OffsetReg r $ OffsetImm 0

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
srcStr (AsmSReg r) = show r
srcStr (AsmImmed i) = immStr i
srcStr (AsmSLabel l) = labStr l

destStr :: AsmDest -> String
destStr (AsmDReg r) = show r
destStr (AsmDLabel l) = labStr l

asmRegStr :: AsmReg -> String
asmRegStr = show . asmReg

labStr :: Label -> String
labStr l = "$" ++ l

immStr :: Immed -> String
immStr i = "$" ++ show i

compArgStr :: CompArg -> String
compArgStr (CompReg r) = asmRegStr r
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
ilocToAsm (Loadai r1 i r2) = [AsmMov (AsmSReg $ OffsetReg (RegNum r1) $ OffsetImm i) 
                                (asmDReg $ RegNum r2)]
ilocToAsm (Loadglobal l r) = [AsmMov (AsmSLabel l) (asmDReg $ RegNum r)]
ilocToAsm (Loadinargument l i r) = loadArg i r
ilocToAsm (Loadret r) = [AsmMov (asmSReg Rax) (asmDReg $ RegNum r)]
ilocToAsm (Storeai r1 r2 i) = [AsmMov (asmSReg $ RegNum r1) 
                                (AsmDReg $ OffsetReg (RegNum r2) $ OffsetImm i)] 
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

ilocToAsm (Add r1 r2 r3) = createAdd r1 r2 r3
ilocToAsm (Div r1 r2 r3) = createDiv r1 r2 r3
ilocToAsm (Mult r1 r2 r3) = createMult r1 r2 r3

createAdd :: Reg -> Reg -> Reg -> [Asm]
createAdd r1 r2 r3 = [ AsmMov (asmSReg $ RegNum r1) (asmDReg $ RegNum r3)
                     , AsmAdd (RegNum r2) (RegNum r3) ]

createDiv :: Reg -> Reg -> Reg -> [Asm]
createDiv r1 r2 r3 = [ AsmMov (AsmImmed 0) (asmDReg Rdx)
                     , AsmMov (asmSReg $ RegNum r1) (asmDReg Rax)
                     , AsmDiv (RegNum r2)
                     , AsmMov (asmSReg Rax) (asmDReg $ RegNum r3) ]

createMult :: Reg -> Reg -> Reg -> [Asm]
createMult r1 r2 r3 = [ AsmMov (asmSReg $ RegNum r1) (asmDReg $ RegNum r3)
                      , AsmMult (RegNum r2) (RegNum r3) ]

createSub :: Reg -> Reg -> Reg -> [Asm]
createSub r1 r2 r3 = [ AsmMov (asmSReg $ RegNum r1) (asmDReg $ RegNum r3)
                     , AsmSub (RegNum r2) (RegNum r3) ]

brz :: Reg -> Label -> Label -> [Asm]
brz r l1 l2 = [ AsmCmp (CompImm 0) (RegNum r)
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
    where printString = if endl then printlnLabel else formatLabel

createRead :: Reg -> [Asm]
createRead r = [ AsmMov (AsmSLabel formatLabel) (asmDReg Rdi)
               , AsmMov (AsmSLabel scanVar) (asmDReg Rsi)
               , AsmMov (AsmImmed 0) (asmDReg Rax)
               , AsmCall scanf
               , AsmMov (AsmSReg $ OffsetReg Rip $ OffsetLab scanVar) 
                        (asmDReg $ RegNum r) ]
--                , AsmMov (asmSReg Rax) (asmDReg $ RegNum r) ]
