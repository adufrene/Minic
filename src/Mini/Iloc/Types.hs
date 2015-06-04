module Mini.Iloc.Types where
{-|

This file contains the data definitions for our internal ILOC.
It is derivied from the instructor provided ILOC handout ("iloc.pdf").
Loadi is replaced with movi.
Adds a new instruction - brz

-}

import Data.List
import Data.HashMap.Strict hiding (map)

import Mini.Graph
import Mini.Types

type RegHash = HashMap Id Reg
type Baggage = (GlobalEnv, LocalEnv, RegHash)
type IlocRet = ([Iloc], Reg)

type IlocGraph = NodeGraph Iloc
type IlocNode = Node Iloc

type Reg = Int
type Immed = Int
type Label = String

retReg :: Reg
retReg = 1

ccReg :: Reg
ccReg = -1

data Iloc = Add Reg Reg Reg
          | Div Reg Reg Reg
          | Mult Reg Reg Reg
          | Multi Reg Immed Reg
          | Sub Reg Reg Reg
          | Comp Reg Reg
          | Compi Reg Immed
          | Jumpi Label
          | Brz Reg Label Label
          | Loadai Reg Immed Reg
          | Loadglobal Id Reg
          | Loadinargument Id Immed Reg
          | Loadret Reg
          | Storeai Reg Reg Immed
          | Storeglobal Reg Id
          | Storeoutargument Reg Immed
          | Storeret Reg
          | Call Label
          | RetILOC
          | New Immed Reg
          | Del Reg
          | PrintILOC Reg
          | Println Reg
          | ReadILOC Reg
          | Mov Reg Reg
          | Movi Immed Reg
          | Moveq Reg Reg
          | Movge Reg Reg
          | Movgt Reg Reg
          | Movle Reg Reg
          | Movlt Reg Reg
          | Movne Reg Reg
          | PrepArgs Immed
          | UnprepArgs Immed
          deriving (Eq, Ord)

condCodeReg :: String
condCodeReg = "ccr"

showReg :: Reg -> String
showReg regNum = "r" ++ show regNum

showIlocHelper :: String -> [String] -> String
showIlocHelper name args = name ++ " " ++ intercalate ", " args

instance Show Iloc where
   show (Add r1 r2 r3) = showIlocHelper "add" $ map showReg [r1, r2, r3]
   show (Div r1 r2 r3) = showIlocHelper "div" $ map showReg [r1, r2, r3]
   show (Mult r1 r2 r3) = showIlocHelper "mult" $ map showReg [r1, r2, r3]
   show (Multi r1 i1 r2) = showIlocHelper "multi" [showReg r1, show i1, showReg r2]
   show (Sub r1 r2 r3) = showIlocHelper "sub" $ map showReg [r1, r2, r3]
   show (Comp r1 r2) = showIlocHelper "comp" [showReg r1, showReg r2, condCodeReg]
   show (Compi r1 i1) = showIlocHelper "compi" [showReg r1, show i1, condCodeReg]
   show (Jumpi l1) = showIlocHelper "jumpi" [l1]
   show (Brz r1 l1 l2) = showIlocHelper "brz" [showReg r1, l1, l2]
   show (Loadai r1 i1 r2) = showIlocHelper "loadai" [showReg r1, show i1, showReg r2]
   show (Loadglobal s1 r1) = showIlocHelper "loadglobal" [s1, showReg r1]
   show (Loadinargument s1 i1 r1) = showIlocHelper "loadinargument" [s1, show i1, showReg r1]
   show (Loadret r1) = showIlocHelper "loadret" [showReg r1]
   show (Storeai r1 r2 i1) = showIlocHelper "storeai" [showReg r1, showReg r2, show i1]
   show (Storeglobal r1 s1) = showIlocHelper "storeglobal" [showReg r1, s1]
   show (Storeoutargument r1 i1) = showIlocHelper "storeoutargument" [showReg r1, show i1]
   show (Storeret r1) = showIlocHelper "storeret" [showReg r1]
   show (Call l1) = showIlocHelper "call" [l1]
   show RetILOC = "ret"
   show (New i1 r1) = showIlocHelper "new" [show i1, showReg r1]
   show (Del r1) = showIlocHelper "del" [showReg r1]
   show (PrintILOC r1) = showIlocHelper "print" [showReg r1]
   show (Println r1) = showIlocHelper "println" [showReg r1]
   show (ReadILOC r1) = showIlocHelper "read" [showReg r1]
   show (Mov r1 r2) = showIlocHelper "mov" [showReg r1, showReg r2]
   show (Movi i1 r1) = showIlocHelper "movi" [show i1, showReg r1]
   show (Moveq r1 r2) = showIlocHelper "moveq" [showReg r1, showReg r2]
   show (Movge r1 r2) = showIlocHelper "movge" [showReg r1, showReg r2]
   show (Movgt r1 r2) = showIlocHelper "movgt" [showReg r1, showReg r2]
   show (Movle r1 r2) = showIlocHelper "movle" [showReg r1, showReg r2]
   show (Movlt r1 r2) = showIlocHelper "movlt" [showReg r1, showReg r2]
   show (Movne r1 r2) = showIlocHelper "movne" [showReg r1, showReg r2]
   show (PrepArgs i) = showIlocHelper "prepArgs" [show i]
   show (UnprepArgs i) = showIlocHelper "unprepArgs" [show i]

-- applies given func to each register in an ILOC and returns resulting iloc
mapToRegs :: Iloc -> (Reg -> Reg) -> Iloc
mapToRegs (Add r1 r2 r3) f = Add (f r1) (f r2) (f r3)
mapToRegs (Div r1 r2 r3) f = Div (f r1) (f r2) (f r3)
mapToRegs (Mult r1 r2 r3) f = Mult (f r1) (f r2) (f r3)
mapToRegs (Multi r1 i1 r2) f = Multi (f r1) i1 (f r2)
mapToRegs (Sub r1 r2 r3) f = Sub (f r1) (f r2) (f r3)
mapToRegs (Comp r1 r2) f = Comp (f r1) (f r2)
mapToRegs (Compi r1 i1) f = Compi (f r1) i1
mapToRegs (Jumpi l1) _ = Jumpi l1
mapToRegs (Brz r1 l1 l2) f = Brz (f r1) l1 l2
mapToRegs (Loadai r1 i1 r2) f = Loadai (f r1) i1 (f r2)
mapToRegs (Loadglobal s1 r1) f = Loadglobal s1 (f r1)
mapToRegs (Loadinargument s1 i1 r1) f = Loadinargument s1 i1 (f r1)
mapToRegs (Loadret r1) f = Loadret (f r1)
mapToRegs (Storeai r1 r2 i1) f = Storeai (f r1) (f r2) i1
mapToRegs (Storeglobal r1 s1) f = Storeglobal (f r1) s1
mapToRegs (Storeoutargument r1 i1) f = Storeoutargument (f r1) i1
mapToRegs (Storeret r1) f = Storeret (f r1)
mapToRegs (Call l1) _ = Call l1
mapToRegs RetILOC _ = RetILOC
mapToRegs (New i1 r1) f = New i1 (f r1)
mapToRegs (Del r1) f = Del (f r1)
mapToRegs (PrintILOC r1) f = PrintILOC (f r1)
mapToRegs (Println r1) f = Println (f r1)
mapToRegs (ReadILOC r1) f = ReadILOC (f r1)
mapToRegs (Mov r1 r2) f = Mov (f r1) (f r2)
mapToRegs (Movi i1 r1) f = Movi i1 (f r1)
mapToRegs (Moveq r1 r2) f = Moveq (f r1) (f r2)
mapToRegs (Movge r1 r2) f = Movge (f r1) (f r2)
mapToRegs (Movgt r1 r2) f = Movgt (f r1) (f r2)
mapToRegs (Movle r1 r2) f = Movle (f r1) (f r2)
mapToRegs (Movlt r1 r2) f = Movlt (f r1) (f r2)
mapToRegs (Movne r1 r2) f = Movne (f r1) (f r2)
mapToRegs (PrepArgs i) _ = PrepArgs i
mapToRegs (UnprepArgs i) _ = UnprepArgs i

-- applies given func to each source register in an ILOC and returns resulting iloc
mapToSrcRegs :: Iloc -> (Reg -> Reg) -> Iloc
mapToSrcRegs (Add r1 r2 r3) f = Add (f r1) (f r2) r3
mapToSrcRegs (Div r1 r2 r3) f = Div (f r1) (f r2) r3
mapToSrcRegs (Mult r1 r2 r3) f = Mult (f r1) (f r2) r3
mapToSrcRegs (Multi r1 i1 r2) f = Multi (f r1) i1 r2
mapToSrcRegs (Sub r1 r2 r3) f = Sub (f r1) (f r2) r3
mapToSrcRegs (Comp r1 r2) f = Comp (f r1) (f r2)
mapToSrcRegs (Compi r1 i1) f = Compi (f r1) i1
mapToSrcRegs (Jumpi l1) _ = Jumpi l1
mapToSrcRegs (Brz r1 l1 l2) f = Brz (f r1) l1 l2
mapToSrcRegs (Loadai r1 i1 r2) f = Loadai (f r1) i1 r2
mapToSrcRegs (Loadglobal s1 r1) _ = Loadglobal s1 r1
mapToSrcRegs (Loadinargument s1 i1 r1) _ = Loadinargument s1 i1 r1
mapToSrcRegs (Loadret r1) _ = Loadret r1
mapToSrcRegs (Storeai r1 r2 i1) f = Storeai (f r1) (f r2) i1
mapToSrcRegs (Storeglobal r1 s1) f = Storeglobal (f r1) s1
mapToSrcRegs (Storeoutargument r1 i1) f = Storeoutargument (f r1) i1
mapToSrcRegs (Storeret r1) f = Storeret (f r1)
mapToSrcRegs (Call l1) _ = Call l1
mapToSrcRegs RetILOC _ = RetILOC
mapToSrcRegs (New i1 r1) _ = New i1 r1
mapToSrcRegs (Del r1) f = Del (f r1)
mapToSrcRegs (PrintILOC r1) f = PrintILOC (f r1)
mapToSrcRegs (Println r1) f = Println (f r1)
mapToSrcRegs (ReadILOC r1) _ = ReadILOC r1
mapToSrcRegs (Mov r1 r2) f = Mov (f r1) r2
mapToSrcRegs (Movi i1 r1) _ = Movi i1 r1
mapToSrcRegs (Moveq r1 r2) f = Moveq (f r1) r2
mapToSrcRegs (Movge r1 r2) f = Movge (f r1) r2
mapToSrcRegs (Movgt r1 r2) f = Movgt (f r1) r2
mapToSrcRegs (Movle r1 r2) f = Movle (f r1) r2
mapToSrcRegs (Movlt r1 r2) f = Movlt (f r1) r2
mapToSrcRegs (Movne r1 r2) f = Movne (f r1) r2
mapToSrcRegs (PrepArgs i) _ = PrepArgs i
mapToSrcRegs (UnprepArgs i) _ = UnprepArgs i

-- iloc registers we will read from for this instuction
getSrcIlocRegs :: Iloc -> [Reg]
getSrcIlocRegs (Add r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Div r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Mult r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Multi r1 _ r2) = [r1, r2]
getSrcIlocRegs (Sub r1 r2 r3) = [r1, r2, r3]
getSrcIlocRegs (Comp r1 r2) = [r1, r2]
getSrcIlocRegs (Compi r _) = [r]
getSrcIlocRegs (Brz r _ _) = [r]
getSrcIlocRegs (Loadai r _ _) = [r]
getSrcIlocRegs (Storeai r1 r2 _) = [r1, r2]
getSrcIlocRegs (Storeglobal r _) = [r]
getSrcIlocRegs (Storeoutargument r _) = [r]
getSrcIlocRegs (Storeret r) = [r]
getSrcIlocRegs (Del r) = [r]
getSrcIlocRegs (PrintILOC r) = [r]
getSrcIlocRegs (Println r) = [r]
getSrcIlocRegs (Mov r _) = [r]
getSrcIlocRegs (Moveq r1 r2) = [r1, r2]
getSrcIlocRegs (Movge r1 r2) = [r1, r2]
getSrcIlocRegs (Movgt r1 r2) = [r1, r2]
getSrcIlocRegs (Movle r1 r2) = [r1, r2]
getSrcIlocRegs (Movlt r1 r2) = [r1, r2]
getSrcIlocRegs (Movne r1 r2) = [r1, r2]
getSrcIlocRegs _ = []

-- get the iloc dest registers
getDstIlocRegs :: Iloc -> [Reg]
getDstIlocRegs (Add _ _ r) = [r]
getDstIlocRegs (Div _ _ r) = [r]
getDstIlocRegs (Mult _ _ r) = [r]
getDstIlocRegs (Multi _ _ r) = [r]
getDstIlocRegs (Sub _ _ r) = [r]
getDstIlocRegs (Loadai _ _ r) = [r]
getDstIlocRegs (Loadglobal _ r) = [r]
getDstIlocRegs (Loadinargument _ _ r) = [r]
getDstIlocRegs (Loadret r) = [r]
getDstIlocRegs (New _ r) = [r]
getDstIlocRegs (ReadILOC r) = [r]
getDstIlocRegs (Mov _ r) = [r]
getDstIlocRegs (Movi _ r) = [r]
getDstIlocRegs (Moveq _ r) = [r]
getDstIlocRegs (Movge _ r) = [r]
getDstIlocRegs (Movgt _ r) = [r]
getDstIlocRegs (Movle _ r) = [r]
getDstIlocRegs (Movlt _ r) = [r]
getDstIlocRegs (Movne _ r) = [r]
getDstIlocRegs _ = []
