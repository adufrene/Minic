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

data Iloc = 
                  Add Reg Reg Reg       -- Used
                | Addi Reg Immed Reg
                | Div Reg Reg Reg       -- Used
                | Mult Reg Reg Reg      -- Used
                | Multi Reg Immed Reg   -- Used
                | Sub Reg Reg Reg       -- Used
                | Rsubi Reg Immed Reg

             | And Reg Reg Reg
             | Or Reg Reg Reg
             | Xori Reg Immed Reg

                | Comp Reg Reg          -- Used
                | Compi Reg Immed       -- Used

               | Cbreq Label Label
               | Cbrge Label Label
               | Cbrgt Label Label
               | Cbrle Label Label
               | Cbrlt Label Label
               | Cbrne Label Label
               | Jumpi Label
               | Brz Reg Label Label    -- Used

           | Loadai Reg Immed Reg       -- Used
           | Loadglobal Id Reg          -- Used
           | Loadinargument Id Immed Reg --Used
           | Loadret Reg                -- Used
           | Computeformaladdress Id Immed Reg
           | Restoreformal Id Immed
           | Computeglobaladdress Id Reg

            | Storeai Reg Reg Immed         -- Used
            | Storeglobal Reg Id            -- Used
            | Storeinargument Reg Id Immed
            | Storeoutargument Reg Immed    -- Used
            | Storeret Reg                  -- Used

                | Call Label                -- Used
                | RetILOC                   -- Used

                | New Immed Reg             -- Used
                | Del Reg                   -- Used

             | PrintILOC Reg                -- Used
             | Println Reg                  -- Used
             | ReadILOC Reg                 -- Used

           | Mov Reg Reg                    -- Used
           | Movi Immed Reg     -- Used
           | Moveq Reg Reg    -- Used
           | Movge Reg Reg    -- Used
           | Movgt Reg Reg    -- Used
           | Movle Reg Reg    -- Used
           | Movlt Reg Reg    -- Used
           | Movne Reg Reg    -- Used

           | PrepArgs Immed
           | UnprepArgs Immed
           deriving (Eq)

condCodeReg :: String
condCodeReg = "ccr"

showReg :: Reg -> String
showReg regNum = "r" ++ show regNum

showIlocHelper :: String -> [String] -> String
showIlocHelper name args = name ++ " " ++ intercalate ", " args

instance Show Iloc where
   show (Add r1 r2 r3) = showIlocHelper "add" $ map showReg [r1, r2, r3]
   show (Addi r1 i1 r2) = showIlocHelper "addi" [showReg r1, show i1, showReg r2]
   show (Div r1 r2 r3) = showIlocHelper "div" $ map showReg [r1, r2, r3]
   show (Mult r1 r2 r3) = showIlocHelper "mult" $ map showReg [r1, r2, r3]
   show (Multi r1 i1 r2) = showIlocHelper "multi" [showReg r1, show i1, showReg r2]
   show (Sub r1 r2 r3) = showIlocHelper "sub" $ map showReg [r1, r2, r3]
   show (Rsubi r1 i1 r2) = showIlocHelper "rsubi" [showReg r1, show i1, showReg r2]

   show (And r1 r2 r3) = showIlocHelper "and" $ map showReg [r1, r2, r3]
   show (Or r1 r2 r3) = showIlocHelper "or" $ map showReg [r1, r2, r3]
   show (Xori r1 i1 r2) = showIlocHelper "xori" [showReg r1, show i1, showReg r2]

   show (Comp r1 r2) = showIlocHelper "comp" [showReg r1, showReg r2, condCodeReg]
   show (Compi r1 i1) = showIlocHelper "compi" [showReg r1, show i1, condCodeReg]

   show (Cbreq l1 l2) = showIlocHelper "cbreq" [condCodeReg, l1, l2]
   show (Cbrge l1 l2) = showIlocHelper "cbrge" [condCodeReg, l1, l2]
   show (Cbrgt l1 l2) = showIlocHelper "cbrgt" [condCodeReg, l1, l2]
   show (Cbrle l1 l2) = showIlocHelper "cbrle" [condCodeReg, l1, l2]
   show (Cbrlt l1 l2) = showIlocHelper "cbrlt" [condCodeReg, l1, l2]
   show (Cbrne l1 l2) = showIlocHelper "cbrne" [condCodeReg, l1, l2]
   show (Jumpi l1) = showIlocHelper "jumpi" [l1]
   show (Brz r1 l1 l2) = showIlocHelper "brz" [showReg r1, l1, l2]

   show (Loadai r1 i1 r2) = showIlocHelper "loadai" [showReg r1, show i1, showReg r2]
   show (Loadglobal s1 r1) = showIlocHelper "loadglobal" [s1, showReg r1]
   show (Loadinargument s1 i1 r1) = showIlocHelper "loadinargument" [s1, show i1, showReg r1]
   show (Loadret r1) = showIlocHelper "loadret" [showReg r1]
   show (Computeformaladdress s1 i1 r1) = showIlocHelper "computeformaladdress" [s1, show i1, showReg r1]
   show (Restoreformal s1 i1) = showIlocHelper "restoreformal" [s1, show i1]
   show (Computeglobaladdress s1 r1) = showIlocHelper "computeglobaladdress" [s1, showReg r1]

   show (Storeai r1 r2 i1) = showIlocHelper "storeai" [showReg r1, showReg r2, show i1]
   show (Storeglobal r1 s1) = showIlocHelper "storeglobal" [showReg r1, s1]
   show (Storeinargument r1 s1 i1) = showIlocHelper "storeinargument" [showReg r1, s1, showReg i1]
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
mapToRegs (Addi r1 i1 r2) f = Addi (f r1) i1 (f r2)
mapToRegs (Div r1 r2 r3) f = Div (f r1) (f r2) (f r3)
mapToRegs (Mult r1 r2 r3) f = Mult (f r1) (f r2) (f r3)
mapToRegs (Multi r1 i1 r2) f = Multi (f r1) i1 (f r2)
mapToRegs (Sub r1 r2 r3) f = Sub (f r1) (f r2) (f r3)
mapToRegs (Rsubi r1 i1 r2) f = Rsubi (f r1) i1 (f r2)

mapToRegs (And r1 r2 r3) f = And (f r1) (f r2) (f r3)
mapToRegs (Or r1 r2 r3) f = Or (f r1) (f r2) (f r3)
mapToRegs (Xori r1 i1 r2) f = Xori (f r1) i1 (f r2)

mapToRegs (Comp r1 r2) f = Comp (f r1) (f r2)
mapToRegs (Compi r1 i1) f = Compi (f r1) i1

mapToRegs (Cbreq l1 l2) f = Cbreq l1 l2
mapToRegs (Cbrge l1 l2) f = Cbrge l1 l2
mapToRegs (Cbrgt l1 l2) f = Cbrgt l1 l2
mapToRegs (Cbrle l1 l2) f = Cbrle l1 l2
mapToRegs (Cbrlt l1 l2) f = Cbrlt l1 l2
mapToRegs (Cbrne l1 l2) f = Cbrne l1 l2
mapToRegs (Jumpi l1) f = Jumpi l1
mapToRegs (Brz r1 l1 l2) f = Brz (f r1) l1 l2

mapToRegs (Loadai r1 i1 r2) f = Loadai (f r1) i1 (f r2)
mapToRegs (Loadglobal s1 r1) f = Loadglobal s1 (f r1)
mapToRegs (Loadinargument s1 i1 r1) f = Loadinargument s1 i1 (f r1)
mapToRegs (Loadret r1) f = Loadret (f r1)
mapToRegs (Computeformaladdress s1 i1 r1) f = Computeformaladdress s1 i1 (f r1)
mapToRegs (Restoreformal s1 i1) f = Restoreformal s1 i1
mapToRegs (Computeglobaladdress s1 r1) f = Computeglobaladdress s1 (f r1)

mapToRegs (Storeai r1 r2 i1) f = Storeai (f r1) (f r2) i1
mapToRegs (Storeglobal r1 s1) f = Storeglobal (f r1) s1
mapToRegs (Storeinargument r1 s1 i1) f = Storeinargument (f r1) s1 i1
mapToRegs (Storeoutargument r1 i1) f = Storeoutargument (f r1) i1
mapToRegs (Storeret r1) f = Storeret (f r1)

mapToRegs (Call l1) f = Call l1
mapToRegs RetILOC f = RetILOC

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

mapToRegs (PrepArgs i) f = PrepArgs i
mapToRegs (UnprepArgs i) f = UnprepArgs i

-- applies given func to each source register in an ILOC and returns resulting iloc
mapToSrcRegs :: Iloc -> (Reg -> Reg) -> Iloc
mapToSrcRegs (Add r1 r2 r3) f = Add (f r1) (f r2) r3
mapToSrcRegs (Addi r1 i1 r2) f = Addi (f r1) i1 r2
mapToSrcRegs (Div r1 r2 r3) f = Div (f r1) (f r2) r3
mapToSrcRegs (Mult r1 r2 r3) f = Mult (f r1) (f r2) r3
mapToSrcRegs (Multi r1 i1 r2) f = Multi (f r1) i1 r2
mapToSrcRegs (Sub r1 r2 r3) f = Sub (f r1) (f r2) r3
mapToSrcRegs (Rsubi r1 i1 r2) f = Rsubi (f r1) i1 r2

mapToSrcRegs (And r1 r2 r3) f = And (f r1) (f r2) r3
mapToSrcRegs (Or r1 r2 r3) f = Or (f r1) (f r2) r3
mapToSrcRegs (Xori r1 i1 r2) f = Xori (f r1) i1 r2

mapToSrcRegs (Comp r1 r2) f = Comp (f r1) (f r2)
mapToSrcRegs (Compi r1 i1) f = Compi (f r1) i1

mapToSrcRegs (Cbreq l1 l2) f = Cbreq l1 l2
mapToSrcRegs (Cbrge l1 l2) f = Cbrge l1 l2
mapToSrcRegs (Cbrgt l1 l2) f = Cbrgt l1 l2
mapToSrcRegs (Cbrle l1 l2) f = Cbrle l1 l2
mapToSrcRegs (Cbrlt l1 l2) f = Cbrlt l1 l2
mapToSrcRegs (Cbrne l1 l2) f = Cbrne l1 l2
mapToSrcRegs (Jumpi l1) f = Jumpi l1
mapToSrcRegs (Brz r1 l1 l2) f = Brz (f r1) l1 l2

mapToSrcRegs (Loadai r1 i1 r2) f = Loadai (f r1) i1 r2
mapToSrcRegs (Loadglobal s1 r1) f = Loadglobal s1 r1
mapToSrcRegs (Loadinargument s1 i1 r1) f = Loadinargument s1 i1 r1
mapToSrcRegs (Loadret r1) f = Loadret r1
mapToSrcRegs (Computeformaladdress s1 i1 r1) f = Computeformaladdress s1 i1 r1
mapToSrcRegs (Restoreformal s1 i1) f = Restoreformal s1 i1
mapToSrcRegs (Computeglobaladdress s1 r1) f = Computeglobaladdress s1 r1

mapToSrcRegs (Storeai r1 r2 i1) f = Storeai (f r1) (f r2) i1
mapToSrcRegs (Storeglobal r1 s1) f = Storeglobal (f r1) s1
mapToSrcRegs (Storeinargument r1 s1 i1) f = Storeinargument (f r1) s1 i1
mapToSrcRegs (Storeoutargument r1 i1) f = Storeoutargument (f r1) i1
mapToSrcRegs (Storeret r1) f = Storeret (f r1)

mapToSrcRegs (Call l1) f = Call l1
mapToSrcRegs RetILOC f = RetILOC

mapToSrcRegs (New i1 r1) f = New i1 r1
mapToSrcRegs (Del r1) f = Del (f r1)

mapToSrcRegs (PrintILOC r1) f = PrintILOC (f r1)
mapToSrcRegs (Println r1) f = Println (f r1)
mapToSrcRegs (ReadILOC r1) f = ReadILOC r1

mapToSrcRegs (Mov r1 r2) f = Mov (f r1) r2
mapToSrcRegs (Movi i1 r1) f = Movi i1 r1
mapToSrcRegs (Moveq r1 r2) f = Moveq (f r1) r2
mapToSrcRegs (Movge r1 r2) f = Movge (f r1) r2
mapToSrcRegs (Movgt r1 r2) f = Movgt (f r1) r2
mapToSrcRegs (Movle r1 r2) f = Movle (f r1) r2
mapToSrcRegs (Movlt r1 r2) f = Movlt (f r1) r2
mapToSrcRegs (Movne r1 r2) f = Movne (f r1) r2

mapToSrcRegs (PrepArgs i) f = PrepArgs i
mapToSrcRegs (UnprepArgs i) f = UnprepArgs i
mapToSrcRegs iloc f = error $ "didn't expect to see a " ++ (show iloc)
