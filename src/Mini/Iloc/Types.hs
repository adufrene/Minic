module Mini.Iloc.Types where
{-|

This file contains the data definitions for our internal ILOC.
It is derivied from the instructor provided ILOC handout ("iloc.pdf").
Loadi is replaced with movi.
Adds a new instruction - brz

-}

import Data.List
import Data.HashMap.Strict hiding (map)
import Mini.Types

type RegHash = HashMap Id Reg
type Baggage = (GlobalEnv, LocalEnv, RegHash)
type IlocRet = ([Iloc], Reg)

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
           | Moveq Immed Reg    -- Used
           | Movge Immed Reg    -- Used
           | Movgt Immed Reg    -- Used
           | Movle Immed Reg    -- Used
           | Movlt Immed Reg    -- Used
           | Movne Immed Reg    -- Used

           | PrepArgs Immed
           | UnprepArgs Immed
           deriving (Eq)

condCodeReg :: String
condCodeReg = "ccr"

showReg :: Reg -> String
showReg regNum = "r" ++ show regNum

showIlocHelper :: String -> [String] -> String
showIlocHelper name args = name ++ " " ++ intercalate ", " args

-- registers we will read from for this instruction
getSrcRegs :: Iloc -> [Reg]
getSrcRegs (Add r1 r2 r3) = [r1, r2]
getSrcRegs (Addi r1 i1 r2) = [r1]
getSrcRegs (Div r1 r2 r3) = [r1, r2]
getSrcRegs (Mult r1 r2 r3) = [r1, r2]
getSrcRegs (Multi r1 i1 r2) = [r1]
getSrcRegs (Sub r1 r2 r3) = [r1, r2]
getSrcRegs (Rsubi r1 i1 r2) = [r1]

getSrcRegs (And r1 r2 r3) = [r1, r2]
getSrcRegs (Or r1 r2 r3) = [r1, r2]
getSrcRegs (Xori r1 i1 r2) = [r1]

getSrcRegs (Comp r1 r2) = [r1, r2]
getSrcRegs (Compi r1 i1) = [r1]

getSrcRegs (Cbreq l1 l2) = []
getSrcRegs (Cbrge l1 l2) = []
getSrcRegs (Cbrgt l1 l2) = []
getSrcRegs (Cbrle l1 l2) = []
getSrcRegs (Cbrlt l1 l2) = []
getSrcRegs (Cbrne l1 l2) = []
getSrcRegs (Jumpi l1) = []
getSrcRegs (Brz r1 l1 l2) = [r1]

getSrcRegs (Loadai r1 i1 r2) = [r1]
getSrcRegs (Loadglobal s1 r1) = []
getSrcRegs (Loadinargument s1 i1 r1) = []
getSrcRegs (Loadret r1) = []
getSrcRegs (Computeformaladdress s1 i1 r1) = []
getSrcRegs (Restoreformal s1 i1) = []
getSrcRegs (Computeglobaladdress s1 r1) = []

getSrcRegs (Storeai r1 r2 i1) = [r1, r2]
getSrcRegs (Storeglobal r1 s1) = [r1]
getSrcRegs (Storeinargument r1 s1 i1) = [r1]
getSrcRegs (Storeoutargument r1 i1) = [r1]
getSrcRegs (Storeret r1) = [r1]

getSrcRegs (Call l1) = []
getSrcRegs RetILOC = []

getSrcRegs (New i1 r1) = []
getSrcRegs (Del r1) = [r1]

getSrcRegs (PrintILOC r1) = [r1]
getSrcRegs (Println r1) = [r1]
getSrcRegs (ReadILOC r1) = []

getSrcRegs (Mov r1 r2) = [r1]
getSrcRegs (Movi i1 r1) = []
getSrcRegs (Moveq i1 r1) = []
getSrcRegs (Movge i1 r1) = []
getSrcRegs (Movgt i1 r1) = []
getSrcRegs (Movle i1 r1) = []
getSrcRegs (Movlt i1 r1) = []
getSrcRegs (Movne i1 r1) = []

getSrcRegs (PrepArgs _) = []
getSrcRegs (UnprepArgs _) = []

getSrcRegs iloc = error $ "unexpected input " ++ (show iloc)

-- registers we will write to for this instruction
getDstRegs :: Iloc -> [Reg]
getDstRegs (Add r1 r2 r3) = [r3]
getDstRegs (Addi r1 i1 r2) = [r2]
getDstRegs (Div r1 r2 r3) = [r3]
getDstRegs (Mult r1 r2 r3) = [r3]
getDstRegs (Multi r1 i1 r2) = [r2]
getDstRegs (Sub r1 r2 r3) = [r3]
getDstRegs (Rsubi r1 i1 r2) = [r2]

getDstRegs (And r1 r2 r3) = [r3]
getDstRegs (Or r1 r2 r3) = [r3]
getDstRegs (Xori r1 i1 r2) = [r2]

getDstRegs (Comp r1 r2) = []
getDstRegs (Compi r1 i1) = []

getDstRegs (Cbreq l1 l2) = []
getDstRegs (Cbrge l1 l2) = []
getDstRegs (Cbrgt l1 l2) = []
getDstRegs (Cbrle l1 l2) = []
getDstRegs (Cbrlt l1 l2) = []
getDstRegs (Cbrne l1 l2) = []
getDstRegs (Jumpi l1) = []
getDstRegs (Brz r1 l1 l2) = []

getDstRegs (Loadai r1 i1 r2) = [r2]
getDstRegs (Loadglobal s1 r1) = [r1]
getDstRegs (Loadinargument s1 i1 r1) = [r1]
getDstRegs (Loadret r1) = [r1]
getDstRegs (Computeformaladdress s1 i1 r1) = [r1]
getDstRegs (Restoreformal s1 i1) = []
getDstRegs (Computeglobaladdress s1 r1) = [r1]

getDstRegs (Storeai r1 r2 i1) = []
getDstRegs (Storeglobal r1 s1) = []
getDstRegs (Storeinargument r1 s1 i1) = []
getDstRegs (Storeoutargument r1 i1) = []
getDstRegs (Storeret r1) = []

getDstRegs (Call l1) = []
getDstRegs RetILOC = []

getDstRegs (New i1 r1) = [r1]
getDstRegs (Del r1) = []

getDstRegs (PrintILOC r1) = []
getDstRegs (Println r1) = []
getDstRegs (ReadILOC r1) = []

getDstRegs (Mov r1 r2) = [r2]
getDstRegs (Movi i1 r1) = [r1]
getDstRegs (Moveq i1 r1) = [r1]
getDstRegs (Movge i1 r1) = [r1]
getDstRegs (Movgt i1 r1) = [r1]
getDstRegs (Movle i1 r1) = [r1]
getDstRegs (Movlt i1 r1) = [r1]
getDstRegs (Movne i1 r1) = [r1]

getDstRegs (PrepArgs _) = []
getDstRegs (UnprepArgs _) = []

getDstRegs iloc = error $ "unexpected input " ++ (show iloc)

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
   show (Moveq i1 r1) = showIlocHelper "moveq" [show i1, showReg r1]
   show (Movge i1 r1) = showIlocHelper "movge" [show i1, showReg r1]
   show (Movgt i1 r1) = showIlocHelper "movgt" [show i1, showReg r1]
   show (Movle i1 r1) = showIlocHelper "movle" [show i1, showReg r1]
   show (Movlt i1 r1) = showIlocHelper "movlt" [show i1, showReg r1]
   show (Movne i1 r1) = showIlocHelper "movne" [show i1, showReg r1]

   show (PrepArgs i) = showIlocHelper "prepArgs" [show i]
   show (UnprepArgs i) = showIlocHelper "unprepArgs" [show i]
