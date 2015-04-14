{-|

This file contains the data definitions for our internal ILOC.
It is derivied from the instructor provided ILOC handout ("iloc.pdf").
Loadi is replaced with movi.
Adds a new instruction - brz

-}

import Mini.Types
import Data.List

type Reg = Int
type Immed = Int
type Label = String

data Iloc = Arithmetic | Boolean | Comparison | Branching | Loads | Stores | Invocation | Allocation | IO | Moves deriving(Show)

data Arithmetic = Add Reg Reg Reg
                | Addi Reg Immed Reg
                | Div Reg Reg Reg
                | Mult Reg Reg Reg
                | Sub Reg Reg Reg
                | Rsubi Reg Immed Reg

data Boolean = And Reg Reg Reg
             | Or Reg Reg Reg
             | Xori Reg Immed Reg

data Comparison = Comp Reg Reg
                | Compi Reg Immed

data Branching = Cbreq Label Label
               | Cbrge Label Label
               | Cbrgt Label Label
               | Cbrle Label Label
               | Cbrlt Label Label
               | Cbrne Label Label
               | Jumpi Label
               | Brz Reg Label Label

data Loads = Loadai Reg Immed Reg
           | Loadglobal Id Reg
           | Loadinargument Id Immed Reg
           | Loadret Reg
           | Computeformaladdress Id Immed Reg
           | Restoreformal Id Immed
           | Computeglobaladdress Id Reg

data Stores = Storeai Reg Reg Immed
            | Storeglobal Reg Id
            | Storeinargument Reg Id Immed
            | Storeoutargument Reg Immed
            | Storeret Reg

data Invocation = Call Label
                | RetILOC

data Allocation = New Immed Reg
                | Del Reg

data IoILOC = PrintILOC Reg
             | Println Reg
             | ReadILOC Reg

data Moves = Mov Reg Reg
           | Movi Immed Reg
           | Moveq Immed Reg
           | Movge Immed Reg
           | Movgt Immed Reg
           | Movle Immed Reg
           | Movlt Immed Reg
           | Movne Immed Reg

condCodeReg = "ccr"

showReg regNum = "r" ++ (show regNum)
showImmed immed = (show immed)
showLabel label = label
showId id = id

showIlocHelper name args = name ++ " " ++ ( concat $ intersperse ", " args)

instance Show Arithmetic where
   show (Add r1 r2 r3) = showIlocHelper "add" $ map showReg [r1, r2, r3]
   show (Addi r1 i1 r2) = showIlocHelper "addi" [showReg r1, showImmed i1, showReg r2]
   show (Div r1 r2 r3) = showIlocHelper "div" $ map showReg [r1, r2, r3]
   show (Mult r1 r2 r3) = showIlocHelper "mult" $ map showReg [r1, r2, r3]
   show (Sub r1 r2 r3) = showIlocHelper "sub" $ map showReg [r1, r2, r3]
   show (Rsubi r1 i1 r2) = showIlocHelper "rsubi" [showReg r1, showImmed i1, showReg r2]

instance Show Boolean where
   show (And r1 r2 r3) = showIlocHelper "and" $ map showReg [r1, r2, r3]
   show (Or r1 r2 r3) = showIlocHelper "or" $ map showReg [r1, r2, r3]
   show (Xori r1 i1 r2) = showIlocHelper "xori" [showReg r1, showImmed i1, showReg r2]

instance Show Comparison where
   show (Comp r1 r2) = showIlocHelper "comp" [showReg r1, showReg r2, condCodeReg]
   show (Compi r1 i1) = showIlocHelper "compi" [showReg r1, showImmed i1, condCodeReg]

instance Show Branching where
   show (Cbreq l1 l2) = showIlocHelper "cbreq" [condCodeReg, showLabel l1, showLabel l2]
   show (Cbrge l1 l2) = showIlocHelper "cbrge" [condCodeReg, showLabel l1, showLabel l2]
   show (Cbrgt l1 l2) = showIlocHelper "cbrgt" [condCodeReg, showLabel l1, showLabel l2]
   show (Cbrle l1 l2) = showIlocHelper "cbrle" [condCodeReg, showLabel l1, showLabel l2]
   show (Cbrlt l1 l2) = showIlocHelper "cbrlt" [condCodeReg, showLabel l1, showLabel l2]
   show (Cbrne l1 l2) = showIlocHelper "cbrne" [condCodeReg, showLabel l1, showLabel l2]
   show (Jumpi l1) = showIlocHelper "jumpi" [showLabel l1]
   show (Brz r1 l1 l2) = showIlocHelper "brz" [showReg r1, showLabel l1, showLabel l2]

instance Show Loads where
   show (Loadai r1 i1 r2) = showIlocHelper "loadai" [showReg r1, showImmed i1, showReg r2]
   show (Loadglobal s1 r1) = showIlocHelper "loadglobal" [showId s1, showReg r1]
   show (Loadinargument s1 i1 r1) = showIlocHelper "loadinargument" [showId s1, showImmed i1, showReg r1]
   show (Loadret r1) = showIlocHelper "loadret" [showReg r1]
   show (Computeformaladdress s1 i1 r1) = showIlocHelper "computeformaladdress" [showId s1, showImmed i1, showReg r1]
   show (Restoreformal s1 i1) = showIlocHelper "restoreformal" [showId s1, showImmed i1]
   show (Computeglobaladdress s1 r1) = showIlocHelper "computeglobaladdress" [showId s1, showReg r1]

instance Show Stores where
   show (Storeai r1 r2 i1) = showIlocHelper "storeai" [showReg r1, showReg r2, showImmed i1]
   show (Storeglobal r1 s1) = showIlocHelper "storeglobal" [showReg r1, showId s1]
   show (Storeinargument r1 s1 i1) = showIlocHelper "storeinargument" [showReg r1, showId s1, showReg i1]
   show (Storeoutargument r1 i1) = showIlocHelper "storeoutargument" [showReg r1, showImmed i1]
   show (Storeret r1) = showIlocHelper "storeret" [(showReg r1)]

instance Show Invocation where
   show (Call l1) = showIlocHelper "call" [showLabel l1]
   show RetILOC = "ret"

instance Show Allocation where
   show (New i1 r1) = showIlocHelper "new" [showImmed i1, showReg r1]
   show (Del r1) = showIlocHelper "del" [showReg r1]

instance Show IoILOC where
   show (PrintILOC r1) = showIlocHelper "print" [showReg r1]
   show (Println r1) = showIlocHelper "println" [showReg r1]
   show (ReadILOC r1) = showIlocHelper "read" [showReg r1]

instance Show Moves where
   show (Mov r1 r2) = showIlocHelper "mov" [showReg r1, showReg r2]
   show (Moveq i1 r1) = showIlocHelper "moveq" [showImmed i1, showReg r1]
   show (Movge i1 r1) = showIlocHelper "movge" [showImmed i1, showReg r1]
   show (Movgt i1 r1) = showIlocHelper "movgt" [showImmed i1, showReg r1]
   show (Movle i1 r1) = showIlocHelper "movle" [showImmed i1, showReg r1]
   show (Movlt i1 r1) = showIlocHelper "movlt" [showImmed i1, showReg r1]
   show (Movne i1 r1) = showIlocHelper "movne" [showImmed i1, showReg r1]
