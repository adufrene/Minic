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
                  Add Reg Reg Reg
                | Addi Reg Immed Reg
                | Div Reg Reg Reg
                | Mult Reg Reg Reg
                | Multi Reg Immed Reg
                | Sub Reg Reg Reg
                | Rsubi Reg Immed Reg

             | And Reg Reg Reg
             | Or Reg Reg Reg
             | Xori Reg Immed Reg

                | Comp Reg Reg
                | Compi Reg Immed

               | Cbreq Label Label
               | Cbrge Label Label
               | Cbrgt Label Label
               | Cbrle Label Label
               | Cbrlt Label Label
               | Cbrne Label Label
               | Jumpi Label
               | Brz Reg Label Label

           | Loadai Reg Immed Reg
           | Loadglobal Id Reg
           | Loadinargument Id Immed Reg
           | Loadret Reg
           | Computeformaladdress Id Immed Reg
           | Restoreformal Id Immed
           | Computeglobaladdress Id Reg

            | Storeai Reg Reg Immed
            | Storeglobal Reg Id
            | Storeinargument Reg Id Immed
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
           | Moveq Immed Reg
           | Movge Immed Reg
           | Movgt Immed Reg
           | Movle Immed Reg
           | Movlt Immed Reg
           | Movne Immed Reg

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
   show (Moveq i1 r1) = showIlocHelper "moveq" [show i1, showReg r1]
   show (Movge i1 r1) = showIlocHelper "movge" [show i1, showReg r1]
   show (Movgt i1 r1) = showIlocHelper "movgt" [show i1, showReg r1]
   show (Movle i1 r1) = showIlocHelper "movle" [show i1, showReg r1]
   show (Movlt i1 r1) = showIlocHelper "movlt" [show i1, showReg r1]
   show (Movne i1 r1) = showIlocHelper "movne" [show i1, showReg r1]
