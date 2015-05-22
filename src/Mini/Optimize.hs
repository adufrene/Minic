module Mini.Optimize ( removeUselessCode ) where
                     

import Mini.Iloc.Types
import Mini.RegAlloc

import Control.Applicative
import Control.Arrow

import Data.List

data Marked a = Marked a Bool deriving (Eq)

instance Functor Marked where 
        fmap f (Marked a m) = Marked (f a) m

instance Applicative Marked where
        pure = flip Marked True
        (Marked f _) <*> (Marked a m) = Marked (f a) m

type MarkedIloc = Marked Iloc
type Worklist = [Iloc]

isMarked :: Marked a -> Bool
isMarked (Marked _ m) = m

mark :: Marked a -> Marked a
mark (Marked a _) = Marked a True

unmark :: Marked a -> Marked a
unmark (Marked a _) = Marked a False

getData :: Marked a -> a
getData (Marked a _) = a

removeUselessCode :: [Iloc] -> [Iloc]
removeUselessCode = sweep . markIloc

markIloc :: [Iloc] -> [MarkedIloc]
markIloc iloc = finishMark workList markedInsns
    where (workList, markedInsns) = mapAccumL mapFun [] iloc
          mapFun l x = if isCritical x 
                           then (x:l, Marked x True) 
                           else (l, Marked x False)

{- 
- Guessing.... for each src reg in insn, go back to where src is target of
- reg, mark that insn, add to worklist
-}
finishMark :: Worklist -> [MarkedIloc] -> [MarkedIloc]
finishMark [] xs = xs
finishMark (insn:rest) xs = finishMark (marked ++ rest) (new ++ old)
    where (srcList, old) = span (/= Marked insn True) xs
          (new, marked) = markSrcs (getSrcIlocRegs insn) srcList 

markSrcs :: [Reg] -> [MarkedIloc] -> ([MarkedIloc], [Iloc])
markSrcs srcs srcList = first reverse markedSrcs
        where markedSrcs = markReversedSrcs srcs $ reverse srcList

markReversedSrcs :: [Reg] -> [MarkedIloc] -> ([MarkedIloc], [Iloc])
markReversedSrcs _ [] = ([], []) -- run out of instructions
markReversedSrcs [] xs = (xs, []) -- run out of src regs
markReversedSrcs srcs (x:xs) = (newInsn:newMarks, newIloc)
    where (newMarks, restIloc) = markReversedSrcs newSrcs xs
          (newSrcs, old) = partition (`notElem` destRegs) srcs
          insn = getData x
          destRegs = getDstIlocRegs insn
          (newInsn, newIloc) = if null old || isMarked x
                                   then (x, restIloc)
                                   else (mark x, insn:restIloc)

sweep :: [MarkedIloc] -> [Iloc]
sweep = map getData . filter isMarked

isCritical :: Iloc -> Bool
isCritical Storeret{} = True
isCritical Brz{} = True
isCritical Jumpi{} = True
isCritical Call{} = True
isCritical RetILOC{} = True
isCritical Storeglobal{} = True
isCritical Loadglobal{} = True  -- Is this critical?
isCritical Println{} = True
isCritical PrintILOC{} = True
isCritical ReadILOC{} = True
isCritical PrepArgs{} = True
isCritical Comp{} = True
isCritical Compi{} = True
isCritical UnprepArgs{} = True
isCritical Storeoutargument{} = True
isCritical _ = False
