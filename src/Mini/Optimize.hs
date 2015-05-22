module Mini.Optimize ( removeUselessCode ) where
                     

import Mini.Iloc.Types
import Mini.RegAlloc
import Mini.Graph

import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.List
import qualified Data.HashMap.Strict as HM

data Marked a = Marked a Bool deriving (Eq)

instance Functor Marked where 
        fmap f (Marked a m) = Marked (f a) m

instance Applicative Marked where
        pure = return
        (<*>) = ap

instance Monad Marked where
        return = flip Marked False
        (Marked a _) >>= f = f a

type MarkedIloc = Marked Iloc
type Worklist = [Iloc]
type Reaches = ()
type ReachGen = ()
type KillGen = ()

isMarked :: Marked a -> Bool
isMarked (Marked _ m) = m

mark :: Marked a -> Marked a
mark (Marked a _) = Marked a True

unmark :: Marked a -> Marked a
unmark (Marked a _) = Marked a False

getMarkedData :: Marked a -> a
getMarkedData (Marked a _) = a

removeUselessCode :: IlocGraph -> IlocGraph
removeUselessCode = second (HM.map $ mapNode $ sweep . markIloc)

markIloc :: [Iloc] -> [MarkedIloc]
markIloc iloc = finishMark workList markedInsns
    where (workList, markedInsns) = mapAccumL mapFun [] iloc
          mapFun l x = if isCritical x 
                           then (x:l, Marked x True) 
                           else (l, Marked x False)

createReaches :: IlocGraph -> Reaches
createReaches (graph, hash) = undefined

createReachingGen :: IlocNode -> ReachGen
createReachingGen node = undefined

createReachingKill :: IlocNode -> KillGen
createReachingKill node = undefined

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
          insn = getMarkedData x
          destRegs = getDstIlocRegs insn
          (newInsn, newIloc) = if null old || isMarked x
                                   then (x, restIloc)
                                   else (mark x, insn:restIloc)

sweep :: [MarkedIloc] -> [Iloc]
sweep = map getMarkedData . filter isMarked

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
