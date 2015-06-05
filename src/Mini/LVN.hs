module Mini.LVN ( numberGraph ) where 

import Control.Arrow
import Control.Monad (join)

import Data.Hashable
import Data.Graph (Vertex)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

import Mini.Iloc.Types
import Mini.Graph

type LVN = Int

type IlocHash = HM.HashMap Vertex IlocNode
type LVHash = (HM.HashMap LVKey LVN, HM.HashMap LVN [Reg])
type NextAndHash = (Reg, LVHash)

data LVKey = LVAdd LVN LVN
           | LVMul LVN LVN
           | LVSub LVN LVN
           | LVDiv LVN LVN
           | LVReg Reg
           | LVImm Immed
           deriving (Show)

assocEqual :: (LVN, LVN) -> (LVN, LVN) -> Bool
assocEqual (lv1, lv2) (lv1', lv2') =  lv1 == lv1' && lv2 == lv2'
                                   || lv1 == lv2' && lv2 == lv1'

prime :: Int
prime = 23753
            
instance Eq LVKey where
        LVAdd lv1 lv2 == LVAdd lv1' lv2' = assocEqual (lv1, lv2) (lv1', lv2')
        LVMul lv1 lv2 == LVMul lv1' lv2' = assocEqual (lv1, lv2) (lv1', lv2') 
        LVSub lv1 lv2 == LVSub lv1' lv2' = lv1 == lv1' && lv2 == lv2'
        LVDiv lv1 lv2 == LVDiv lv1' lv2' = lv1 == lv1' && lv2 == lv2'
        LVReg r == LVReg r' = r == r'
        LVImm i == LVImm i' = i == i'
        _ == _ = False

instance Hashable LVKey where
        hashWithSalt salt (LVAdd lv1 lv2) = lv1 * lv2 + salt
        hashWithSalt salt (LVMul lv1 lv2) = (lv1 * lv2 + salt) * prime
        hashWithSalt salt (LVSub lv1 lv2) = hash lv1 `hashWithSalt` lv2 + salt
        hashWithSalt salt (LVDiv lv1 lv2) = (hash lv1 `hashWithSalt` lv2 + salt) 
                                                * prime
        hashWithSalt salt (LVReg r) = hashWithSalt salt r * prime
        hashWithSalt salt (LVImm i) = hashWithSalt salt i

numberGraph :: (Reg, IlocGraph) -> (Reg, IlocGraph)
numberGraph (nextReg, (graph, hash)) = second (\x -> (graph, x)) 
        $ HM.foldlWithKey' numberBlock (nextReg, HM.empty) hash 

numberBlock :: (Reg, IlocHash) -> Vertex -> IlocNode -> (Reg, IlocHash)
numberBlock (nextReg, newHash) v node = 
        (newNext, HM.insert v (const newIloc `mapNode` node) newHash)
    where (newNext, newIloc) = numberIloc nextReg $ getData node

numberIloc :: Reg -> [Iloc] -> (Reg, [Iloc])
numberIloc nextReg iloc = (newReg, newIl)
    where ((newReg, lvHash), newIl) = second concat $ L.mapAccumL createLVHash 
            (nextReg, (HM.empty, HM.empty)) iloc

createLVHash :: NextAndHash -> Iloc -> (NextAndHash, [Iloc])
createLVHash nah il = findFunc il nah


lvInsert :: LVKey -> LVN -> LVHash -> LVHash
lvInsert key val (kToL, lToR) = (HM.insert key val kToL, newLToR)
    where newLToR = case key of
                        LVReg r -> HM.insertWith (++) val [r] lToR
                        _ -> lToR

findFunc :: Iloc -> NextAndHash -> (NextAndHash, [Iloc])
findFunc il@(Add r1 r2 r3) = makeLV LVAdd (LVReg r1) (LVReg r2) (LVReg r3) il
findFunc il@(Div r1 r2 r3) = makeLV LVDiv (LVReg r1) (LVReg r2) (LVReg r3) il
findFunc il@(Mult r1 r2 r3) = makeLV LVMul (LVReg r1) (LVReg r2) (LVReg r3) il
findFunc il@(Multi r1 i r2) = makeLV LVMul (LVReg r1) (LVImm i) (LVReg r2) il
findFunc il@(Sub r1 r2 r3) = makeLV LVSub (LVReg r1) (LVReg r2) (LVReg r3) il
findFunc il@(Mov r1 r2) = numMov (LVReg r1) (LVReg r2) il
findFunc il@(Movi i r) = numMov (LVImm i) (LVReg r) il
findFunc il = \x -> (x, [il])


findOrInsert :: NextAndHash -> LVKey -> (NextAndHash, LVN)
findOrInsert nah@(next, (kToL, _)) key =  
        if key `HM.member` kToL
            then (nah, kToL HM.! key)
            else (key `insert` nah, next)

member :: LVKey -> NextAndHash -> Bool
member key (_, (kToL, _)) = key `HM.member` kToL

(!) :: NextAndHash -> LVKey -> LVN
(!) (_, (kToL, _)) key = kToL HM.! key

insert :: LVKey -> NextAndHash -> NextAndHash
insert key (nextReg, hash) = (nextReg + 1, lvInsert key nextReg hash)

{- Insert and kill -}
newExpr :: LVKey -> LVKey -> NextAndHash -> NextAndHash
newExpr expKey dest (next, hash) = (next + 1, lvInsert dest next nah')
    where nah' = lvInsert expKey next hash

makeLV :: (LVN -> LVN -> LVKey) -> LVKey -> LVKey -> LVKey -> Iloc 
        -> NextAndHash -> (NextAndHash, [Iloc])
makeLV keyFun src1 src2 dest il nah = 
        if lvKey `member` nah''
            then createCopy nah'' lvKey dest
            else (newExpr lvKey dest nah'', [il])
    where (nah', lv1) = nah `findOrInsert` src1
          (nah'', lv2) = nah' `findOrInsert` src2
          lvKey = keyFun lv1 lv2

numMov :: LVKey -> LVKey -> Iloc -> NextAndHash -> (NextAndHash, [Iloc])
numMov src dest il nah = (second (lvInsert dest lv1) nah', [il])
    where (nah', lv1) = nah `findOrInsert` src

createCopy :: NextAndHash -> LVKey -> LVKey -> (NextAndHash, [Iloc])
createCopy nah expKey dest@(LVReg r) = 
        (second (lvInsert dest lvn) nah, [Mov lvn r])
    where lvn = nah ! expKey
