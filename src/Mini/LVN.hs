module Mini.LVN ( numberGraph ) where 

import Control.Arrow

import Data.Hashable
import Data.Graph (Vertex)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

import Mini.Iloc.Types
import Mini.Graph

type LVN = Int

type IlocHash = HM.HashMap Vertex IlocNode
type LVNHash = (HM.HashMap LVNKey LVN, HM.HashMap LVN Reg)

data LVNKey = LVNAdd LVN LVN
            | LVNMult LVN LVN
            | LVNSub LVN LVN
            | LVNDiv LVN LVN
            | LVNReg Reg
            deriving (Show)

assocEqual :: (LVN, LVN) -> (LVN, LVN) -> Bool
assocEqual (lv1, lv2) (lv1', lv2') =  lv1 == lv1' && lv2 == lv2'
                                   || lv1 == lv2' && lv2 == lv1'
            
instance Eq LVNKey where
        LVNAdd lv1 lv2 == LVNAdd lv1' lv2' = assocEqual (lv1, lv2) (lv1', lv2')
        LVNMult lv1 lv2 == LVNMult lv1' lv2' = assocEqual (lv1, lv2) (lv1', lv2') 
        LVNSub lv1 lv2 == LVNSub lv1' lv2' = lv1 == lv1' && lv2 == lv2'
        LVNDiv lv1 lv2 == LVNDiv lv1' lv2' = lv1 == lv1' && lv2 == lv2'
        LVNReg r == LVNReg r' = r == r'
        _ == _ = False

instance Hashable LVNKey where
        hashWithSalt salt (LVNAdd lv1 lv2) = lv1 * lv2 + salt
        hashWithSalt salt (LVNMult lv1 lv2) = (lv1 * lv2 + salt) * 23753
        hashWithSalt salt (LVNSub lv1 lv2) = hash lv1 `hashWithSalt` lv2 + salt
        hashWithSalt salt (LVNDiv lv1 lv2) = (hash lv1 `hashWithSalt` lv2 + salt) 
                                                * 23753
        hashWithSalt salt (LVNReg r) = hashWithSalt salt r

numberGraph :: (Reg, IlocGraph) -> (Reg, IlocGraph)
numberGraph (nextReg, (graph, hash)) = second (\x -> (graph, x)) 
        $ HM.foldlWithKey' numberBlock (nextReg, HM.empty) hash 

numberBlock :: (Reg, IlocHash) -> Vertex -> IlocNode -> (Reg, IlocHash)
numberBlock (nextReg, newHash) v node = 
        (newNext, HM.insert v (const newIloc `mapNode` node) newHash)
    where (newNext, newIloc) = numberIloc nextReg $ getData node

numberIloc :: Reg -> [Iloc] -> (Reg, [Iloc])
numberIloc nextReg iloc = undefined
    where firstPass = L.foldl' createLVNHash 
            (nextReg, (HM.empty, HM.empty)) iloc

createLVNHash :: (Reg, LVNHash) -> Iloc -> (Reg, LVNHash)
createLVNHash rh@(reg, hash) il
    | isNumberable il = undefined
    | otherwise = rh

isNumberable :: Iloc -> Bool
isNumberable il = False
