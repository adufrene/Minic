module Mini.LVN ( numberGraph ) where 

import Control.Arrow

import Data.Hashable

import Mini.Iloc.Types
import Mini.Graph

type LVN = Int

data BinExp = LVNAdd LVN LVN
            | LVNMult LVN LVN
            | LVNSub LVN LVN
            | LVNDiv LVN LVN
            deriving (Show)

assocEqual :: (LVN, LVN) -> (LVN, LVN) -> Bool
assocEqual (lv1, lv2) (lv1', lv2') =  lv1 == lv1' && lv2 == lv2'
                                   || lv1 == lv2' && lv2 == lv1'
            
instance Eq BinExp where
        LVNAdd lv1 lv2 == LVNAdd lv1' lv2' = assocEqual (lv1, lv2) (lv1', lv2')
        LVNMult lv1 lv2 == LVNMult lv1' lv2' = assocEqual (lv1, lv2) (lv1', lv2') 
        LVNSub lv1 lv2 == LVNSub lv1' lv2' = lv1 == lv1' && lv2 == lv2'
        LVNDiv lv1 lv2 == LVNDiv lv1' lv2' = lv1 == lv1' && lv2 == lv2'
        _ == _ = False

instance Hashable BinExp where
        hashWithSalt salt (LVNAdd lv1 lv2) = lv1 * lv2 + salt
        hashWithSalt salt (LVNMult lv1 lv2) = (lv1 * lv2 + salt) * 23753
        hashWithSalt salt (LVNSub lv1 lv2) = hash lv1 `hashWithSalt` lv2 + salt
        hashWithSalt salt (LVNDiv lv1 lv2) = (hash lv1 `hashWithSalt` lv2 + salt) 
                                                * 23753

numberGraph :: Reg -> IlocGraph -> (Reg, IlocGraph)
numberGraph nextReg ig = undefined
