module Mini.Asm.Types where

import Mini.Iloc.Types

data Asm = AsmAdd
         | AsmSub
         | AsmDiv
         | AsmMult
         | AsmMulti
         | AsmSub
         | AsmComp
         | AsmCompi
         | AsmBrz
         | AsmLoadai
         | AsmLoadGlobal
         | AsmLoadInArg
         | AsmLoadRet
         | AsmStoreai
         | AsmStoreGlobal
         | AsmStoreInArg
         | AsmStoreOutArg
         | AsmStoreRet
         | AsmCall
         | AsmRet
         | AsmNew
         | AsmDel
         | AsmPrint
         | AsmPrintln
         | AsmRead
         | AsmMov
         | AsmMovi
         | AsmMoveq
         | AsmMovge
         | AsmMovgt
         | AsmMovle
         | AsmMovlt
         | AsmMovne
         deriving (Eq, Show)

ilocToAsm :: Iloc -> [Asm]
ilocToAsm iloc = undefined
