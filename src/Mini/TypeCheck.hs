module TypeCheck where

import Mini.Types
import Data.HashMap.Lazy

readTypes :: Program -> HashMap Id [Field]
readTypes prog = foldl foldFun empty $ getTypes prog
    where foldFun hash (TypeDef _ id fields) = 
              let newHash = insert id fields hash
                  isValid = all $ fmap (\field -> (getFieldType field) `member` newHash) fields
              in if isValid then newHash else error $ show getTypeLine ++ ": Type contains field of undeclared type" 

readDecls :: Program -> HashMap Id [Field] -> HashMap Id Type
readDecls prog hash = foldl foldFun empty $ getDeclarations prog
    where foldFun newHash (Declaration line decType id)
            | decType == "int" || decType == "bool" = insert id decType newHash
            | otherwise = if decType `member` hash 
                            then insert id decType newHash 
                            else error $ show line ++ ": use of undefined type"

readFuncs :: Program -> (HashMap Id [Field], HashMap Id Type) -> HashMap Id Type
readFuncs prog (typeHash, decHash) = empty
