{-# LANGUAGE OverloadedStrings #-}

module Mini.Types where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative

data Program = Program { getTypes :: [Type]
                       , getDeclarations :: [Declaration]
                       , getFunctions :: [Function]
                       } deriving (Show)

data Type = Type { getTypeLine :: Int
                 , getTypeId :: String
                 , getTypeFields :: [Field]
                 } deriving (Show)

data Field = Field { getFieldLine :: Int
                   , getFieldType :: String
                   , getFieldId :: Int
                   } deriving (Show)

data Declaration = Declaration { getDecLine :: Int
                               , getDecType :: String
                               , getDecId :: Int
                               } deriving (Show)

data Function = Function { getFunLine :: Int
                         , getFunId :: String
                         , getFunParameters :: [Field]
                         , getFunDeclarations :: [Declaration]
                         , getFunBody :: [Statement]
                         } deriving (Show)

data Statement = Block { getBlockStmts :: [Statement] }
               | Asgn { getAsgnLValue :: LValue
                       , getAsgnExpr :: Expression }
               | Print { getPrintExpr :: Expression
                       , hasEndl :: Bool }
               | Read { getReadLValue :: LValue }
               | Cond { getCondExpr :: Expression
                      , getCondBlock :: Statement -- Block Statement
                      , getElseBlock :: Maybe Statement }
               | Loop { getLoopExpr :: Expression
                      , getLoopBlock :: Statement } -- Block Statement
               | Delete { getDelExpr :: Expression }
               | Ret { getRetExpr :: Maybe Expression }
               | Invocation { getArguments :: [Argument] }
                       deriving (Show)

data LValue = LValue deriving (Show)
data Expression = Expression deriving (Show)
data Argument = Argument deriving (Show)

instance FromJSON Program where
        parseJSON (Object v) = 
            Program <$>
            (v .: "types") <*>
            (v .: "declarations") <*>
            (v .: "functions")

instance FromJSON Function where
        parseJSON (Object v) =
            Function <$>
            (v .: "line") <*>
            (v .: "id") <*>
            (v .: "parameters") <*>
            (v .: "declarations") <*>
            (v .: "body")

instance FromJSON Type where
        parseJSON (Object v) =
            Type <$>
            (v .: "line") <*>
            (v .: "id") <*>
            (v .: "fields")

instance FromJSON Declaration where
        parseJSON (Object v) =
            Declaration <$>
            (v .: "line") <*>
            (v .: "type") <*>
            (v .: "id")

instance FromJSON Field where
        parseJSON (Object v) =
            Field <$>
            (v .: "line") <*>
            (v .: "type") <*>
            (v .: "id")

-- Here down is wip
instance FromJSON Statement where
       parseJSON obj@(Object v) =
--             jsonToStatement (parse (\obj -> (obj .: "stmt")) v) v
             jsonToStatement "block" obj
                where jsonToStatement "block" obj = parseBlock obj
                      jsonToStatement "assign" obj = parseAsgn obj
                      jsonToStatement "print" obj = parsePrint obj
                      jsonToStatement "read" obj = parseRead obj
                      jsonToStatement "if" obj = parseCond obj
                      jsonToStatement "while" obj = parseLoop obj
                      jsonToStatement "delete" obj = parseDelete obj
                      jsonToStatement "return" obj = parseRet obj
                      jsonToStatement "invocation" obj = parseInvoc obj

-- Temporarily filling in data, probably not accurate
parseBlock :: Value -> Parser Statement
parseBlock (Object v) = Block <$> (v .: "block")

parseAsgn :: Value -> Parser Statement
parseAsgn (Object v) = Asgn <$> (v .: "id") <*> (v .: "expr")

parsePrint :: Value -> Parser Statement
parsePrint (Object v) = Print <$> (v .: "expr") <*> (v .: "hasEndl")

parseRead :: Value -> Parser Statement
parseRead (Object v) = Read <$> (v .: "value")

parseCond :: Value -> Parser Statement
parseCond obj@(Object v) = Cond <$> (v .: "expr") <*> (parseBlock obj) <*> (v .: "else")

parseLoop :: Value -> Parser Statement
parseLoop obj@(Object v) = Loop <$> (v .: "expr") <*> (parseBlock obj)

parseDelete :: Value -> Parser Statement
parseDelete (Object v) = Delete <$> (v .: "expr")

parseRet :: Value -> Parser Statement
parseRet (Object v) = Ret <$> (v .:? "expr")

parseInvoc :: Value -> Parser Statement
parseInvoc (Object v) = Invocation <$> (v .: "args")
