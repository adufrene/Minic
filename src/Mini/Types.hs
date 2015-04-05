{-# LANGUAGE OverloadedStrings #-}

module Mini.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.HashMap.Lazy
import Control.Applicative

type Id = String
type Arguments = [Expression]

data Program = Program { getTypes :: [Type]
                       , getDeclarations :: [Declaration]
                       , getFunctions :: [Function]
                       } deriving (Show)

data Type = Type { getTypeLine :: Int
                 , getTypeId :: Id
                 , getTypeFields :: [Field]
                 } deriving (Show)

data Field = Field { getFieldLine :: Int
                   , getFieldType :: String
                   , getFieldId :: Id
                   } deriving (Show)

data Declaration = Declaration { getDecLine :: Int
                               , getDecType :: String
                               , getDecId :: Id
                               } deriving (Show)

data Function = Function { getFunLine :: Int
                         , getFunId :: Id
                         , getFunParameters :: [Field]
                         , getFunDeclarations :: [Declaration]
                         , getFunBody :: [Statement]
                         , getFunReturnType :: String
                         } deriving (Show)

data Statement = Block { getBlockStmts :: [Statement] }
               | Asgn {  getAsgnLine :: Int
                       , getAsgnLValue :: LValue
                       , getAsgnExpr :: Expression }
               | Print { getPrintLine :: Int
                       , getPrintExpr :: Expression
                       , hasEndl :: Bool }
               | Read { getReadLine :: Int
                      , getReadLValue :: LValue }
               | Cond { getCondLine :: Int
                      , getCondExpr :: Expression
                      , getCondBlock :: Statement -- Block Statement
                      , getElseBlock :: Maybe Statement }
               | Loop { getLoopLine :: Int
                      , getLoopExpr :: Expression
                      , getLoopBlock :: Statement } -- Block Statement
               | Delete { getDelLine :: Int
                        , getDelExpr :: Expression }
               | Ret { getRetLine :: Int
                     , getRetExpr :: Maybe Expression }
               | Invocation { getSInvocLine :: Int
                            , getSInvocId :: Id
                            , getSInvocArgs :: Arguments } deriving (Show)

data LValue = LValue { getLValLine :: Maybe Int 
                     , getLValId :: Id
                     , getLValLeft :: Maybe LValue } deriving (Show)

data Expression = BinExp { getBinExpLine :: Int
                         , getBOp :: String
                         , getLeftExp :: Expression
                         , getRightExp :: Expression }
                | UExp {  getUOpLine :: Int
                        , getUOp :: String
                        , getUOpand :: Expression }
                | DotExp { getDotLine :: Int
                         , getDotLeft :: Expression
                         , getDotId :: Id }
                | InvocExp { getEInvocLine :: Int
                           , getEInvocId :: Id
                           , getEInvocArgs :: Arguments }
                | IdExp { getIdLine :: Int
                        , getId :: Id }
                | IntExp { getIntExpLine :: Int
                         , getValue :: Int }
                | TrueExp { getTrueExpLine :: Int}
                | FalseExp { getFalseExpLine :: Int}
                | NewExp { getNewLine :: Int
                         , getNewId :: Id }
                | NullExp { getNullLine :: Int } deriving (Show)



instance FromJSON Program where
        parseJSON (Object v) = 
            Program <$>
            (v .: "types") <*>
            (v .: "declarations") <*>
            (v .: "functions")
        parseJSON _ = undefined

instance FromJSON Function where
        parseJSON (Object v) =
            Function <$>
            (v .: "line") <*>
            (v .: "id") <*>
            (v .: "parameters") <*>
            (v .: "declarations") <*>
            (v .: "body") <*>
            (v .: "return_type")
        parseJSON _ = undefined

instance FromJSON Type where
        parseJSON (Object v) =
            Type <$>
            (v .: "line") <*>
            (v .: "id") <*>
            (v .: "fields")
        parseJSON _ = undefined

instance FromJSON Declaration where
        parseJSON (Object v) =
            Declaration <$>
            (v .: "line") <*>
            (v .: "type") <*>
            (v .: "id")
        parseJSON _ = undefined

instance FromJSON Field where
        parseJSON (Object v) =
            Field <$>
            (v .: "line") <*>
            (v .: "type") <*>
            (v .: "id")
        parseJSON _ = undefined

instance FromJSON Statement where
        parseJSON (Object v) =
             jsonToStatement (v ! "stmt") v
                where jsonToStatement "block" = parseBlock 
                      jsonToStatement "assign" = parseAsgn 
                      jsonToStatement "print" = parsePrint 
                      jsonToStatement "read" = parseRead 
                      jsonToStatement "if" = parseCond 
                      jsonToStatement "while" = parseLoop 
                      jsonToStatement "delete" = parseDelete 
                      jsonToStatement "return" = parseRet 
                      jsonToStatement "invocation" = parseInvoc 
                      jsonToStatement _ = undefined
        parseJSON _ = undefined

instance FromJSON LValue where
        parseJSON (Object v) =
            LValue <$> (v .:? "line") <*> (v .: "id") <*> (v .:? "left")
        parseJSON _ = undefined

instance FromJSON Expression where
        parseJSON (Object v) =
            jsonToExpression (v ! "exp") v
                where jsonToExpression "binary" = parseBinExp 
                      jsonToExpression "unary" = parseUExp 
                      jsonToExpression "dot" = parseDotExp 
                      jsonToExpression "invocation" = parseInvocExp
                      jsonToExpression "id" = parseIdExp
                      jsonToExpression "num" = parseNumExp
                      jsonToExpression "true" = parseTrueExp
                      jsonToExpression "false" = parseFalseExp
                      jsonToExpression "new" = parseNewExp
                      jsonToExpression "null" = parseNullExp
                      jsonToExpression _ = undefined
        parseJSON _ = undefined

parseBlock :: HashMap Text Value -> Parser Statement
parseBlock hm = Block <$> (hm .: "list")

parseAsgn :: HashMap Text Value -> Parser Statement
parseAsgn hm = Asgn <$> (hm .: "line") <*> (hm .: "target") <*> (hm .: "source")

parsePrint :: HashMap Text Value -> Parser Statement
parsePrint hm = Print <$> (hm .: "line") <*> (hm .: "exp") <*> (hm .: "endl")

parseRead :: HashMap Text Value -> Parser Statement
parseRead hm = Read <$> (hm .: "line") <*> (hm .: "target")

parseCond :: HashMap Text Value -> Parser Statement
parseCond hm = Cond <$> (hm .: "line") <*> (hm .: "guard") <*> (hm .: "then") <*> (hm .:? "else")

parseLoop :: HashMap Text Value -> Parser Statement
parseLoop hm = Loop <$> (hm .: "line") <*> (hm .: "guard") <*> (hm .: "body")

parseDelete :: HashMap Text Value -> Parser Statement
parseDelete hm = Delete <$> (hm .: "line") <*> (hm .: "guard")

parseRet :: HashMap Text Value -> Parser Statement
parseRet hm = Ret <$> (hm .: "line") <*> (hm .:? "exp")

parseInvoc :: HashMap Text Value -> Parser Statement
parseInvoc hm = Invocation <$> (hm .: "line") <*> (hm .: "id") <*> (hm .: "args")

parseBinExp :: HashMap Text Value -> Parser Expression
parseBinExp hm = BinExp <$> (hm .: "line") <*> (hm .: "operator") <*> (hm .: "lft") <*> (hm .: "rht")

parseUExp :: HashMap Text Value -> Parser Expression
parseUExp hm = UExp <$> (hm .: "line") <*> (hm .: "operator") <*> (hm .: "operand")

parseDotExp :: HashMap Text Value -> Parser Expression
parseDotExp hm = DotExp <$> (hm .: "line") <*> (hm .: "left") <*> (hm .: "id")

parseInvocExp :: HashMap Text Value -> Parser Expression
parseInvocExp hm = InvocExp <$> (hm .: "line") <*> (hm .: "id") <*> (hm .: "args")

parseIdExp :: HashMap Text Value -> Parser Expression
parseIdExp hm = IdExp <$> (hm .: "line") <*> (hm .: "id")

parseNumExp :: HashMap Text Value -> Parser Expression
parseNumExp hm = IntExp <$> (hm .: "line") <*> fmap read (hm .: "value")

parseTrueExp :: HashMap Text Value -> Parser Expression
parseTrueExp hm = TrueExp <$> (hm .: "line")

parseFalseExp :: HashMap Text Value -> Parser Expression
parseFalseExp hm = FalseExp <$> (hm .: "line")

parseNewExp :: HashMap Text Value -> Parser Expression
parseNewExp hm = NewExp <$> (hm .: "line") <*> (hm .: "id")

parseNullExp :: HashMap Text Value -> Parser Expression
parseNullExp hm = NullExp <$> (hm .: "line")
