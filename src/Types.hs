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

data Type = Type { getTypeId :: Id
                 , getTypeFields :: [Field]
                 } deriving (Show)

data Field = Field { getFieldType :: String
                   , getFieldId :: Id
                   } deriving (Show)

data Declaration = Declaration { getDecType :: String
                               , getDecId :: Id
                               } deriving (Show)

data Function = Function { getFunId :: Id
                         , getFunParameters :: [Field]
                         , getFunDeclarations :: [Declaration]
                         , getFunBody :: [Statement]
                         , getFunReturnType :: String
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
               | Invocation { getSInvocId :: Id
                            , getSInvocArgs :: Arguments } deriving (Show)

data LValue = LValue { getLValId :: Id
                     , getLValLeft :: Maybe LValue } deriving (Show)

data Expression = BinExp { getBOp :: String
                         , getLeftExp :: Expression
                         , getRightExp :: Expression }
                | UExp { getUOp :: String
                        , getUOpand :: Expression }
                | DotExp { getDotLeft :: Expression
                         , getDotId :: Id }
                | InvocExp { getEInvocId :: Id
                           , getEInvocArgs :: Arguments }
                | IdExp { getId :: Id }
                | IntExp { getValue :: Int }
                | TrueExp
                | FalseExp
                | NewExp { getNewId :: Id }
                | NullExp deriving (Show)



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
            (v .: "id") <*>
            (v .: "parameters") <*>
            (v .: "declarations") <*>
            (v .: "body") <*>
            (v .: "return_type")
        parseJSON _ = undefined

instance FromJSON Type where
        parseJSON (Object v) =
            Type <$>
            (v .: "id") <*>
            (v .: "fields")
        parseJSON _ = undefined

instance FromJSON Declaration where
        parseJSON (Object v) =
            Declaration <$>
            (v .: "type") <*>
            (v .: "id")
        parseJSON _ = undefined

instance FromJSON Field where
        parseJSON (Object v) =
            Field <$>
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
            LValue <$> (v .: "id") <*> (v .:? "left")
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
parseAsgn hm = Asgn <$> (hm .: "target") <*> (hm .: "source")

parsePrint :: HashMap Text Value -> Parser Statement
parsePrint hm = Print <$> (hm .: "exp") <*> (hm .: "endl")

parseRead :: HashMap Text Value -> Parser Statement
parseRead hm = Read <$> (hm .: "target")

parseCond :: HashMap Text Value -> Parser Statement
parseCond hm = Cond <$> (hm .: "guard") <*> (hm .: "then") <*> (hm .:? "else")

parseLoop :: HashMap Text Value -> Parser Statement
parseLoop hm = Loop <$> (hm .: "guard") <*> (hm .: "body")

parseDelete :: HashMap Text Value -> Parser Statement
parseDelete hm = Delete <$> (hm .: "guard")

parseRet :: HashMap Text Value -> Parser Statement
parseRet hm = Ret <$> (hm .:? "exp")

parseInvoc :: HashMap Text Value -> Parser Statement
parseInvoc hm = Invocation <$> (hm .: "id") <*> (hm .: "args")

parseBinExp :: HashMap Text Value -> Parser Expression
parseBinExp hm = BinExp <$> (hm .: "operator") <*> (hm .: "lft") <*> (hm .: "rht")

parseUExp :: HashMap Text Value -> Parser Expression
parseUExp hm = UExp <$> (hm .: "operator") <*> (hm .: "operand")

parseDotExp :: HashMap Text Value -> Parser Expression
parseDotExp hm = DotExp <$> (hm .: "left") <*> (hm .: "id")

parseInvocExp :: HashMap Text Value -> Parser Expression
parseInvocExp hm = InvocExp <$> (hm .: "id") <*> (hm .: "args")

parseIdExp :: HashMap Text Value -> Parser Expression
parseIdExp hm = IdExp <$> (hm .: "id")

parseNumExp :: HashMap Text Value -> Parser Expression
parseNumExp hm = IntExp . read <$> (hm .: "value")

parseTrueExp :: HashMap Text Value -> Parser Expression
parseTrueExp _ = return TrueExp

parseFalseExp :: HashMap Text Value -> Parser Expression
parseFalseExp _ = return FalseExp

parseNewExp :: HashMap Text Value -> Parser Expression
parseNewExp hm = NewExp <$> (hm .: "id")

parseNullExp :: HashMap Text Value -> Parser Expression
parseNullExp _ = return NullExp
