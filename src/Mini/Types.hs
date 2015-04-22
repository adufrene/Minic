{-# LANGUAGE OverloadedStrings #-}

module Mini.Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.HashMap.Strict
import Data.Maybe

type Arguments = [Expression]
type Type = String
type Id = String

type DecHash = HashMap Id Type
type FunHash = HashMap Id ([Type], Type)
type StructHash = HashMap Type [Field]

type LocalEnv = DecHash
data GlobalEnv = GlobalEnv { getStructHash :: StructHash
                           , getDecsHash :: DecHash
                           , getFuncsHash :: FunHash} deriving (Show)

class HasLines a where
        getLineString :: a -> String

data YesNo a = Yes a | No a deriving (Show)

instance Functor YesNo where
        fmap f (Yes a) = Yes $ f a
        fmap f (No a) = No $ f a

instance Monad YesNo where
        return = Yes
        (Yes x) >>= f = f x
        (No x) >>= f = No $ fromYesNo $ f x

instance Applicative YesNo where
        pure = return
        (<*>) = ap

-- If 3rd arg is yes, run 1st function
-- else run 2nd function
yesNo :: (a -> b) -> (a -> b) -> YesNo a -> b
yesNo f _ (Yes a) = f a
yesNo _ f (No a) = f a

isYes :: YesNo a -> Bool
isYes (Yes _) = True
isYes (No _) = False

isNo :: YesNo a -> Bool
isNo = not . isYes

fromYesNo :: YesNo a -> a
fromYesNo (Yes x) = x
fromYesNo (No x) = x
-- Data Constructors --

data Program = Program { getTypes :: [TypeDef]
                       , getDeclarations :: [Declaration]
                       , getFunctions :: [Function]
                       } deriving (Show)

data TypeDef = TypeDef { getTypeLine :: Int
                       , getTypeId :: Id
                       , getTypeFields :: [Field]
                       } deriving (Show)

data Field = Field { getFieldLine :: Int
                   , getFieldType :: Type
                   , getFieldId :: Id
                   } deriving (Show)

data Declaration = Declaration { getDecLine :: Int
                               , getDecType :: Type
                               , getDecId :: Id
                               } deriving (Show)

data Function = Function { getFunLine :: Int
                         , getFunId :: Id
                         , getFunParameters :: [Field]
                         , getFunDeclarations :: [Declaration]
                         , getFunBody :: [Statement]
                         , getFunReturnType :: Type
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
               | InvocSt { getSInvocLine :: Int
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

-- FromJSON --

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

instance FromJSON TypeDef where
        parseJSON (Object v) =
            TypeDef <$>
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

-- ToJSON --

instance ToJSON Program where
  toJSON (Program t d f) = object ["types" .= t, "declarations" .= d, "functions" .= f]

instance ToJSON Function where
  toJSON (Function line funId parameters declarations body return_type) =
    object [ "line" .= line
           , "id" .= funId
           , "parameters" .= parameters
           , "declarations" .= declarations
           , "body" .= body
           , "return_type" .= return_type]

instance ToJSON TypeDef where
  toJSON (TypeDef line typeId fields) = object ["line" .= line, "id" .= typeId, "fields" .= fields]

instance ToJSON Declaration where
  toJSON (Declaration line dType dId) = object ["line" .= line, "type" .= dType, "id" .= dId]

instance ToJSON Field where
  toJSON (Field line fType fId) = object [ "line" .= line, "type" .= fType, "id" .= fId]

instance ToJSON LValue where
  toJSON (LValue maybeLine valId maybeLeft) =
    object (linePair ++ valIdPair ++ leftPair)
    where
      linePair
        | isJust maybeLine = ["line" .= fromJust maybeLine]
        | otherwise = []
      valIdPair = ["id" .= valId]
      leftPair
        | isJust maybeLeft = ["left" .= fromJust maybeLeft]
        | otherwise = []

instance ToJSON Statement where
  toJSON (Block list) = object ["stmt" .= pack "block", "list" .= list]
  toJSON (Asgn line target source) =
    object ["stmt" .= pack "assign"
           , "line" .= line
           , "target" .= target
           , "source" .= source]
  toJSON (Print line printExp endl) =
    object ["stmt" .= pack "print", "line" .= line, "exp" .= printExp, "endl" .= endl]
  toJSON (Read line target) = object ["stmt" .= pack "read", "line" .= line, "target" .= target]
  toJSON (Cond line guard condThen maybeElse) =
    object (restL ++ elsePair)
    where
      elsePair
        | isJust maybeElse = ["else" .= fromJust maybeElse]
        | otherwise = []
      restL = ["stmt" .= pack "if", "line" .= line, "guard" .= guard, "then" .= condThen]
  toJSON (Loop line guard body) =
    object ["stmt" .= pack "while"
           , "line" .= line
           , "guard" .= guard
           , "body" .= body]
  toJSON (Delete line guard) = object ["stmt" .= pack "delete", "line" .= line, "guard" .= guard]
  toJSON (Ret line maybeExp) =
    object (restL ++ expPair)
    where
      restL = ["stmt" .= pack "return" , "line" .= line]
      expPair
        | isJust maybeExp = ["exp" .= fromJust maybeExp]
        | otherwise = []
  toJSON (InvocSt line invocId args) =
    object [ "stmt" .= pack "invocation"
           , "line" .= line
           , "id" .= invocId
           , "args" .= args]

instance ToJSON Expression where
  toJSON (BinExp line operator lft rht) =
    object [ "exp" .= pack "binary"
           , "line" .= line
           , "operator" .= operator
           , "lft" .= toJSON lft
           , "rht" .= toJSON rht]
  toJSON (UExp line operator operand) =
    object [ "exp" .= pack "unary", "line" .= line, "operator" .= operator, "operand" .= operand]
  toJSON (DotExp line left dotId) =
    object [ "exp" .= pack "dot", "line" .= line, "left" .= left, "id" .= dotId]
  toJSON (InvocExp line invocId args) =
    object [ "exp" .= pack "invocation", "line" .= line, "id" .= invocId, "args" .= args]
  toJSON (IdExp line idId) = object ["exp" .= pack "id", "line" .= line, "id" .= idId]
  toJSON (IntExp line value) = object ["exp" .= pack "num", "line" .= line, "value" .= show value]
  toJSON (TrueExp line) = object ["exp" .= pack "true", "line" .= line]
  toJSON (FalseExp line) = object ["exp" .= pack "false", "line" .= line]
  toJSON (NewExp line newId) = object ["exp" .= pack "new", "line" .= line, "id" .= newId]
  toJSON (NullExp line) = object ["exp" .= pack "null", "line" .= line]

-- HasLines --

instance HasLines TypeDef where
        getLineString = show . getTypeLine 

instance HasLines Field where
        getLineString = show . getFieldLine

instance HasLines Declaration where
        getLineString = show . getDecLine

instance HasLines Function where
        getLineString = show . getFunLine

instance HasLines Statement where
        getLineString (Block (x:_)) = getLineString x
        getLineString stmt@Block{} = "Unknown line"
        getLineString stmt@Asgn{} = show $ getAsgnLine stmt
        getLineString stmt@Print{} = show $ getPrintLine stmt
        getLineString stmt@Read{} = show $ getReadLine stmt
        getLineString stmt@Cond{} = show $ getCondLine stmt
        getLineString stmt@Loop{} = show $ getLoopLine stmt
        getLineString stmt@Delete{} = show $ getDelLine stmt
        getLineString stmt@Ret{} = show $ getRetLine stmt
        getLineString stmt@InvocSt{} = show $ getSInvocLine stmt

instance HasLines LValue where
        getLineString val = toString $ getLValLine val
            where toString (Just line) = show line
                  toString Nothing = "Unknown line"

instance HasLines Expression where
        getLineString expr@BinExp{} = show $ getBinExpLine expr
        getLineString expr@UExp{} = show $ getUOpLine expr
        getLineString expr@DotExp{} = show $ getDotLine expr
        getLineString expr@InvocExp{} = show $ getEInvocLine expr
        getLineString expr@IdExp{} = show $ getIdLine expr
        getLineString expr@IntExp{} = show $ getIntExpLine expr
        getLineString expr@TrueExp{} = show $ getTrueExpLine expr
        getLineString expr@FalseExp{} = show $ getFalseExpLine expr
        getLineString expr@NewExp{} = show $ getNewLine expr
        getLineString expr@NullExp{} = show $ getNullLine expr

-- Helpers --

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
parseInvoc hm = InvocSt <$> (hm .: "line") <*> (hm .: "id") <*> (hm .: "args")

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

