{
module Mini.Parser (parse) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Char
import Data.Either
import Data.List (find)
import Data.Maybe

import Mini.Types
}

%name calc
%tokentype { Token }

%monad { P }
%lexer { lexer } { TokenEOF }

%token
    struct      { TokenStruct }
    int         { TokenInt }
    bool        { TokenBool }
    fun         { TokenFun }
    void        { TokenVoid }
    print       { TokenPrint }
    endl        { TokenEndl }
    read        { TokenRead }
    if          { TokenIf }
    else        { TokenElse }
    while       { TokenWhile }
    delete      { TokenDelete }
    return      { TokenReturn }
    true        { TokenTrue }
    false       { TokenFalse }
    new         { TokenNew }
    null        { TokenNull } 
    id          { TokenId $$ }
    boolOp      { TokenBoolOp $$ }
    cmpOp       { TokenCmpOp $$ }
    num         { TokenNum $$ }
    '='         { TokenEq }
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenTimes }
    '/'         { TokenDiv }
    '('         { TokenOB }
    ')'         { TokenCB }
    '{'         { TokenLBrace }
    '}'         { TokenRBrace }
    ';'         { TokenSemi }
    ','         { TokenComma }
    '.'         { TokenDot }
    '!'         { TokenBang }

%right '='
%left '+' '-'
%left '*' '/'
%left NEG
%left '.'

%%

{-
- Combine Types & Declarations in grammar into {TypeDecls}*, create data type
- for TypeDecl with two data constructors, Type & Declaration. TypeDecl =
- TypeSub | Declaration. When creating Program, split [TypeDecls] by type
- constructor into type and Decls. 
-}

Program :: { Program }
    : TypeDecls Functions { 
                            let (types,decls) = fromTypeDecls $1
                            in Program (reverse types) (reverse decls) (reverse $2) 
                          }


TypeDecls :: { [TypeDecl] }
    : {- empty -}                               { [] }
    | TypeDecls TypeDecl                        { $2 ++ $1 }

TypeDecl :: { [TypeDecl] }
    : struct id TypeOrDec                       { $3 $2 }
    | DecType IdList ';'                        {% lineP >>= \l -> return $ fmap (Decl l $1) $2 }

NestedDecl :: { [Field] }
    : {- empty -}                               { [] }
    | NestedDecl Decl ';'                       { $2 : $1 }

Decl :: { Field }
    : Type id                                   {% lineP >>= \l -> return $ Field l $1 $2 }

Type :: { Type }
    : int                                       { intType }
    | bool                                      { boolType }
    | struct id                                 { $2 }

DecType :: { Type }
    : int                                       { intType }
    | bool                                      { boolType }

TypeOrDec :: { Id -> [TypeDecl] }
    : '{' NestedDecl '}' ';'                    {% lineP >>= \l -> return 
                                                    $ \x -> [TDef l x $ reverse $2] }
    | IdList ';'                                {% lineP >>= \l -> return 
                                                    $ \x -> fmap (Decl 0 x) $1 }

Declarations :: { [Declaration] }
    : {- empty -}                               { [] }
    | Declarations Declaration                  { $2 ++ $1 }

Declaration :: { [Declaration] }
    : Type IdList ';'                           {% lineP >>= \l -> return $ fmap (Declaration l $1) $2 }

IdList :: { [Id] }
    : id                                        { [$1] }
    | IdList ',' id                             { $3 : $1 }

Functions :: { [Function] }
    : {- empty -}                               { [] }
    | Functions Function                        { $2 : $1 }

Function :: { Function }
    : fun id Parameters RetType '{' 
        Declarations StatementList 
      '}'                                       {% lineP >>= \l -> return $ 
                                                    Function l $2 (reverse $3) (reverse $6) (reverse $7) $4 }

Parameters :: { [Field] }
    : '('')'                                    { [] }
    | '(' DeclList ')'                          { $2 }

DeclList :: { [Field] }
    : Decl                                      { [$1] }
    | DeclList ',' Decl                         { $3 : $1 }

RetType :: { Type }
    : Type                                      { $1 }
    | void                                      { voidType }

Statement :: { Statement }
    : Block                                     { $1 }
    | Assignment                                { $1 }
    | Print                                     { $1 }
    | Read                                      { $1 }
    | Conditional                               { $1 }
    | Loop                                      { $1 }
    | Delete                                    { $1 }
    | Ret                                       { $1 }
    | Invocation                                { $1 }

Block :: { Statement }
    : '{' StatementList '}'                     { Block (reverse $2) }

StatementList :: { [Statement] }
    : {- empty -}                               { [] }
    | StatementList Statement                   { $2 : $1 }

Assignment :: { Statement }
    : LValue '=' Expression ';'          {% lineP >>= \l -> return $ Asgn l $1 $3 }

Print :: { Statement }
    : print Expression Endl ';'          {% lineP >>= \l -> return $ Print l $2 $3 }

Endl :: { Bool }
    : {- empty -}                               { False }
    | endl                                      { True }

Read :: { Statement }
    : read LValue ';'                    {% lineP >>= \l -> return $ Read l $2 }

Conditional :: { Statement }
    : if '(' Expression ')' 
        Block ElseBlock                         {% lineP >>= \l -> return $ Cond l $3 $5 $6 }

ElseBlock :: { Maybe Statement }
    : {- empty -}                               { Nothing }
    | else Block                                { Just $2 }

Loop :: { Statement }
    : while '(' Expression ')' Block     {% lineP >>= \l -> return $ Loop l $3 $5 }

Delete :: { Statement }
    : delete Expression ';'              {% lineP >>= \l -> return $ Delete l $2 }

Ret :: { Statement }
    : return MaybeExpr ';'               {% lineP >>= \l -> return $ Ret l $2 }

MaybeExpr :: { Maybe Expression }
    : {- empty -}                               { Nothing }
    | Expression                                { Just $1 }

Invocation :: { Statement }
    : id Arguments ';'                   {% lineP >>= \l -> return $ InvocSt l $1 $ reverse $2 }

LValue :: { LValue }
    : id                                 { LValue Nothing $1 Nothing }
    | LValue '.' id                      {% lineP >>= \l -> return $ LValue (Just l) $3 (Just $1)} 

Expression :: { Expression }
    : Boolterm                                  { $1 }
    | Expression boolOp Boolterm           {% lineP >>= \l -> return $ BinExp l $2 $1 $3 }

Boolterm :: { Expression }
    : Simple                                    { $1 }
    | Boolterm cmpOp Simple                {% lineP >>= \l -> return $ BinExp l $2 $1 $3 }

Simple :: { Expression }
    : Term                                      { $1 }
    | Simple '+' Term                      {% lineP >>= \l -> return $ BinExp l "+" $1 $3 }
    | Simple '-' Term                      {% lineP >>= \l -> return $ BinExp l "-" $1 $3 }

Term :: { Expression }
    : Unary                                     { $1 }
    | Term '*' Unary                    {% lineP >>= \l -> return $ BinExp l "*" $1 $3 }
    | Term '/' Unary                    {% lineP >>= \l -> return $ BinExp l "/" $1 $3 }

Unary :: { Expression }
    : Selector                                  { $1 }
    | '!' Unary                       {% lineP >>= \l -> return $ UExp l "!" $2 }
    | '-' Unary %prec NEG             {% lineP >>= \l -> return $ UExp l "-" $2 }

Selector :: { Expression }
    : Factor                                    { $1 }
    | Selector '.' id                    {% lineP >>= \l -> return $ DotExp l $1 $3 }

Factor :: { Expression }
    : '(' Expression ')'                        { $2 }
    | id                                 {% lineP >>= \l -> return $ IdExp l $1 }
    | id Arguments                       {% lineP >>= \l -> return $ InvocExp l $1 $ reverse $2 }
    | num                                {% lineP >>= \l -> return $ IntExp l $1 }
    | true                               {% lineP >>= \l -> return $ TrueExp l }
    | false                              {% lineP >>= \l -> return $ FalseExp l }
    | new id                             {% lineP >>= \l -> return $ NewExp l $2 }
    | null                               {% lineP >>= \l -> return $ NullExp l }

Arguments :: { [Expression] }
    : '('')'                                    { [] }
    | '(' ExprList ')'                          { $2}

ExprList :: { [Expression] }
    : Expression                                { [$1] }
    | ExprList ',' Expression                  { $3 : $1 }

{

data TypeDecl = TDef Int Id [Field]
              | Decl Int Type Id

type ParseResult = Either String
type P a = ReaderT (String, Int) ParseResult a

data Token = TokenStruct
           | TokenInt
           | TokenBool
           | TokenFun
           | TokenVoid
           | TokenPrint
           | TokenEndl
           | TokenRead
           | TokenIf
           | TokenElse
           | TokenWhile
           | TokenDelete
           | TokenReturn
           | TokenTrue
           | TokenFalse
           | TokenNew
           | TokenNull
           | TokenEOF
           | TokenId Id
           | TokenBoolOp String 
           | TokenCmpOp String
           | TokenNum Int
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenOB
           | TokenCB
           | TokenLBrace
           | TokenRBrace
           | TokenSemi
           | TokenComma
           | TokenDot
           | TokenBang
           deriving (Show)

keywords :: [(String, Token)]
keywords = [ ("struct", TokenStruct)
           , ("int", TokenInt)
           , ("bool", TokenBool)
           , ("fun", TokenFun)
           , ("void", TokenVoid)
           , ("print", TokenPrint)
           , ("read", TokenRead)
           , ("if", TokenIf)
           , ("else", TokenElse)
           , ("while", TokenWhile)
           , ("delete", TokenDelete)
           , ("return", TokenReturn)
           , ("true", TokenTrue)
           , ("false", TokenFalse)
           , ("new", TokenNew)
           , ("null", TokenNull)
           , ("endl", TokenEndl) ]
    

charTks :: [(Char, Token)]
charTks = [ ('=', TokenEq)
          , ('+', TokenPlus)
          , ('-', TokenMinus)
          , ('*', TokenTimes)
          , ('/', TokenDiv)
          , ('(', TokenOB)
          , (')', TokenCB)
          , ('{', TokenLBrace)
          , ('}', TokenRBrace)
          , (';', TokenSemi)
          , (',', TokenComma)
          , ('.', TokenDot)
          , ('<', TokenCmpOp "<")
          , ('>', TokenCmpOp ">")
          , ('!', TokenBang) ]

fromTypeDecls :: [TypeDecl] -> ([TypeDef], [Declaration])
fromTypeDecls = foldr foldFun ([],[])
    where foldFun (TDef l i f) (ts, ds) = ((TypeDef l i f):ts, ds)
          foldFun (Decl l t i) (ts, ds) = (ts, (Declaration l t i):ds)

mkP :: (String -> Int -> ParseResult a) -> P a
mkP = ReaderT . uncurry

runP :: P a -> String -> Int -> ParseResult a
runP f s l = runReaderT f (s, l)

lineP :: P Int
lineP = asks snd >>= return

happyError :: P a
happyError = lineP >>= \l -> fail (show l ++ ": Parse error")

lexer :: (Token -> P a) -> P a
lexer cont = mkP lexer'
    where lexer' [] = returnToken cont TokenEOF []
          lexer' ('#':cs) = lexer' (dropWhile (/= '\n') cs)
          lexer' s = nextLex cont s

returnToken :: (t -> P a) -> t -> String -> Int -> ParseResult a
returnToken cont tok = runP (cont tok)

nextLex :: (Token -> P a) -> String -> Int -> ParseResult a
nextLex cont s = case s of
        ('\n':cs) -> \line -> returnToken lexer cont cs (line+1)
        (c:cs)
            | isSpace c -> runP (lexer cont) cs
            | isAlpha c -> lexText cont (c:cs)
            | isDigit c -> lexNum cont (c:cs)
        ('<':'=':cs) -> returnToken cont (TokenCmpOp "<=") cs
        ('>':'=':cs) -> returnToken cont (TokenCmpOp ">=") cs
        ('=':'=':cs) -> returnToken cont (TokenCmpOp "==") cs
        ('!':'=':cs) -> returnToken cont (TokenCmpOp "!=") cs
        ('&':'&':cs) -> returnToken cont (TokenBoolOp "&&") cs
        ('|':'|':cs) -> returnToken cont (TokenBoolOp "||") cs
        (c:cs)
            | isJust (charTk c) -> returnToken cont (fromJust $ charTk c) cs
            | otherwise -> lexError ("Unexpected token " ++ [c]) s
    where charTk c = snd <$> find ((==) c . fst) charTks

lexNum :: (Token -> P a) -> String -> Int -> ParseResult a
lexNum cont s = returnToken cont (TokenNum (read num)) rest
    where (num, rest) = span isDigit s

lexText :: (Token -> P a) -> String -> Int -> ParseResult a
lexText cont s = returnToken cont
        (fromMaybe (TokenId word) (snd <$> find ((==) word . fst) keywords)) rest
    where (word, rest) = span (\x -> isAlpha x || isDigit x) s

lexError :: String -> String -> Int -> ParseResult a
lexError err = runP (lineP >>= \l -> fail (show l ++ ": " ++ err))
 
parse :: String -> Program
parse fileString = either (\x -> error x) id $ runP calc fileString 1

}

