{
module Mini.Parser (parse) where

import Control.Applicative ((<$>))
import Data.Char
import Data.List (find)
import Data.Maybe

import Mini.Types
}

%name calc
%tokentype { Token }
%error { parseError }

%monad { P } { thenP } { returnP }
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
--    eof         { TokenEOF }
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

Program :: { Program }
    : Types Declarations Functions {- eof -}         { Program (reverse $1) (reverse $2) (reverse $3) }

Types :: { [TypeDef] }
    : {- empty -}                               { [] }
    | Types TypeSub                             { $2 : $1 }

TypeSub :: { TypeDef }
    : struct id '{' NestedDecl '}' ';'   { TypeDef 0 $2 $ reverse $4 }

NestedDecl :: { [Field] }
    : {- empty -}                               { [] }
    | NestedDecl Decl ';'                       { $2 : $1 }

Decl :: { Field }
    : Type id                            { Field 0 $1 $2 }

Type :: { Type }
    : int                                       { intType }
    | bool                                      { boolType }
    | struct id                                 { $2 }

Declarations :: { [Declaration] }
    : {- empty -}                               { [] }
-- This rule matches empty on empty and on Lineno
    | Declarations Declaration                  { $2 ++ $1 }

Declaration :: { [Declaration] }
    : Type IdList ';'                    { map (Declaration 0 $1) $2 }

IdList :: { [Id] }
    : id                                        { [$1] }
    | IdList ',' id                             { $3 : $1 }

Functions :: { [Function] }
    : {- empty -}                               { [] }
    | Functions Function                        { $2 : $1 }

Function :: { Function }
    : fun id Parameters RetType '{' 
        Declarations StatementList 
      '}'                                       { Function 0 $2 (reverse $3) (reverse $6) (reverse $7) $4 }

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
    : LValue '=' Expression ';'          { Asgn 0 $1 $3 }

Print :: { Statement }
    : print Expression Endl ';'          { Print 0 $2 $3 }

Endl :: { Bool }
    : {- empty -}                               { False }
    | endl                                      { True }

Read :: { Statement }
    : read LValue ';'                    { Read 0 $2 }

Conditional :: { Statement }
    : if '(' Expression ')' 
        Block ElseBlock                         { Cond 0 $3 $5 $6 }

ElseBlock :: { Maybe Statement }
    : {- empty -}                               { Nothing }
    | else Block                                { Just $2 }

Loop :: { Statement }
    : while '(' Expression ')' Block     { Loop 0 $3 $5 }

Delete :: { Statement }
    : delete Expression ';'              { Delete 0 $2 }

Ret :: { Statement }
    : return MaybeExpr ';'               { Ret 0 $2 }

MaybeExpr :: { Maybe Expression }
    : {- empty -}                               { Nothing }
    | Expression                                { Just $1 }

Invocation :: { Statement }
    : id Arguments ';'                   { InvocSt 0 $1 $ reverse $2 }

LValue :: { LValue }
    : id                                 { LValue Nothing $1 Nothing }
    | LValue '.' id                      { LValue (Just 0) $3 (Just $1)} 

Expression :: { Expression }
    : Boolterm                                  { $1 }
    | Expression boolOp Boolterm           { BinExp 0 $2 $1 $3 }

Boolterm :: { Expression }
    : Simple                                    { $1 }
    | Boolterm cmpOp Simple                { BinExp 0 $2 $1 $3 }

Simple :: { Expression }
    : Term                                      { $1 }
    | Simple '+' Term                      { BinExp 0 "+" $1 $3 }
    | Simple '-' Term                      { BinExp 0 "-" $1 $3 }

Term :: { Expression }
    : Unary                                     { $1 }
    | Term '*' Unary                    { BinExp 0 "*" $1 $3 }
    | Term '/' Unary                    { BinExp 0 "/" $1 $3 }

Unary :: { Expression }
    : Selector                                  { $1 }
    | '!' Unary                       { UExp 0 "!" $2 }
    | '-' Unary %prec NEG             { UExp 0 "-" $2 }

Selector :: { Expression }
    : Factor                                    { $1 }
    | Selector '.' id                    { DotExp 0 $1 $3 }

Factor :: { Expression }
    : '(' Expression ')'                        { $2 }
    | id                                 { IdExp 0 $1 }
    | id Arguments                       { InvocExp 0 $1 $ reverse $2 }
    | num                                { IntExp 0 $1 }
    | true                               { TrueExp 0 }
    | false                              { FalseExp 0 }
    | new id                             { NewExp 0 $2 }
    | null                               { NullExp 0 }

Arguments :: { [Expression] }
    : '('')'                                    { [] }
    | '(' ExprList ')'                          { $2}

ExprList :: { [Expression] }
    : Expression                                { [$1] }
    | ExprList ',' Expression                  { $3 : $1 }

{-Lineno ::  { LineNumber }
    : {- empty -}                               {% getLineNo }
-}

{
data ParseResult a = Ok a | Failed String
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

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
keywords = [
    ("struct", TokenStruct),
    ("int", TokenInt),
    ("bool", TokenBool),
    ("fun", TokenFun),
    ("void", TokenVoid),
    ("print", TokenPrint),
    ("read", TokenRead),
    ("if", TokenIf),
    ("else", TokenElse),
    ("while", TokenWhile),
    ("delete", TokenDelete),
    ("return", TokenReturn),
    ("true", TokenTrue),
    ("false", TokenFalse),
    ("new", TokenNew),
    ("null", TokenNull),
    ("endl", TokenEndl)
    ]

charTks :: [(Char, Token)]
charTks = [
    ('=', TokenEq),
    ('+', TokenPlus),
    ('-', TokenMinus),
    ('*', TokenTimes),
    ('/', TokenDiv),
    ('(', TokenOB),
    (')', TokenCB),
    ('{', TokenLBrace),
    ('}', TokenRBrace),
    (';', TokenSemi),
    (',', TokenComma),
    ('.', TokenDot),
    ('<', TokenCmpOp "<"),
    ('>', TokenCmpOp ">"),
    ('!', TokenBang)
    ]

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
thenP m k = \s l ->
    case m s l of
        Ok a -> k a s l
        Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l -> Ok a

failP :: String -> P a
failP a = \s l -> Failed a

parseError :: Token -> P a
parseError tk = getLineNo `thenP` \line ->
                        failP (show line ++ ": parse error at "
                                ++ (show tk))

lexer :: (Token -> P a) -> P a
lexer cont s = case s of
        [] -> cont TokenEOF []
        ('\n':cs) -> \line -> lexer cont cs (line + 1)
        ('#':cs) -> lexer cont $ dropWhile ((/=) '\n') cs
        (c:cs)
            | isSpace c -> lexer cont cs
            | isAlpha c -> lexText cont (c:cs)
            | isDigit c -> lexNum cont (c:cs)
        ('<':'=':cs) -> cont (TokenCmpOp "<=") cs       -- <=
        ('>':'=':cs) -> cont (TokenCmpOp ">=") cs       -- >=
        ('=':'=':cs) -> cont (TokenCmpOp "==") cs       -- ==
        ('!':'=':cs) -> cont (TokenCmpOp "!=") cs       -- !=
        ('&':'&':cs) -> cont (TokenBoolOp "&&") cs      -- &&
        ('|':'|':cs) -> cont (TokenBoolOp "||") cs      -- ||
        (c:cs)
            | isJust (charTk c)-> cont (fromJust $ charTk c) cs
            | otherwise -> failP ("Unexpected token " ++ [c]) s
    where charTk c = snd <$> find ((==) c . fst) charTks

lexNum :: (Token -> P a) -> P a
lexNum cont s = cont (TokenNum (read num)) rest
    where (num, rest) = span isDigit s

lexText :: (Token -> P a) -> P a
lexText cont s = cont
        (fromMaybe (TokenId word) (snd <$> find ((==) word . fst) keywords)) rest
    where (word, rest) = span (\x -> isAlpha x || isDigit x) s
 
fromParseResult :: ParseResult a -> a
fromParseResult (Ok a) = a
fromParseResult (Failed s) = error s

parse :: String -> Program
parse fileString = fromParseResult $ calc fileString 1

}

