{
module Mini.Parser where

import Control.Applicative ((<$>))
import Data.Char
import Data.List (find)
import Data.Maybe

import Mini.Types
}

%name parse
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
    eof         { TokenEOF }
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
    : Types Declarations Functions eof          { Program $1 $2 $3 }

Types :: { [TypeDef] }
    : {- empty -}                               { [] }
    | Types TypeSub                             { $2 : $1 }

TypeSub :: { TypeDef }
    : struct Lineno id '{' NestedDecl '}' ';'   { TypeDef $2 $3 $5 }

NestedDecl :: { [Field] }
    : {- empty -}                               { [] }
    | NestedDecl Decl ';'                       { $2 : $1 }

Decl :: { Field }
    : Lineno Type id                            { Field $1 $2 $3 }

Type :: { Type }
    : int                                       { intType }
    | bool                                      { boolType }
    | struct id                                 { $2 }

Declarations :: { [Declaration] }
    : {- empty -}                               { [] }
-- This rule matches empty on empty and on Lineno
    | Declarations Declaration                  { $2 ++ $1 }

Declaration :: { [Declaration] }
    : Lineno Type IdList ';'                    { map (Declaration $1 $2) $3 }

IdList :: { [Id] }
    : id                                        { [$1] }
    | IdList ',' id                             { $3 : $1 }

Functions :: { [Function] }
    : {- empty -}                               { [] }
    | Functions Function                        { $2 : $1 }

Function :: { Function }
    : Lineno fun id Parameters RetType '{' 
        Declarations StatementList 
      '}'                                       { Function $1 $3 $4 $7 $8 $5 }

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
    : '{' StatementList '}'                     { Block $2 }

StatementList :: { [Statement] }
    : {- empty -}                               { [] }
    | StatementList Statement                   { $2 : $1 }

Assignment :: { Statement }
    : Lineno LValue '=' Expression ';'          { Asgn $1 $2 $4 }

Print :: { Statement }
    : Lineno print Expression Endl ';'          { Print $1 $3 $4 }

Endl :: { Bool }
    : {- empty -}                               { False }
    | endl                                      { True }

Read :: { Statement }
    : Lineno read LValue ';'                    { Read $1 $3 }

Conditional :: { Statement }
    : Lineno if '(' Expression ')' 
        Block ElseBlock                         { Cond $1 $4 $6 $7 }

ElseBlock :: { Maybe Statement }
    : {- empty -}                               { Nothing }
    | else Block                                { Just $2 }

Loop :: { Statement }
    : Lineno while '(' Expression ')' Block     { Loop $1 $4 $6 }

Delete :: { Statement }
    : Lineno delete Expression ';'              { Delete $1 $3 }

Ret :: { Statement }
    : Lineno return MaybeExpr ';'               { Ret $1 $3 }

MaybeExpr :: { Maybe Expression }
    : {- empty -}                               { Nothing }
    | Expression                                { Just $1 }

Invocation :: { Statement }
    : Lineno id Arguments ';'                   { InvocSt $1 $2 $3 }

LValue :: { LValue }
    : Lineno id                                 { LValue $1 $2 Nothing }
    | Lineno LValue '.' id                      { LValue $1 $4 (Just $2)} 

Expression :: { Expression }
    : Boolterm                                  { $1 }
    | Lineno Boolterm boolOp Boolterm           { BinExp $1 $3 $2 $4 }

Boolterm :: { Expression }
    : Simple                                    { $1 }
    | Lineno Simple cmpOp Simple                { BinExp $1 $3 $2 $4 }

Simple :: { Expression }
    : Term                                      { $1 }
    | Lineno Term '+' Term                      { BinExp $1 "+" $2 $4 }
    | Lineno Term '-' Term                      { BinExp $1 "-" $2 $4 }

Term :: { Expression }
    : Unary                                     { $1 }
    | Lineno Unary '*' Unary                    { BinExp $1 "*" $2 $4 }
    | Lineno Unary '/' Unary                    { BinExp $1 "/" $2 $4 }

Unary :: { Expression }
    : Selector                                  { $1 }
    | Lineno '!' Selector                       { UExp $1 "!" $3 }
    | '-' Lineno Selector %prec NEG             { UExp $2 "-" $3 }

Selector :: { Expression }
    : Factor                                    { $1 }
    | Lineno Selector '.' id                    { DotExp $1 $2 $4 }

Factor :: { Expression }
    : '(' Expression ')'                        { $2 }
    | Lineno id                                 { IdExp $1 $2 }
    | Lineno id Arguments                       { InvocExp $1 $2 $3 }
    | Lineno num                                { IntExp $1 $2 }
    | Lineno true                               { TrueExp $1 }
    | Lineno false                              { FalseExp $1 }
    | Lineno new id                             { NewExp $1 $3 }
    | Lineno null                               { NullExp $1 }

Arguments :: { [Expression] }
    : Expression                                { [$1] }
    | Arguments ',' Expression                  { $2 : $1 }

Lineno ::  { LineNumber }
    : {- empty -}                               {% getLineNo }

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
    ("null", TokenNull)
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
                        failP (show line ++ ": parse error at token"
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
        (fromMaybe (TokenId s) (snd <$> find ((==) word . fst) keywords)) rest
    where (word, rest) = span isAlpha s
 
}

