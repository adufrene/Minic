{
    module Mini.Parser where

    import Mini.Types
    import Mini.Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

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
    struct      { TokenStruct }
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

%%

Program :: Program
    : Types Declarations Functions eof  { Program $1 $2 $3 }

Types :: [TypeDef]
    : {- empty -}                       { [] }
    | Types TypeSub                     { $2 : $1 }

TypeSub :: TypeDef
    : struct id '{' NestedDecl '}' ';'  { TypeDef 0 $2 $4 }

NestedDecl :: [Field]
    : {- empty -}                       { [] }
    | NestedDecl Decl ';'               { $2 : $1 }

Decl :: Field
    : Type id                           { Field 0 $1 $2 }

Type :: Type
    : int                               { intType }
    | bool                              { boolType }
    | struct id                         { $2 }

Declarations :: [Declaration]
    : {- empty -}                       { [] }
    | Declarations Declaration          { $2 ++ $1 }

Declaration :: [Declaration]
    : Type IdList ';'                   { map (Declaration 0 $1) $2 }

IdList :: [Id]
    : {- empty -}                       { [] }
    | id                                { [$1] }
    | IdList ',' id                     { $3 : $1 }

Functions :: [Function]
    : {- empty -}                       { [] }
    | Functions Function                { $2 : $1 }

Function :: Function
    : fun id Parameters RetType '{' 
        Declarations StatementList 
      '}'                               { Function 0 $2 $3 $6 $7 $4 }

Parameters :: [Field]
    : '('')'                            { [] }
    | '(' DeclList ')'                  { $2 }

DecList :: [Field]
    : Decl                              { [$1] }
    | DeclList ',' Decl                 { $3 : $1 }

RetType :: Type
    : Type                              { $1 }
    | void                              { voidType }

Statement :: Statement
    : Block                             { $1 }
    | Assignment                        { $1 }
    | Print                             { $1 }
    | Read                              { $1 }
    | Conditional                       { $1 }
    | Loop                              { $1 }
    | Delete                            { $1 }
    | Ret                               { $1 }
    | Invocation                        { $1 }

Block :: Statement
    : '{' StatementList '}'             { Block $2 }

StatementList :: [Statement]
    : {- empty -}                       { [] }
    | StatementList Statement           { $2 : $1 }
