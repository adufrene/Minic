{
    module Main where
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    let     { TokenLet }
    in      { TokenIn }
    int     { TokenInt $$ }
    var     { TokenVar $$ }
    id      { TokenId $$ }
    '='     { TokenEq }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenOB }
    ')'     { TokenCB }
    '{'     { TokenLBrace }
    '}'     { TokenRBrace }
    ';'     { TokenSemi }

%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }

%%

Exp     : let var '=' Exp in Exp    { Let $2 $4 $6 }
        | Exp1                      { Exp1 $1 }

Exp1    : Exp1 '+' Term             { Plus $1 $3 }
        | Exp1 '-' Term             { Minus $1 $3 }
        | Term                      { Term $1 }

Term    : Term '*' Factor           { Times $1 $3 }
        | Term '/' Factor           { Div $1 $3 }
        | Factor                    { Factor $1 }

Factor  : int                       { Int $1 }
        | var                       { Var $1 }
        | '(' Exp ')'               { Brack $2 }

lineno :: { LineNumber }
        : {- empty -}               {% getLineNo }

{
    data ParseResult a = Ok a | Failed String
    type LineNumber = Int
    type P a = String -> LineNumber -> ParseResult a

    getLineNo :: P LineNumber
    getLineNo = \s l -> Ok l

    thenP :: P a -> (a -> P b) -> P b
    m `thenP` k =
        case m of
            Ok a -> k a
                Failed e -> Failed e

    returnP :: a -> P a
    returnP a = Ok a

    failP :: String -> P a
    failP err = Failed err

    catchP :: P a -> (String -> P a) -> P a
    catchP m k =
        case m of 
            Ok a -> Ok a
                Failed e -> k e

    parseError :: Token -> P a
    parseError token = getLineNo `thenP` \line ->
                        failP (show line ++ ": parse error")

    data Exp 
            = Let String Exp Exp
            | Exp1 Exp1 
            deriving (Show)

    data Exp1 
            = Plus Exp1 Term
            | Minus Exp1 Term
            | Term Term deriving
            deriving (Show)

    data Term 
            = Times Term Factor
            | Div Term Factor
            | Factor Factor
            deriving (Show)

    data Factor
            = Int Int
            | Var String
            | Brack Exp
            deriving (Show)


    data Token
            = TokenLet
            | TokenIn
            | TokenInt Int
            | TokenVar String
            | TokenEq
            | TokenPlus
            | TokenMinus
            | TokenTimes
            | TokenDiv
            | TokenOB
            | TokenCB
            | TokenEOF
            deriving (Show)
    
    lexer :: (Token -> P a) -> P a
    lexer cont s = case s of
            [] -> cont TokenEOF []
            ('\n':cs) -> \line -> lexer cont cs (line + 1)
            (c:cs)
                | isSpace c -> lexer cs
                | isAlpha c -> lexVar (c:cs)
                | isDigit c -> lexNum (c:cs)
            ('=':cs) -> cont TokenEq cs
            ('+':cs) -> cont TokenPlus cs
            ('-':cs) -> cont TokenMinus cs
            ('*':cs) -> cont TokenTimes cs
            ('/':cs) -> cont TokenDiv cs
            ('(':cs) -> cont TokenOB cs
            (')':cs) -> cont TokenCB cs
        where lexNum cs = cont (TokenInt (read num)) rest
                where (num,rest) = span isDigit cs
              lexVar cs =
                case span isAlpha cs of
                    ("let",rest) -> cont TokenLet rest
                    ("in",rest)  -> cont TokenIn rest
                    (var,rest)   -> cont (TokenVar var) rest
    
    calc :: P Exp

    main :: IO ()
    main = getContents >>= print . calc . lexer

}
