grammar Mini;

options
{
   output = AST;
}

tokens
{
   STRUCT         =  'struct';
   INT            =  'int';
   BOOL           =  'bool';
   FUN            =  'fun';
   VOID           =  'void';
   PRINT          =  'print';
   ENDL           =  'endl';
   READ           =  'read';
   IF             =  'if';
   ELSE           =  'else';
   WHILE          =  'while';
   DELETE         =  'delete';
   RETURN         =  'return';
   TRUE           =  'true';
   FALSE          =  'false';
   NEW            =  'new';
   NULL           =  'null';
   PROGRAM;
   TYPES;
   TYPE;
   DECLS;
   FUNCS;
   DECL;
   DECLLIST;
   PARAMS;
   RETTYPE;
   BLOCK;
   STMTS;
   INVOKE;
   ARGS;
   NEG;
}

@header
{
   /* package declaration here */
}
@lexer::header
{
   /* package declaration here */
}

@members
{
   private boolean _errors = false;
   public boolean hasErrors()
   {
      return _errors;
   }
}

@rulecatch
{
   catch (RecognitionException re)
   {
      reportError(re);
      recover(input, re);
      _errors = true;
   }
}

/*
   Lexer Rules
*/

LBRACE   :  '{'   ;
RBRACE   :  '}'   ;
SEMI     :  ';'   ;
COMMA    :  ','   ;
LPAREN   :  '('   ;
RPAREN   :  ')'   ;
ASSIGN   :  '='   ;
DOT      :  '.'   ;
AND      :  '&&'  ;
OR       :  '||'  ;
EQ       :  '=='  ;
LT       :  '<'   ;
GT       :  '>'   ;
NE       :  '!='  ;
LE       :  '<='  ;
GE       :  '>='  ;
PLUS     :  '+'   ;
MINUS    :  '-'   ;
TIMES    :  '*'   ;
DIVIDE   :  '/'   ;
NOT      :  '!'   ;

ID       :  ('a'..'z' | 'A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')* ;

INTEGER  :  '0' | ('1'..'9') ('0'..'9')* ;

WS       :  (  ' '
            |  '\t'
            |  '\f'
            |  '\r'
            |  '\n'
            )+
            { skip(); }
         ;

COMMENT  :  '#' (~'\n')* '\n'
            { skip(); }
         ;


/*
   Parser Rules
*/

program
   :  t=types d=declarations f=functions EOF
      -> ^(PROGRAM $t $d $f)
   ;
types
   :  (STRUCT ID LBRACE) => types_sub -> ^(TYPES types_sub)
   |  -> TYPES
   ;
types_sub
   :  (STRUCT ID LBRACE) => type_declaration types_sub
   |
   ;
type_declaration
   :  STRUCT^ ID LBRACE! nested_decl RBRACE! SEMI!
   ;
nested_decl
   :  (decl SEMI!)+
   ;
decl
   :  t=type i=ID
      -> ^(DECL ^(TYPE $t) $i)
   ;
type
   :  INT
   |  BOOL
   |  STRUCT^ ID
   ;
declarations
   :  (declaration)* -> ^(DECLS declaration*)
   ;
declaration
   :  t=type ilist=id_list SEMI
      -> ^(DECLLIST ^(TYPE $t) $ilist)
   ;
id_list
   :  ID (COMMA! ID)*
   ;
functions
   :  function* -> ^(FUNCS function*)
   ;
function
   :  FUN id=ID p=parameters r=return_type LBRACE d=declarations s=statement_list RBRACE
      -> ^(FUN $id $p ^(RETTYPE $r) $d $s)
   ;
parameters
   :  LPAREN (decl (COMMA decl)*)? RPAREN
      -> ^(PARAMS decl*)
   ;
return_type
   :  type
   |  VOID
   ;
statement
   :  block
   |  (lvalue ASSIGN) => assignment
   |  print
   |  read
   |  conditional
   |  loop
   |  delete
   |  ret
   |  (ID LPAREN) => invocation
   ;
block
   :  LBRACE s=statement_list RBRACE
      -> ^(BLOCK $s)
   ;
statement_list
   :  (statement)* -> ^(STMTS statement*)
   ;
assignment
   :  l=lvalue ASSIGN e=expression SEMI
      -> ^(ASSIGN $e $l)
   ;
print
   :  PRINT^ expression (ENDL)? SEMI!
   ;
read
   :  READ^ lvalue SEMI!
   ;
conditional
   :  IF^ LPAREN! expression RPAREN! block (ELSE! block)?
   ;
loop
   :  WHILE LPAREN e=expression RPAREN b=block
      -> ^(WHILE $e $b $e)
   ;
delete
   :  DELETE^ expression SEMI!
   ;
ret
   :  RETURN^ (expression)? SEMI!
   ;
invocation
   :  id=ID a=arguments SEMI
      -> ^(INVOKE $id $a)
   ;
lvalue
   :  ID (DOT^ ID)*
   ;
expression
   :  boolterm ((AND^ | OR^) boolterm)*
   ;
boolterm
   :  simple ((EQ^ | LT^ | GT^ | NE^ | LE^ | GE^) simple)?
   ;
simple
   :  term ((PLUS^ | MINUS^) term)*
   ;
term
   :  unary ((TIMES^ | DIVIDE^) unary)*
   ;
unary
   :  NOT! odd_not
   |  MINUS! odd_neg
   |  selector
   ;
odd_not
   :  NOT! even_not
   |  s=selector
      -> ^(NOT $s)
   ;
even_not
   :  NOT! odd_not
   |  selector
   ;
odd_neg
   :  MINUS! even_neg
   |  s=selector
      -> ^(NEG $s)
   ;
even_neg
   :  MINUS! odd_neg
   |  selector
   ;
selector
   :  factor (DOT^ ID)*
   ;
factor
   :  LPAREN! expression RPAREN!
   |  id=ID a=arguments
      -> ^(INVOKE $id $a)
   |  ID
   |  INTEGER
   |  TRUE
   |  FALSE
   |  NEW^ ID
   |  NULL
   ;
arguments
   :  LPAREN! arg_list RPAREN!
   ;
arg_list
   :  expression (COMMA expression)*
      -> ^(ARGS expression+)
   |
      -> ARGS
   ;
