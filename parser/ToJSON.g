tree grammar ToJSON;

options
{
   tokenVocab=Mini;
   ASTLabelType=CommonTree;
}

/*
   Tree Parser -- generates JSON representation of AST
*/
@header
{
   import javax.json.*;
}

@members
{
   private JsonBuilderFactory factory = Json.createBuilderFactory(null);
}


translate
   returns [JsonValue json = null]
   :  ^(PROGRAM t=types d=declarations f=functions)
         {
            $json = factory.createObjectBuilder()
               .add("types", $t.json)
               .add("declarations", $d.json)
               .add("functions", $f.json)
               .build();
         }
   ;

types
   returns [JsonValue json = null]
   @init { JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  ^(TYPES (t=type_decl { abuilder.add($t.json); })*)
      { $json = abuilder.build(); }
   |  { $json = abuilder.build(); }
   ;

type_decl
   returns [JsonValue json = null]
   :  ^(ast=STRUCT id=ID n=nested_decl)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("id", $id.text)
            .add("fields", $n.json)
            .build();
      }
   ;

nested_decl
   returns [JsonValue json = null]
   @init{ JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  (f=field_decl { abuilder.add($f.json); })+
      { $json = abuilder.build();}
   ;

field_decl
   returns [JsonValue json = null]
   :  ^(DECL ^(TYPE t=type) id=ID)
      {
         $json = factory.createObjectBuilder()
            .add("line", $id.line)
            .add("type", $t.str)
            .add("id", $id.text)
            .build();
      }
   ;

type
   returns [String str = null]
   :  INT { str = "int"; }
   |  BOOL { str = "bool"; }
   |  ^(STRUCT id=ID) { str = $id.text; }
   ;

declarations
   returns [JsonValue json = null]
   @init { JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  ^(DECLS (d=decl_list[abuilder])*)
      { $json = abuilder.build(); }
   |  { $json = abuilder.build(); }
   ;

decl_list[JsonArrayBuilder abuilder]
   :  ^(DECLLIST ^(TYPE t=type)
         (id=ID
            {
               $abuilder.add(factory.createObjectBuilder()
                  .add("line", $id.line)
                  .add("type", $t.str)
                  .add("id", $id.text)
               );
            }
         )+
      )
   ;

functions
   returns [JsonValue json = null]
   @init{ JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  ^(FUNCS (f=function { abuilder.add($f.json); })*)
      { $json = abuilder.build(); }
   |  { $json = abuilder.build(); }
   ;

function
   returns [JsonValue json = null]
   :  ^(ast=FUN id=ID p=parameters r=return_type d=declarations
         s=statement_list)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("id", $id.text)
            .add("parameters", $p.json)
            .add("return_type", $r.str)
            .add("declarations", $d.json)
            .add("body", $s.json)
            .build();
      }
   ;

parameters
   returns [JsonValue json = null]
   @init{ JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  ^(PARAMS (p=param_decl { abuilder.add($p.json); })*)
      { $json = abuilder.build(); }
   ;

param_decl
   returns [JsonValue json = null]
   :  ^(DECL ^(TYPE t=type) id=ID)
      {
         $json = factory.createObjectBuilder()
            .add("line", $id.line)
            .add("type", $t.str)
            .add("id", $id.text)
            .build();
      }
   ;

return_type
   returns [String str = null]
   :  ^(RETTYPE rtype) { $str = $rtype.str; }
   ;

rtype
   returns [String str = null]
   :  t=type { $str = $t.str; }
   |  VOID { $str = "void"; }
   ;

statement
   returns [JsonValue json = null]
   :  (s=block
      |  s=assignment
      |  s=print
      |  s=read
      |  s=conditional
      |  s=loop
      |  s=delete
      |  s=return_stmt
      |  s=invocation_stmt
      )
      { $json = $s.json; }
   ;

block
   returns [JsonValue json = null]
   :  ^(BLOCK s=statement_list)
      {
         $json = factory.createObjectBuilder()
            .add("stmt", "block")
            .add("list", $s.json)
            .build();
      }
   ;

statement_list
   returns [JsonValue json = null]
   @init{ JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  ^(STMTS (s=statement { abuilder.add($s.json); })*)
      { $json = abuilder.build(); }
   ;

assignment
   returns [JsonValue json = null]
   :  ^(ast=ASSIGN e=expression l=lvalue)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("stmt", "assign")
            .add("source", $e.json)
            .add("target", $l.json)
            .build();
      }
   ;

print
   returns [JsonValue json = null]
   @init { boolean endl = false; }
   :  ^(ast=PRINT e=expression (ENDL { endl = true; })?)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("stmt", "print")
            .add("exp", $e.json)
            .add("endl", endl)
            .build();
      }
   ;

read
   returns [JsonValue json = null]
   :  ^(ast=READ l=lvalue)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("stmt", "read")
            .add("target", $l.json)
            .build();
      }
   ;

conditional
   returns [JsonValue json = null]
   :  ^(ast=IF g=expression t=block (e=block)?)
      {
         JsonObjectBuilder obuilder = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("stmt", "if")
            .add("guard", $g.json)
            .add("then", $t.json);

         if ($e.json != null)
         {
            obuilder.add("else", $e.json);
         }

         $json = obuilder.build();
      }
   ;

loop
   returns [JsonValue json = null]
   :  ^(ast=WHILE e=expression b=block expression)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("stmt", "while")
            .add("guard", $e.json)
            .add("body", $b.json)
            .build();
      }
   ;

delete
   returns [JsonValue json = null]
   :  ^(ast=DELETE e=expression)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("stmt", "delete")
            .add("guard", $e.json)
            .build();
      }
   ;

return_stmt
   returns [JsonValue json = null]
   :  ^(ast=RETURN (e=expression)?)
      {
         JsonObjectBuilder obuilder = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("stmt", "return");

         if ($e.json != null)
         {
            obuilder.add("exp", $e.json);
         }

         $json = obuilder.build();
      }
   ;

invocation_stmt
   returns [JsonValue json = null]
   @init { JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  ^(INVOKE id=ID ^(ARGS (e=expression { abuilder.add($e.json); })*))
      {
         $json = factory.createObjectBuilder()
            .add("line", $id.line)
            .add("stmt", "invocation")
            .add("id", $id.text)
            .add("args", abuilder.build())
            .build();
      }
   ;

lvalue
   returns [JsonValue json = null]
   :  id=ID
      { $json = factory.createObjectBuilder().add("id", $id.text).build(); }
   |  ^(ast=DOT l=lvalue id=ID)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("left", $l.json)
            .add("id", $id.text)
            .build();
      }
   ;

expression
   returns [JsonValue json = null]
   :  ^((ast=AND | ast=OR | ast=EQ | ast=LT | ast=GT | ast=NE | ast=LE
         | ast=GE | ast=PLUS | ast=MINUS | ast=TIMES | ast=DIVIDE)
         lft=expression rht=expression)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "binary")
            .add("operator", $ast.text)
            .add("lft", $lft.json)
            .add("rht", $rht.json)
            .build();
      }
   |  ^(ast=NOT e=expression)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "unary")
            .add("operator", "!")
            .add("operand", $e.json)
            .build();
      }
   |  ^(ast=NEG e=expression)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "unary")
            .add("operator", "-")
            .add("operand", $e.json)
            .build();
      }
   |  ^(ast=DOT    e=expression id=ID)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "dot")
            .add("left", $e.json)
            .add("id", $id.text)
            .build();
      }
   |  e=invocation_exp { $json = $e.json; }
   |  id=ID
      {
         $json = factory.createObjectBuilder()
            .add("line", $id.line)
            .add("exp", "id")
            .add("id", $id.text)
            .build();
      }
   |  i=INTEGER
      {
         $json = factory.createObjectBuilder()
            .add("line", $i.line)
            .add("exp", "num")
            .add("value", $i.text)
            .build();
      }
   |  ast=TRUE
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "true")
            .build();
      }
   |  ast=FALSE
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "false")
            .build(); }
   |  ^(ast=NEW id=ID)
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "new")
            .add("id", $id.text)
            .build();
      }
   |  ast=NULL
      {
         $json = factory.createObjectBuilder()
            .add("line", $ast.line)
            .add("exp", "null")
            .build(); }
   ;

invocation_exp
   returns [JsonValue json = null]
   @init { JsonArrayBuilder abuilder = factory.createArrayBuilder(); }
   :  ^(INVOKE id=ID ^(ARGS (e=expression { abuilder.add($e.json); })*))
      {
         $json = factory.createObjectBuilder()
            .add("line", $id.line)
            .add("exp", "invocation")
            .add("id", $id.text)
            .add("args", abuilder.build())
            .build();
      }
   ;
