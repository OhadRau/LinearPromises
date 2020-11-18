%token <bool>   BOOL
%token <int>    INT
%token <string> STRING
%token <string> IDENT

%token FUNC UNION RECORD
%token BEGIN END

%token LET IN
%token IF THEN ELSE
%token MATCH
%token FOR TO
%token WHILE
%token PROMISE
%token ASYNC

%token TYPE_UNIT TYPE_BOOL TYPE_INT TYPE_STRING TYPE_PROMISE

%token EQUAL
%token IS_EQUAL NOT_EQUAL LESS_THAN LESS_THAN_EQUAL GREATER_THAN GREATER_THAN_EQUAL
%token ADD SUB MUL DIV
%token DOT
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token COMMA
%token STAR
%token COLON
%token SEMICOLON
%token RIGHT_ARROW
%token LEFT_ARROW LEFT_TILDE_ARROW
%token QUESTION

%token EOF

%right SEMICOLON IN
%nonassoc ASYNC
%nonassoc LEFT_ARROW LEFT_TILDE_ARROW
%nonassoc IS_EQUAL NOT_EQUAL LESS_THAN LESS_THAN_EQUAL GREATER_THAN GREATER_THAN_EQUAL
%left ADD SUB
%left MUL DIV
%nonassoc QUESTION
%nonassoc DOT
%nonassoc LEFT_PAREN

%{
  open Lang
%}

%start<Lang.program> program
%%

program:
  | EOF                            { { programName="Example"; funcs=[]; types=[] } }
  | fn = func; prog = program      { { prog with funcs=fn::prog.funcs } }
  | ty = type_decl; prog = program { { prog with types=ty::prog.types } }
;

func:
  | FUNC; name = IDENT; LEFT_PAREN; p = params; RIGHT_PAREN COLON; t = type_expr;
    BEGIN; e = expr; END
    { { funcName = name; retType = t; params = p; expr = e } }
;

type_decl:
  | RECORD; typeName = IDENT; BEGIN; fields = record_fields; END
    { { typeName; typeDefn = Record fields } }
  | UNION; typeName = IDENT; BEGIN; cases = union_cases; END
    { { typeName; typeDefn = Union cases } }
;

record_fields:
  |
    { [] }
  | name = IDENT; COLON; ty = type_expr
    { [(name, ty)] }
  | name = IDENT; COLON; ty = type_expr; SEMICOLON; rest = record_fields
    { (name, ty)::rest }
;

union_cases:
  |
    { [] }
  | name = IDENT; LEFT_BRACKET; tys = type_args; RIGHT_BRACKET
    { [(name, tys)] }
  | name = IDENT; LEFT_BRACKET; tys = type_args; RIGHT_BRACKET; SEMICOLON; rest = union_cases
    { (name, tys)::rest }
;

primitive_type:
  | TYPE_UNIT  { `Unit }
  | TYPE_BOOL  { `Bool }
  | TYPE_STRING{ `String }
  | TYPE_INT   { `Int }
  | id = IDENT { `Custom id }
;

type_args:
  |
    { [] }
  | e = type_expr
    { [e] }
  | e = type_expr; COMMA; rest = type_args
    { e::rest }
;

type_expr:
(* We don't actually need to parse function types
  | LEFT_PAREN; args = type_list; RIGHT_PAREN RIGHT_ARROW; ret = type_expr
     { `Function (args, ret) }
*)
  | ty = primitive_type
     { ty :> Lang.ty }
  | TYPE_PROMISE STAR LEFT_PAREN; t = primitive_type; RIGHT_PAREN
     { `PromiseStar t }
  | TYPE_PROMISE LEFT_PAREN; t = primitive_type; RIGHT_PAREN
     { `Promise t }
;

params:
  |
    { [] }
  | id = IDENT; COLON; ty = type_expr
    { [(id, ty)] }  
  | id = IDENT; COLON; ty = type_expr; COMMA; rest = params
    { (id, ty)::rest }
;

expr:
  | LEFT_PAREN RIGHT_PAREN
    { Unit }
  | LEFT_PAREN; e = expr; RIGHT_PAREN
    { e }
  | left = expr; MUL; right = expr
    { Infix { mode=`Mul; left; right } }
  | left = expr; DIV; right = expr
    { Infix { mode=`Div; left; right } }
  | left = expr; ADD; right = expr
    { Infix { mode=`Add; left; right } }
  | left = expr; SUB; right = expr
    { Infix { mode=`Sub; left; right } }
  | left = expr; IS_EQUAL; right = expr
    { Infix { mode=`Eq; left; right } }
  | left = expr; NOT_EQUAL; right = expr
    { Infix { mode=`Neq; left; right } }
  | left = expr; LESS_THAN; right = expr
    { Infix { mode=`Lt; left; right } }
  | left = expr; LESS_THAN_EQUAL; right = expr
    { Infix { mode=`Lte; left; right } }
  | left = expr; GREATER_THAN; right = expr
    { Infix { mode=`Gt; left; right } }
  | left = expr; GREATER_THAN_EQUAL; right = expr
    { Infix { mode=`Gte; left; right } }
  | LET; id = IDENT; COLON; annot = type_expr; EQUAL; value = expr; IN; body = expr
    { Let { id; annot; value; body } }
  | MATCH; matchValue = expr; BEGIN; matchCases = match_cases; END
    { Match { matchValue; matchCases } }
  | MATCH; matchRecord = expr; BEGIN; LEFT_BRACE; matchArgs = named_pattern_args; RIGHT_BRACE; RIGHT_ARROW; matchBody = expr; END
    { RecordMatch { matchRecord; matchArgs; matchBody } }
  | IF; condition = expr; THEN; then_branch = expr; ELSE; else_branch = expr; END
    { If { condition; then_branch; else_branch } }
  | fn = expr; LEFT_PAREN; args = args; RIGHT_PAREN
    { Apply { fn; args } }
  | ctor = IDENT; LEFT_BRACKET; args = args; RIGHT_BRACKET
    { ConstructUnion { unionCtor=ctor; unionArgs=args } }
  | ctor = IDENT; LEFT_BRACE; args = named_args; RIGHT_BRACE
    { ConstructRecord { recordCtor=ctor; recordArgs=args } }
  | record = expr; DOT; field = IDENT
    { RecordAccess { record; field } }
  | PROMISE; read = IDENT; COMMA; write = IDENT; COLON; ty = primitive_type; IN; promiseBody = expr
    { Promise { read; write; ty; promiseBody } }
  | promiseStar = expr; LEFT_ARROW; newValue = expr
    { Write { promiseStar; newValue; unsafe = false } }
  | promiseStar = expr; LEFT_TILDE_ARROW; newValue = expr
    { Write { promiseStar; newValue; unsafe = true } }
  | QUESTION; promise = expr
    { Read { promise } }
  | ASYNC; application = expr
    { Async { application } }
  | FOR; name = IDENT; EQUAL; first = expr; TO; last = expr; BEGIN; forBody = expr; END
    { For { name; first; last; forBody } }
  | WHILE; whileCond = expr; BEGIN; whileBody = expr; END
    { While { whileCond; whileBody } }
  | v = IDENT
    { Variable v }
  | i = INT
    { Number i }
  | b = BOOL
    { Boolean b }
  | s = STRING
    { String s }
  | left = expr; SEMICOLON; right = expr
    { Let { id="_"; annot=`Unit; value=left; body=right } }
;

match_cases:
  | patCtor = IDENT; LEFT_BRACKET; patArgs = pattern_args; RIGHT_BRACKET; RIGHT_ARROW; patResult = expr
    { [{ patCtor; patArgs; patResult }] }
  | patCtor = IDENT; LEFT_BRACKET; patArgs = pattern_args; RIGHT_BRACKET; RIGHT_ARROW; patResult = expr; rest = match_cases
    { { patCtor; patArgs; patResult }::rest }
;

args:
  |
    { [] }
  | e = expr
    { [e] }
  | e = expr; COMMA; rest = args
    { e::rest }
;

pattern_args:
  |
    { [] }
  | id = IDENT
    { [id] }
  | id = IDENT; COMMA; rest = pattern_args
    { id::rest }
;

named_pattern_args:
  |
    { [] }
  | id = IDENT; EQUAL; v = IDENT
    { [(id, v)] }
  | id = IDENT; EQUAL; v = IDENT; COMMA; rest = named_pattern_args
    { (id, v)::rest }
;

named_args:
  |
    { [] }
  | id = IDENT; EQUAL; e = expr
    { [(id, e)] }
  | id = IDENT; EQUAL; e = expr; COMMA; rest = named_args
    { (id, e)::rest }
;
