%token <bool>   BOOL
%token <int>    INT
%token <string> IDENT

%token FUNC
%token UNION
%token RECORD
%token BEGIN
%token END

%token LET
%token IN
%token IF
%token MATCH
%token THEN
%token ELSE
%token FOR
%token TO
%token WHILE
%token PROMISE
%token ASYNC

%token TYPE_UNIT
%token TYPE_BOOL
%token TYPE_INT
%token TYPE_PROMISE

%token EQUAL
%token DOT
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token COMMA
%token STAR
%token COLON
%token SEMICOLON
%token RIGHT_ARROW
%token LEFT_ARROW
%token LEFT_TILDE_ARROW
%token QUESTION

%token EOF

%right SEMICOLON IN
%nonassoc ASYNC
%nonassoc QUESTION LEFT_ARROW LEFT_TILDE_ARROW
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
  | LET; id = IDENT; COLON; annot = type_expr; EQUAL; value = expr; IN; body = expr
    { Let { id; annot; value; body } }
  | MATCH; matchValue = expr; BEGIN; matchCases = match_cases; END
    { Match { matchValue; matchCases } }
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
  | PROMISE; ty = primitive_type
    { Promise { ty } }
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

named_args:
  |
    { [] }
  | id = IDENT; EQUAL; e = expr
    { [(id, e)] }
  | id = IDENT; EQUAL; e = expr; COMMA; rest = named_args
    { (id, e)::rest }
;
