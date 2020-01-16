%token <bool>   BOOL
%token <int>    INT
%token <string> IDENT

%token FUNC
%token BEGIN
%token END

%token LET
%token IN
%token IF
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
%token LEFT_PAREN
%token RIGHT_PAREN
%token COMMA
%token STAR
%token COLON
%token SEMICOLON
%token LEFT_ARROW
%token LEFT_TILDE_ARROW
%token QUESTION

%token EOF

%right SEMICOLON IN
%nonassoc ASYNC
%nonassoc QUESTION LEFT_ARROW LEFT_TILDE_ARROW
%nonassoc LEFT_PAREN

%start<Lang.program> program
%%

program:
  | EOF                       { { programName="Example"; funcs=[] } }
  | fn = func; prog = program { { prog with funcs=fn::prog.funcs } }
;

func:
  | FUNC; name = IDENT; LEFT_PAREN; p = params; RIGHT_PAREN COLON; t = type_decl;
    BEGIN; e = expr; END
    { { funcName = name; retType = t; params = p; expr = e } }
;

primitive_type:
  | TYPE_UNIT { `Unit }
  | TYPE_BOOL { `Bool }
  | TYPE_INT  { `Int }
;

type_decl:
(* We don't actually need to parse function types
  | LEFT_PAREN; args = type_list; RIGHT_PAREN RIGHT_ARROW; ret = type_decl
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
  | id = IDENT; COLON; ty = type_decl
    { [(id, ty)] }  
  | id = IDENT; COLON; ty = type_decl; COMMA; rest = params
    { (id, ty)::rest }
;

expr:
  | LEFT_PAREN RIGHT_PAREN
    { Unit }
  | LEFT_PAREN; e = expr; RIGHT_PAREN
    { e }
  | LET; id = IDENT; COLON; annot = type_decl; EQUAL; value = expr; IN; body = expr
    { Let { id; annot; value; body } }
  | IF; condition = expr; THEN; then_branch = expr; ELSE; else_branch = expr; END
    { If { condition; then_branch; else_branch } }
  | fn = expr; LEFT_PAREN; args = args; RIGHT_PAREN
    { Apply { fn; args } }
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

args:
  |
    { [] }
  | e = expr
    { [e] }
  | e = expr; COMMA; rest = args
    { e::rest }
;
