type primitive_ty =
  [ `Int | `Bool | `Unit | `Custom of string ]

type ty = [
  | primitive_ty
  | `Promise of primitive_ty
  | `PromiseStar of primitive_ty
  | `Function of ty list * ty ]

let rec string_of_ty = function
  | `Int -> "Int"
  | `Bool -> "Bool"
  | `Unit -> "Unit"
  | `Custom name -> name
  | `Promise ty -> "Promise(" ^ string_of_ty (ty :> ty) ^ ")"
  | `PromiseStar ty -> "Promise*(" ^ string_of_ty (ty :> ty) ^ ")"
  | `Function (args, result) ->
     "(" ^ String.concat ", " (List.map string_of_ty args) ^ ") -> " ^ string_of_ty result

type custom_ty =
  | Record of (string * ty) list
  | Union of (string * ty list) list

type ty_decl = { typeName: string; typeDefn: custom_ty }

let string_of_ty_decl {typeName; typeDefn} =
  let rec format_record_cases = function
    | [] -> ""
    | [(name, ty)] ->
      name ^ ": " ^ string_of_ty ty ^ "\n"
    | (name, ty)::cases ->
      name ^ ": " ^ string_of_ty ty ^ ";\n" ^
      format_record_cases cases
  and format_union_cases = function
    | [] -> ""
    | [(name, [])] ->
      name ^ "\n"
    | [(name, tys)] ->
      let args = List.map string_of_ty tys |> String.concat "," in
      name ^ "(" ^ args ^ ")\n"
    | (name, [])::cases ->
      name ^ ";\n" ^
      format_union_cases cases
    | (name, tys)::cases ->
      let args = List.map string_of_ty tys |> String.concat "," in
      name ^ "(" ^ args ^ ");\n" ^
      format_union_cases cases in
  match typeDefn with
  | Record fields ->
    "record " ^ typeName ^ " begin\n" ^
    format_record_cases fields ^
    "end"
  | Union alternatives ->
    "union " ^ typeName ^ " begin\n" ^
    format_union_cases alternatives ^
    "end"

type pattern = {
  patCtor: string;
  patArgs: string list;
  patResult: expr
}

and expr =
  | Variable of string
  | Unit
  | Number of int
  | Boolean of bool
  | Let of { id: string; annot: ty; value: expr; body: expr }
  | If of { condition: expr; then_branch: expr; else_branch: expr }
  | Match of { matchValue: expr; matchCases: pattern list }
  | RecordMatch of { matchRecord: expr; matchArgs: (string * string) list; matchBody: expr }
  | Apply of { fn: expr; args: expr list }
  | ConstructUnion of { unionCtor: string; unionArgs: expr list }
  | ConstructRecord of { recordCtor: string; recordArgs: (string * expr) list }
  | RecordAccess of { record: expr; field: string }
  | Promise of { read: string; write: string; ty: primitive_ty; promiseBody: expr }
  | Write of { promiseStar: expr; newValue: expr; unsafe: bool }
  | Read of { promise: expr }
  | Async of { application: expr }
  | For of { name: string; first: expr; last: expr; forBody: expr }
  | While of { whileCond: expr; whileBody: expr }

let string_of_match_record pairs =
  "{ " ^ (List.map (fun (k, v) -> k ^ " = " ^ v) pairs |> String.concat ", ") ^ "}"

let rec string_of_expr = function
  | Variable v -> v
  | Unit -> "()"
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Let { id; annot; value; body } ->
    "let " ^ id ^ ": " ^ string_of_ty annot ^ " = " ^ string_of_expr value ^ " in " ^ string_of_expr body
  | If { condition; then_branch; else_branch } ->
    "if " ^ string_of_expr condition ^ " then " ^ string_of_expr then_branch ^ " else " ^ string_of_expr else_branch ^ " end"
  | Match { matchValue; matchCases } ->
    "match " ^ string_of_expr matchValue ^ " begin " ^ string_of_match_cases matchCases ^ " end"
  | RecordMatch { matchRecord; matchArgs; matchBody } ->
    "match " ^ string_of_expr matchRecord ^ " begin " ^ string_of_match_record matchArgs ^ " -> " ^ string_of_expr matchBody ^ " end"
  | Apply { fn; args } ->
    string_of_expr fn ^ "(" ^ (List.fold_right (fun arg str -> string_of_expr arg ^ ", " ^ str) args "") ^ ")"
  | ConstructUnion { unionCtor; unionArgs } ->
    unionCtor ^ "[" ^ (List.fold_right (fun arg str -> string_of_expr arg ^ ", " ^ str) unionArgs "") ^ "]"
  | ConstructRecord { recordCtor; recordArgs } ->
    recordCtor ^ "{" ^ (List.fold_right (fun (name, arg) str -> name ^ "=" ^ string_of_expr arg ^ ", " ^ str) recordArgs "") ^ "}"
  | RecordAccess { record; field } ->
    (string_of_expr record) ^ "." ^ field
  | Promise { read; write; ty; promiseBody } ->
    "promise " ^ read ^ ", " ^ write ^ ": " ^ string_of_ty (ty :> ty) ^ " in " ^ string_of_expr promiseBody
  | Write { promiseStar; newValue; unsafe } ->
    let arrow = if unsafe then " <~ " else " <- " in
    string_of_expr promiseStar ^ arrow ^ string_of_expr newValue
  | Read { promise } ->
    "?" ^ string_of_expr promise
  | Async { application } ->
    "async " ^ string_of_expr application
  | For { name; first; last; forBody } ->
    "for " ^ name ^ " = " ^ string_of_expr first ^ " to " ^ string_of_expr last ^ " begin " ^ string_of_expr forBody ^ " end"
  | While { whileCond; whileBody } ->
    "while " ^ string_of_expr whileCond ^ " begin " ^ string_of_expr whileBody ^ " end"

and string_of_match_cases cases =
  let string_of_case { patCtor; patArgs; patResult } =
    patCtor ^ "[" ^ (String.concat ", " patArgs) ^ "] -> " ^ string_of_expr patResult in
  List.map string_of_case cases |> String.concat " "

type func = {
  funcName: string;
  retType: ty;
  params: (string * ty) list;
  expr: expr
}

type program = {
  programName: string;
  funcs: func list;
  types: ty_decl list
}

let example =
  Let { id="a"; annot=`Int; value=(Number 5);
        body=(If { condition=(Apply { fn=(Variable ">");
                                      args=[Variable "a"; Number 0] });
                   then_branch=(Boolean true);
                   else_branch=(Boolean false) }) }
