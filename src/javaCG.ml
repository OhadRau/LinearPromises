open Lang

let make_id =
  let unique = ref 0 in
  function
  | "_" ->
    let id = "$_" ^ string_of_int !unique in
    unique := !unique + 1;
    id
  | id -> id

let rec java_type = function
  | `Int -> "Integer"
  | `Bool -> "Boolean"
  | `Unit -> "Unit" (* Unit class *)
  | `Promise k -> Printf.sprintf "Promise<%s>" (java_type (k :> ty))
  | `PromiseStar k -> Printf.sprintf "OwnedPromise<%s>" (java_type (k :> ty))
  | `Function (_, _) -> failwith "Cannot create temp function variable"

let rec emit_args = function
  | [] -> ""
  | [arg] -> emit ~in_expr:true arg
  | arg::args ->
    Printf.sprintf "%s, %s"
      (emit ~in_expr:true arg) (emit_args args)

and emit ?(in_expr=false) = function
  | Let { id; annot; value; body } ->
    let ty = java_type annot in
    Printf.sprintf "%s %s = %s; %s"
      ty (make_id id) (* = *) (emit ~in_expr:true value) (* ; *) (emit body)
  | If { condition; then_branch; else_branch } when in_expr ->
    Printf.sprintf "((%s) ? (%s) : (%s))"
      (emit ~in_expr condition) (emit ~in_expr then_branch) (emit ~in_expr else_branch)
  | If { condition; then_branch; else_branch } ->
    Printf.sprintf "if (%s) { %s } else { %s }"
      (emit ~in_expr:true condition) (emit then_branch) (emit else_branch)
  (* TODO: How do we get the final ; + return when it's needed?
     This works ok for now because of our functional AST but e.g. we can't do
     something like: while (true) { ... }; return a; *)
  | e when not in_expr ->
    Printf.sprintf "return %s;" (emit ~in_expr:true e)
  | Variable v -> v
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Apply { fn; args } ->
    Printf.sprintf "%s(%s)"
      (emit ~in_expr:true fn) (emit_args args)
  | Promise { ty } ->
    let ty' = java_type (ty :> ty) in
    Printf.sprintf "new OwnedPromise<%s>()" ty'
  | Write { promiseStar; newValue } ->
    Printf.sprintf "%s.write(%s)"
      (emit ~in_expr:true promiseStar) (emit ~in_expr:true newValue)
  | Read { promise } ->
    Printf.sprintf "%s.read()"
      (emit ~in_expr:true promise)
  | Async { application } ->
    Printf.sprintf "async(() -> %s)"
      (emit ~in_expr:true application)
  | For _ | While _ when not in_expr ->
    failwith "Cannot use for/while loop inside of an expression"
  | For { name; first; last; forBody } ->
    (* TODO: What if last < first? What if type(first) != int? *)
    Printf.sprintf "for (int %s = %s; %s < %s; %s++) { %s }"
      name (* = *) (emit ~in_expr:true first) (* ; *)
      name (* < *) (emit ~in_expr:true last)  (* ; *)
      name (* ++ *)
      (emit forBody)
  | While { whileCond; whileBody } ->
    Printf.sprintf "while (%s) { %s }"
    (emit ~in_expr:true whileCond) (emit whileBody)
