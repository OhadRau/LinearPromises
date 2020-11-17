open Lang
open Contexts

let resolveUserType userTypes = function
  | `Custom tyName -> begin
    match List.find_opt (fun {typeName; _} -> typeName = tyName) userTypes with
    | Some userTy -> userTy
    | _ -> failwith (tyName ^ " cannot be resolved to a type")
  end
  | ty -> failwith (string_of_ty ty ^ " is not a user-defined type")

(* must_use decides whether values of a type must be consumed. If a type contains
   a Promise* then it must be used, otherwise it does not need to be used. *)
let rec must_use_ userTypes history = function
  | `Int | `Bool | `Unit -> false
  | `Promise t -> must_use userTypes (t :> ty)
  | `Custom c when List.mem c history -> false
  | `Custom c -> begin
    let resolved = resolveUserType userTypes (`Custom c) in
    let history = c::history in
    match resolved.typeDefn with
    | Record fields -> List.exists (fun (_, ty) -> must_use userTypes ~history ty) fields
    | Union cases -> List.exists (fun (_, args) -> List.exists (must_use userTypes ~history) args) cases
  end
  | `PromiseStar _ -> true
  | `Function _ -> false

and must_use =
  let table = Hashtbl.create 1000 in
  fun userTypes ?(history=[]) ty ->
    if Hashtbl.mem table ty then
      Hashtbl.find table ty
    else begin
      let result = must_use_ userTypes history ty in
      Hashtbl.add table ty result;
      result
    end

module List = struct
  include List
  let rec all_equal = function
    | [] -> true
    | [_] -> true
    | item1::item2::rest -> item1 = item2 && all_equal (item2::rest)
  let sort_by_first a = sort (fun (x1, _) (x2, _) -> compare x1 x2) a
end

let rec free env = function
  | Variable v -> if not (Vars.mem v env) then Vars.singleton v else Vars.empty
  | Unit | Number _ | Boolean _ -> Vars.empty
  | Let { id; value; body; _ } ->
    let env_body = Vars.add id env in
    Vars.union (free env value) (free env_body body)
  | If { condition; then_branch; else_branch } ->
    Vars.union (free env condition)
      (Vars.union (free env then_branch) (free env else_branch))
  | Match { matchValue; matchCases } ->
    Vars.union (free env matchValue)
      (matchCases |> List.map begin fun {patArgs; patResult; _} ->
        let env_result = Vars.union env (Vars.of_list patArgs) in
        free env_result patResult
      end |> Vars.concat)
  | RecordMatch { matchRecord; matchArgs; matchBody } ->
    let env_body = Vars.union env (matchArgs |> List.map snd |> Vars.of_list) in
    Vars.union (free env matchRecord) (free env_body matchBody)
  | Apply { fn; args } ->
    Vars.union (free env fn) (List.map (free env) args |> Vars.concat)
  | ConstructUnion { unionArgs; _ } ->
    List.map (free env) unionArgs |> Vars.concat
  | ConstructRecord { recordArgs; _ } ->
    List.map (fun (_, arg) -> free env arg) recordArgs |> Vars.concat
  | RecordAccess { record; _ } ->
    free env record
  | Promise { read; write; promiseBody; _ } ->
    let env = Vars.add read (Vars.add write env) in
    free env promiseBody
  | Write { newValue; unsafe=true; _ } ->
    (* If we perform an unsafe write, this should not count as consuming the Promise*. *)
    free env newValue
  | Write { promiseStar; newValue; unsafe=false } ->
    Vars.union (free env promiseStar) (free env newValue)
  | Read { promise } ->
    free env promise
  | Async { application } ->
    free env application
  | For { name; first; last; forBody } ->
    let env_body = Vars.add name env in
    Vars.union (free env_body forBody)
      (Vars.union (free env first) (free env last))
  | While { whileCond; whileBody } ->
    Vars.union (free env whileCond) (free env whileBody)

let rec free_sequence env = function
  | [] -> Vars.empty
  | expr::rest -> Vars.union (free env expr) (free_sequence env rest)

let split userTypes gamma left_exp right_exp =
  let left_free = free Vars.empty left_exp
  and right_free = free Vars.empty right_exp in
  let claim vars = Env.filter (fun v _ -> Vars.mem v vars) gamma in
  let gamma_left = claim left_free and gamma_right = claim right_free in
  let all_used = Env.for_all begin fun id ty ->
    let must_be_used = must_use userTypes ty
    and used_left = Env.mem id gamma_left
    and used_right = Env.mem id gamma_right in
    if must_be_used then used_left || used_right else true
  end gamma
  (* If we want linear, rather than relevant typing: *)
  and none_reused = Env.for_all begin fun id ty ->
    let must_be_used = must_use userTypes ty in
    if must_be_used then not (Env.mem id gamma_right) else true
  end gamma_left in
  if all_used && none_reused then Some (gamma_left, gamma_right)
  else failwith (
    Printf.sprintf
      "Unable to split env [%s] between %s and %s; env [%s] differed from [%s] (all=%b,none=%b)\n"
      (Env.to_string string_of_ty gamma)
      (string_of_expr left_exp) (string_of_expr right_exp)
      (Env.to_string string_of_ty gamma_left) (Env.to_string string_of_ty gamma_right)
      all_used none_reused)

let rec split_sequence userTypes gamma = function
  | [] -> []
  | expr::rest ->
    let expr_free = free Vars.empty expr
    and rest_free = free_sequence Vars.empty rest in
    let claim vars = Env.filter (fun v _ -> Vars.mem v vars) gamma in
    let gamma_expr = claim expr_free
    and gamma_rest = claim rest_free in
    let all_used = Env.for_all begin fun id ty ->
      let must_be_used = must_use userTypes ty
      and used_left = Env.mem id gamma_expr
      and used_right = Env.mem id gamma_rest in
      if must_be_used then used_left || used_right else true
    end gamma
    (* If we want linear, rather than relevant typing: *)
    and none_reused = Env.for_all begin fun id ty ->
      let must_be_used = must_use userTypes ty in
      if must_be_used then not (Env.mem id gamma_rest) else true
    end gamma_expr in
    if all_used && none_reused then gamma_expr::(split_sequence userTypes gamma_rest rest)
    else failwith (
      Printf.sprintf
        "Unable to split env [%s] between %s and {%s}; env [%s] differed from [%s] (all=%b,none=%b)"
        (Env.to_string string_of_ty gamma)
        (string_of_expr expr) (List.map string_of_expr rest |> String.concat ";")
        (Env.to_string string_of_ty gamma_expr) (Env.to_string string_of_ty gamma_rest)
        all_used none_reused)

let find_record_type userTypes constructor =
  let user_type = List.find_opt begin function
    | { typeName; _ } when typeName = constructor -> true
    | _ -> false
  end userTypes in
  match user_type with
    | Some ({ typeDefn = Record fields; _ } as user_type) -> (user_type, fields)
    | _ -> failwith ("Expected constructor " ^ constructor ^ " for record type, but could not find a match")

let find_union_type userTypes constructor =
  let user_type = List.find_opt begin function
    | { typeDefn = Union cases; _}
      when List.exists (fun (caseName, _) -> caseName = constructor) cases -> true
    | _ -> false
  end userTypes in
  match user_type with
    | Some ({ typeDefn = Union cases; _ } as user_type) ->
      let _, fields = List.find (fun (name, _) -> name = constructor) cases in
      (user_type, fields)
    | _ -> failwith ("Expected constructor " ^ constructor ^ " for union type, but could not find a match")

(* Check if the expression completely consumes the entire environment *)
let env_complete userTypes env expr =
  match split userTypes env expr Unit with
  | Some _ -> true
  | None -> false

(* typecheck : ty_decl list -> env -> expr -> ty *)
let rec typecheck userTypes env expr = match expr with
  | Unit when env_complete userTypes env expr -> `Unit
  | Number _ when env_complete userTypes env expr -> `Int
  | Boolean _ when env_complete userTypes env expr -> `Bool
  | Variable var when Env.mem var env && env_complete userTypes env expr ->
    let gamma, _ = split userTypes env (Variable var) (Unit) |> Option.get in
    Env.find var gamma
  | Variable var -> failwith ("Unknown variable: " ^ var)
  | Let { id; annot; value; body } ->
    let gamma_value, gamma_body = split userTypes env value body |> Option.get in
    let ty_value = typecheck userTypes gamma_value value in
    if annot = ty_value then
      let gamma_body = Env.add id annot gamma_body in
      typecheck userTypes gamma_body body
    else failwith (
      "Mismatched types in let expression: got " ^ string_of_ty ty_value ^
      ", expected " ^ string_of_ty annot)
  | Apply { fn = Variable fn; args } when Env.mem fn env ->
    (* Since the function is guaranteed to NOT be linear, we don't need to bother to split it from the environment *)
    begin match Env.find fn env with
    | `Function (expected_arg_types, return_type) ->
      if check_sequence userTypes env expected_arg_types args then return_type
      else failwith "Argument types for function differed from expected types"
    | ty -> failwith ("Attempted to use type " ^ string_of_ty ty ^ " as a function")
    end
  | Apply { fn = _; _ } -> failwith (
      "Must apply a known function by name: " ^ string_of_expr expr ^ "; env = [" ^
      Env.to_string string_of_ty env ^ "]")
  | Async { application=e } ->
    if typecheck userTypes env e = `Unit then `Unit
    else failwith "Expected async expression to return type unit"
  | ConstructRecord { recordCtor; recordArgs=record_args } when env_complete userTypes env expr ->
    (* Find the actual user type and its composition *)
    let user_type, expected_args = find_record_type userTypes recordCtor in
    (* Validate that the names of each field match up *)
    let expected_args = List.sort_by_first expected_args
    and record_args = List.sort_by_first record_args in
    let get_field_names args = List.map fst args
    and get_field_values args = List.map snd args in
    if get_field_names expected_args = get_field_names record_args then
      (* Validate that the types of each field match up *)
      let expected_types = get_field_values expected_args
      and arg_values = get_field_values record_args in
      if check_sequence userTypes env expected_types arg_values then `Custom user_type.typeName
      else failwith "Argument types for record differed from expected types"
    else failwith ("Argument names for constructor " ^ recordCtor ^ " did not match expected names")
  | ConstructUnion { unionCtor; unionArgs } when env_complete userTypes env expr ->
    (* Find the actual user type and its composition *)
    let user_type, expected_args = find_union_type userTypes unionCtor in
    (* Validate that the types of each field match up *)
    if check_sequence userTypes env expected_args unionArgs then `Custom user_type.typeName
    else failwith "Argument types for union differed from expected types"
  | If { condition; then_branch; else_branch } ->
    let gamma_cond, gamma_then = split userTypes env condition then_branch |> Option.get
    and gamma_cond_, gamma_else = split userTypes env condition else_branch |> Option.get in
    if gamma_cond = gamma_cond_ && typecheck userTypes gamma_cond condition = `Bool then
      (* Both branches share the same environment & return type *)
      let then_ty = typecheck userTypes gamma_then then_branch
      and else_ty = typecheck userTypes gamma_else else_branch in
      if then_ty = else_ty then then_ty
      else failwith "If statement requires types of then/else branches to match"
    else failwith "If statement requires condition to evaluate to boolean"
  | Match { matchValue; matchCases } ->
    let pattern_types = List.map begin fun { patCtor; patArgs; patResult } ->
      let union_type, field_types = find_union_type userTypes patCtor in
      let gamma_value, gamma_result = split userTypes env matchValue patResult |> Option.get in
      let gamma_result = List.combine patArgs field_types |> Env.add_bindings gamma_result in
      (* Repetitive work but I'm not sure how to make this much better without gross code... *)
      if typecheck userTypes gamma_value matchValue = `Custom union_type.typeName then
        (union_type, typecheck userTypes gamma_result patResult)
      else failwith ("Match expression expected pattern of type " ^ union_type.typeName)
    end matchCases in
    if List.all_equal pattern_types then begin
      match pattern_types with
      | [] -> failwith "Match expression expects at least one branch"
      | (_, return_type)::_ -> return_type
    end else failwith "Types of branches in a match expression must be the same"
  | RecordMatch { matchRecord; matchArgs=args; matchBody } ->
    let gamma_value, gamma_body = split userTypes env matchRecord matchBody |> Option.get in
    let _, fields = begin match typecheck userTypes gamma_value matchRecord with
      | `Custom ctor -> find_record_type userTypes ctor
      | _ -> failwith "Record match must receive a record value"
    end in
    let fields = List.sort_by_first fields
    and args = List.sort_by_first args in
    let gamma_body = List.map2 begin fun (field_name, field_type) (arg_name, bound_name) ->
      assert (field_name = arg_name);
      (bound_name, field_type)
    end fields args |> Env.add_bindings gamma_body in
    typecheck userTypes gamma_body matchBody
  | RecordAccess { record; field } when env_complete userTypes env expr ->
    (* This escapes the guarantees of a linear type system, and only makes sense in the
       context of relevant types. E.g. `let x = A { v: Promise*(Int) } in use(x.v); use(x.v)` *)
    let record_ty, fields = begin match typecheck userTypes env record with
      | `Custom ctor -> find_record_type userTypes ctor
      | _ -> failwith "Record access must receive a record value"
    end in
    begin match List.find_opt (fun (name, _) -> name = field) fields with
      | None -> failwith ("Type " ^ record_ty.typeName ^ " does not contain field " ^ field)
      | Some (_, field_ty) -> field_ty
    end
  | Promise { read; write; ty; promiseBody } when env_complete userTypes env expr ->
    let env = env |> Env.add read (`Promise ty) |> Env.add write (`PromiseStar ty) in
    typecheck userTypes env promiseBody
  | Write { promiseStar; newValue; _ } ->
    let gamma_promise, gamma_value = split userTypes env promiseStar newValue |> Option.get in
    let value_ty = typecheck userTypes gamma_value newValue in
    begin match typecheck userTypes gamma_promise promiseStar with
      | `PromiseStar ty when value_ty = (ty :> ty) -> `Unit
      | _ -> failwith ("Expected promise of type " ^ string_of_ty value_ty)
    end
  | Read { promise } ->
    begin match typecheck userTypes env promise with
      | `Promise value_ty -> (value_ty :> ty)
      | ty -> failwith ("Could not read non-promise type " ^ string_of_ty ty)
    end
  | For { name; first; last; forBody } ->
    begin match split_sequence userTypes env [first; last; forBody] with
      | [env_first; env_last; env_body] ->
        let env_body = Env.add name `Int env_body in
        if typecheck userTypes env_first first = `Int &&
           typecheck userTypes env_last last = `Int
        then typecheck userTypes env_body forBody
        else failwith "For expression expects integer bounds"
      | _ -> failwith "Unable to split environments safely in for expression"
    end
  | While { whileCond; whileBody } ->
    let env_cond, env_body = split userTypes env whileCond whileBody |> Option.get in
    if typecheck userTypes env_cond whileCond = `Bool then
      typecheck userTypes env_body whileBody
    else failwith "While expression expects boolean condition"
  | Unit | Number _ | Boolean _ | ConstructRecord _ | ConstructUnion _
  | RecordAccess _ | Promise _ ->
    failwith ("Leftover variables in env in expr: " ^ string_of_expr expr)

and check_sequence userTypes env types args =
  let envs = split_sequence userTypes env args in
  let to_check = List.combine envs (List.combine args types) in
  List.for_all begin fun (env, (arg, expected_type)) ->
    typecheck userTypes env arg = expected_type
  end to_check

let rec load_env fns env = match fns with
  | [] -> env
  | { funcName; retType; params; _ }::funcs ->
    let env' = Env.add funcName (`Function (List.map snd params, retType)) env in
    load_env funcs env'

let typecheck_fn userTypes env = function
  | { funcName; retType; params; expr } ->
    let env = Env.add_bindings env params in
    let ty = typecheck userTypes env expr in
    if ty <> retType then
      failwith ("Function '" ^ funcName ^ "' return value does not match declared type. Expected " ^ string_of_ty retType ^
                ", Got: " ^ string_of_ty ty)
    else `Function (List.map snd params, retType)
