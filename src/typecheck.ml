open Lang

module LinEnv = struct
  include Set.Make(String)

  let to_string linEnv =
    fold (fun s result -> s ^ "; " ^ result) linEnv ""
  
  let print linEnv =
    print_endline @@ to_string linEnv
end

module Env = struct
  include Map.Make(String)

  let inter =
    let f _key val0 val1 = match (val0, val1) with
    | (Some v0, Some v1) when v0 = v1 -> Some v0
    | (Some _a, Some b) -> Some b (* Take the updated type *)
      (*failwith ("Type mismatch in env for variable " ^ key ^ ": " ^ string_of_ty a ^ " vs. " ^ string_of_ty b)*)
    | (Some v, None) | (None, Some v) -> Some v
    | (None, None) -> None in
    merge f
  
  let replace key value env =
    add key value (remove key env)

  (* Apply deltas to oldEnv from newEnv based on which linear
     variables were eliminated in the new linear environment *)
  let cover oldLinEnv newLinEnv oldEnv newEnv =
    let deltas = LinEnv.diff oldLinEnv newLinEnv in
    let copy key oldType =
      if LinEnv.mem key deltas then
        find key newEnv
      else
        oldType in
    mapi copy oldEnv
  
  let print show env =
    print_endline @@ fold (fun k v result -> k ^ ": " ^ show v ^ "; " ^ result) env ""
end

let resolveUserType userTypes = function
  | `Custom tyName -> begin
    match List.find_opt (fun {typeName; _} -> typeName = tyName) userTypes with
    | Some userTy -> userTy
    | _ -> failwith (tyName ^ " cannot be resolved to a type")
  end
  | ty -> failwith (string_of_ty ty ^ " is not a user-defined type")

(* Liftable decides whether a *-value can be lifted to top-level in a type; if a type
   contains (or can contain) a *-value, then it will be liftable. Primitive types are
   not liftable and Promise*(_) is automatically liftable since it's a *-value. *)
let rec liftable_ userTypes history = function
  | `Int | `Bool | `Unit -> false
  | `Promise t -> liftable userTypes (t :> ty)
  | `Custom c when List.mem c history -> false
  | `Custom c -> begin
    let resolved = resolveUserType userTypes (`Custom c) in
    let history = c::history in
    match resolved.typeDefn with
    | Record fields -> List.exists (fun (_, ty) -> liftable userTypes ~history ty) fields
    | Union cases -> List.exists (fun (_, args) -> List.exists (liftable userTypes ~history) args) cases
  end
  | `PromiseStar _ -> true
  | `Function _ -> failwith "Function lifting is not currently supported"

and liftable =
  let table = Hashtbl.create 1000 in
  fun userTypes ?(history=[]) ty ->
    if Hashtbl.mem table ty then
      Hashtbl.find table ty
    else begin
      let result = liftable_ userTypes history ty in
      Hashtbl.add table ty result;
      result
    end

(* typecheck : ty_decl list -> linear -> env -> expr -> (ty * linear * env) *)
(* TODO: Assert that delta = {} at the end of every program *)
let rec typecheck userTypes linEnv env = function
  | Unit -> (`Unit, linEnv, env)
  | Boolean _ -> (`Bool, linEnv, env)
  | Number _ -> (`Int, linEnv, env)
  | Variable v when Env.mem v env ->
    (Env.find v env, linEnv, env)
  | Variable v -> failwith ("Unknown variable: " ^ v)
  | Let { id; annot; body;
          value = Promise { ty } } ->
    (* If we're trying to construct a Promise* of the correct type *)
    if annot = `PromiseStar ty then begin
      (* Add it to the linear environment & environment *)
      let linEnvP = LinEnv.add id linEnv
      and envP = Env.add id annot env in
      (* Now typecheck the let's body with that in mind *)
      match typecheck userTypes linEnvP envP body with
      | (tau', linEnv', _) ->
        (* notEliminated = the difference between the original environment & the
           one after the body. If the body of the let doesn't eliminate that value
           then it can leak to outside the current scope. *)
        let notEliminated = LinEnv.diff linEnv' linEnv in
        if notEliminated = linEnv' then
          (tau', linEnv', env)
        else failwith ("Linear variables not consumed: " ^ (LinEnv.to_string @@ LinEnv.diff linEnv' linEnv) ^
                       " in definition of '" ^ id ^ "'")
    end else
      failwith ("Unmatched promise type in let expression: Got " ^ string_of_ty (`PromiseStar ty) ^ ", expected " ^ string_of_ty annot)
  | Let { id; annot; value; body } ->
    let (ty, linEnv0, env0) = typecheck userTypes linEnv env value in
    if ty = annot then begin
      let envId = Env.add id annot env0 in
      let (tau', linEnv1, env1) = typecheck userTypes (linEnv0 |> LinEnv.add id) envId body in
      if liftable userTypes annot then
        if LinEnv.subset linEnv1 linEnv0 then
          (tau', LinEnv.inter linEnv0 linEnv1, Env.inter env0 env1)
        else failwith ("Linear environment leaked vars {" ^ LinEnv.to_string linEnv1 ^ "} in definition of '" ^ id ^ "'")
      else
        (tau', LinEnv.inter linEnv0 linEnv1, Env.inter env0 env1)
    end else
      failwith ("Unmatched type in let expression: Got " ^ string_of_ty ty ^ ", expected " ^ string_of_ty annot)
  | Apply { fn; args } -> begin
    let rec evalArgs linEnv' env' params args =
      begin match params, args with
      | [], [] -> ([], linEnv', env')
      | [], _ | _, [] -> failwith "Type mismatch in function application: number of parameters differed between expected and actual"
      | expected_ty::prest, (Variable v as arg)::arest ->
        begin match expected_ty, typecheck userTypes linEnv' env' arg with
        | `PromiseStar tau, (`PromiseStar tau', linEnvN, envN) when tau = tau' ->
          evalArgs (LinEnv.remove v linEnvN) (Env.replace v (`Promise tau) envN) prest arest
        | `Promise tau, (`PromiseStar tau', linEnvN, envN) when tau = tau' ->
          evalArgs linEnvN envN prest arest
        | _, (t, linEnvN, envN) when t = expected_ty && liftable userTypes t ->
          evalArgs (LinEnv.remove v linEnvN) envN prest arest
        | _, (t, linEnvN, envN) when t = expected_ty ->
          evalArgs linEnvN envN prest arest
        | _, (t, _, _) -> begin
            print_endline @@ "Expected: " ^ string_of_ty expected_ty;
            print_endline @@ "Got: " ^ string_of_ty t;
            Env.print string_of_ty env';
            failwith "Type mismatch in function application"
          end
        end
      | expected_ty::prest, expr::arest ->
        let (t, linEnvN, envN) = typecheck userTypes linEnv' env' expr in
        if t = expected_ty then
          evalArgs linEnvN envN prest arest
        else begin
          print_endline @@ "Expected: " ^ string_of_ty expected_ty;
          print_endline @@ "Got: " ^ string_of_ty t;
          Env.print string_of_ty env';
          failwith "Type mismatch in function application"
        end
      end in
    match typecheck userTypes linEnv env fn with
    | (`Function (argTypes, returnType), _, _) ->
      let (_, linEnv', env') = evalArgs linEnv env argTypes args in
      (returnType, linEnv', env')
    | _ -> failwith "Cannot call a non-function"
    end
  | ConstructUnion { unionCtor; unionArgs } ->
    let ty = List.find_opt begin function
      | { typeDefn = Union cases; _ }
        when List.exists (fun (caseName, _) -> caseName = unionCtor) cases -> true
      | _ -> false
    end userTypes in
    begin match ty with
      | Some { typeName; typeDefn = Union cases } -> begin
        let _, params = List.find (fun (caseName, _) -> caseName = unionCtor) cases in

        let rec evalArgs linEnv' env' params args =
          begin match params, args with
          | [], [] -> ([], linEnv', env')
          | [], _ | _, [] -> failwith "Type mismatch in union construction: number of parameters differed between expected and actual"
          | expected_ty::prest, (Variable v as arg)::arest ->
            begin match expected_ty, typecheck userTypes linEnv' env' arg with
            | `PromiseStar tau, (`PromiseStar tau', linEnvN, envN) when tau = tau' ->
              evalArgs (LinEnv.remove v linEnvN) (Env.replace v (`Promise tau) envN) prest arest
            | `Promise tau, (`PromiseStar tau', linEnvN, envN) when tau = tau' ->
              evalArgs linEnvN envN prest arest
            | _, (t, linEnvN, envN) when t = expected_ty && liftable userTypes t ->
              evalArgs (LinEnv.remove v linEnvN) envN prest arest
            | _, (t, linEnvN, envN) when t = expected_ty ->
              evalArgs linEnvN envN prest arest
            | _, (t, _, _) -> begin
                print_endline @@ "Expected: " ^ string_of_ty expected_ty;
                print_endline @@ "Got: " ^ string_of_ty t;
                Env.print string_of_ty env';
                failwith "Type mismatch in union construction"
              end
            end
          | expected_ty::prest, expr::arest ->
            let (t, linEnvN, envN) = typecheck userTypes linEnv' env' expr in
            if t = expected_ty then
              evalArgs linEnvN envN prest arest
            else begin
              print_endline @@ "Expected: " ^ string_of_ty expected_ty;
              print_endline @@ "Got: " ^ string_of_ty t;
              Env.print string_of_ty env';
              failwith "Type mismatch in union construction"
            end
          end in

        let (_, linEnvA, envA) = evalArgs linEnv env params unionArgs in

        let argMatches arg expected =
          let (actual_ty, _, _) = typecheck userTypes linEnv env arg in
          expected = actual_ty in
        if List.for_all2 argMatches unionArgs params then
          (* Apply the updated environment *)
          (`Custom typeName, linEnvA, envA)
        else failwith ("Union constructor " ^ unionCtor ^ " applied to invalid arguments")
      end
      | _ -> failwith ("Cannot construct value with union constructor " ^ unionCtor)
    end
  | ConstructRecord { recordCtor; recordArgs } ->
    let ty = List.find_opt begin function
      | { typeName; _ } when typeName = recordCtor -> true
      | _ -> false
    end userTypes in
    begin match ty with
      | Some { typeName; typeDefn = Record fields } -> begin
        let fieldsSorted =
          List.sort (fun (name1, _) (name2, _) -> compare name1 name2) fields
        and argsSorted =
          List.sort (fun (name1, _) (name2, _) -> compare name1 name2) recordArgs in
        let argMatches (argName, arg) (expectedName, expected_ty) =
          let (actual_ty, _, _) = typecheck userTypes linEnv env arg in
          expected_ty = actual_ty && argName = expectedName in
        if List.for_all2 argMatches argsSorted fieldsSorted then
          (`Custom typeName, linEnv, env)
        else failwith ("Record constructor " ^ recordCtor ^ " applied to invalid arguments")
      end
      | _ -> failwith ("Cannot construct value with union constructor " ^ recordCtor)
    end
  | RecordAccess { record; field } -> begin
    let (record_ty, linEnv', env') = typecheck userTypes linEnv env record in
    let record_fields = match resolveUserType userTypes record_ty with
      | { typeDefn = Record fields; _ } -> fields
      | _ -> failwith "Cannot access fields of non-record type" in
    match List.find_opt (fun (name, _) -> name = field) record_fields with
    | Some (_, ty) -> (ty, linEnv', env')
    | None -> failwith ("Type " ^ string_of_ty record_ty ^ " has no field " ^ field)
    end
  | Write { promiseStar = Variable p; newValue; unsafe } -> begin
    let (ty, _, _) = typecheck userTypes linEnv env newValue in
    if Env.mem p env then
      match Env.find p env with
      | `PromiseStar tau when ty = (tau :> ty) ->
        (`Unit, LinEnv.remove p linEnv, Env.replace p (`Promise tau) env)
      | `Promise tau when ty = (tau :> ty) && unsafe ->
        (`Unit, linEnv, env)
      | _ -> failwith "Write location does not match expected type"
    else
      failwith "Write location does not exist"
  end
  | Write _ -> failwith "Write location is not an identifier"
  | Read { promise } -> begin
    match typecheck userTypes linEnv env promise with
    | (`PromiseStar tau, linEnv', env')
    | (`Promise tau, linEnv', env') ->
      ((tau :> ty), linEnv', env')
    | _ -> failwith "Read location is not a promise"
  end
  | If { condition; then_branch; else_branch } -> begin
    match typecheck userTypes linEnv env condition with
    | (`Bool, _, _) ->
      let (thenType, thenLinEnv, thenEnv) = typecheck userTypes linEnv env then_branch
      and (elseType, elseLinEnv, _) = typecheck userTypes linEnv env else_branch in
      if thenType <> elseType then
        failwith "Type mismatch between conditional branches"
      else
        (* HACK: thenEnv, elseEnv can replace the type of a promise --
           how do we copy these modified types without allowing new vars
           declared in the branches to escape their scope? *)
        let mergedLinEnv = LinEnv.union thenLinEnv elseLinEnv in
        (thenType, mergedLinEnv, Env.cover linEnv mergedLinEnv env thenEnv)
    | _ -> failwith "If condition is not a boolean"
  end
  | Match { matchValue; matchCases } -> begin
    let (val_ty, linEnv', env') = typecheck userTypes linEnv env matchValue in
    (* If we match against a variable in the linear environment, eliminate it *)
    let linEnv' = match matchValue with
    | Variable v when LinEnv.mem v linEnv' -> LinEnv.remove v linEnv'
    | _ -> linEnv' in
    let ty_name = match val_ty with
      | `Custom name -> name
      | _ -> failwith "Cannot match against a primitive type" in
    let ty_defn = match List.find (fun {typeName; _} -> typeName = ty_name) userTypes with
      | { typeDefn = Union cases; _ } -> cases
      | _ -> failwith "Must match against a union type" in
    (* Check:
         1. All patterns: val_ty
         2. All val_ty's included in patterns
         3. All branches have same result type
    *)
    let ctorsExpected = List.map (fun (ctor, _) -> ctor) ty_defn |> List.sort String.compare
    and ctorsApplied = List.map (fun { patCtor; _ } -> patCtor) matchCases |> List.sort String.compare in
    let rec check_branches = function
      | {patCtor; patArgs; patResult}::rest ->
        let (_, ctor_arg_types) = List.find (fun (ctor, _) -> ctor = patCtor) ty_defn in
        let args_and_types = List.map2 (fun l r -> (l, r)) patArgs ctor_arg_types in
        let (linEnv'', env'') = List.fold_left begin fun (linEnv, env) (arg, ty) ->
          let env' = Env.add arg ty env
          and linEnv' =
            if liftable userTypes ty then LinEnv.add arg linEnv else linEnv in
          (linEnv', env')
        end (linEnv', env') args_and_types in
        let (result_ty, linEnv''', env''') = typecheck userTypes linEnv'' env'' patResult in
        (* if NewLEnv - OldLEnv = 0 then no new linear variables in scope *)
        if LinEnv.diff linEnv''' linEnv = LinEnv.empty then
          let result = (result_ty, linEnv''', Env.cover linEnv' linEnv''' env' env''') in
          if rest = [] || check_branches rest = result then
            result
          else failwith "Match expression branches must have the same type"
        else failwith "Leftover linear variable in match branch"
      | [] -> failwith "Match expression must contain at least one branch" in
    let (result_ty, linEnv'', env'') = check_branches matchCases in
    if ctorsApplied = ctorsExpected then
      (result_ty, linEnv'', env'')
    else failwith "Match cases must exactly match all union cases"
  end
  | Async { application } ->
    let (_, linEnv', env') = typecheck userTypes linEnv env application in
    (`Unit, linEnv', env')
  | For { name; first; last; forBody } ->
    let (firstTy, _, _) = typecheck userTypes linEnv env first
    and (lastTy, _, _) = typecheck userTypes linEnv env last in
    if firstTy <> `Int || lastTy <> `Int then
      failwith "For loop bounds must be integers"
    else begin
      let envName = Env.add name `Int env in
      match typecheck userTypes LinEnv.empty envName forBody with
      | (ty, linEnv', _) when linEnv' = LinEnv.empty ->
        (ty, linEnv, env)
      | _ -> failwith "Unused promises in for loop"
    end
  | While { whileCond; whileBody } ->
    let (condTy, _, _) = typecheck userTypes linEnv env whileCond in
    if condTy <> `Bool then
      failwith "While condition must be a bool"
    else begin
      match typecheck userTypes LinEnv.empty env whileBody with
      | (ty, linEnv', _) when linEnv' = LinEnv.empty ->
        (ty, linEnv, env)
      | _ -> failwith "Unused promises in while loop"
    end
  | Promise _ -> failwith "Promise must be named"

let rec load_env fns env = match fns with
  | [] -> env
  | { funcName; retType; params; _ }::funcs ->
    let env' = Env.add funcName (`Function (List.map snd params, retType)) env in
    load_env funcs env'

let rec load_params args userTypes linEnv env = match args with
  | [] -> (linEnv, env)
  | (name, ty)::params when liftable userTypes ty ->
    let env' = Env.add name ty env
    and linEnv' = LinEnv.add name linEnv in
    load_params params userTypes linEnv' env'
  | (name, ty)::params ->
    let env' = Env.add name ty env in
    load_params params userTypes linEnv env'

let typecheck_fn userTypes env = function
  | { funcName; retType; params; expr } ->
    let linEnv', env' = load_params params userTypes (LinEnv.empty) env in
    let (ty, linEnv'', _) = typecheck userTypes linEnv' env' expr in
    if LinEnv.empty <> linEnv'' then
      let leftovers = LinEnv.elements linEnv'' |> String.concat "," in
      failwith ("Function '" ^ funcName ^ "' must eliminate all linear variables. {" ^
                leftovers ^ "} were not eliminated.")
    else if ty <> retType then
      failwith ("Function '" ^ funcName ^ "' return value does not match declared type. Expected " ^ string_of_ty retType ^
                ", Got: " ^ string_of_ty ty)
    else `Function (List.map snd params, retType)