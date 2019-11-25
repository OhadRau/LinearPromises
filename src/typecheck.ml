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

(* typecheck : linear -> env -> expr -> (ty * linear * env) *)
(* TODO: Assert that delta = {} at the end of every program *)
let rec typecheck linEnv env = function
  | Unit -> (`Unit, linEnv, env)
  | Boolean _ -> (`Bool, linEnv, env)
  | Number _ -> (`Int, linEnv, env)
  | Variable v when Env.mem v env ->
    (Env.find v env, linEnv, env)
  | Variable v -> failwith ("Unknown variable: " ^ v)
  | Let { id; annot; body;
          value = Promise { ty } } ->
    if annot = `PromiseStar ty then begin
      let linEnvP = LinEnv.add id linEnv
      and envP = Env.add id annot env in
      match typecheck linEnvP envP body with
      | (tau', linEnv', _) ->
        let notEliminated = LinEnv.diff linEnv' linEnv in
        if notEliminated = linEnv' then
          (tau', linEnv', env)
        else failwith ("Linear variables not consumed: " ^ LinEnv.to_string @@ LinEnv.diff linEnv' linEnv)
    end else
      failwith ("Unmatched promise type in let expression: Got " ^ string_of_ty (`PromiseStar ty) ^ ", expected " ^ string_of_ty annot)
  | Let { id; annot; value; body } ->
    let (ty, linEnv0, env0) = typecheck linEnv env value in
    if ty = annot then begin
      let envId = Env.add id annot env0 in
      let (tau', linEnv1, env1) = typecheck linEnv0 envId body in
      (tau', LinEnv.inter linEnv0 linEnv1, Env.inter env0 env1)
    end else
      failwith ("Unmatched type in let expression: Got " ^ string_of_ty ty ^ ", expected " ^ string_of_ty annot)
  | Apply { fn; args } -> begin
    let rec evalArgs linEnv' env' params args =
      begin match params, args with
      | [], [] -> ([], linEnv', env')
      | [], _ | _, [] -> failwith "Type mismatch in function application: number of parameters differed between expected and actual"
      | expected_ty::prest, (Variable v as arg)::arest ->
        begin match expected_ty, typecheck linEnv' env' arg with
        | `PromiseStar tau, (`PromiseStar tau', linEnvN, envN) when tau = tau' ->
          evalArgs (LinEnv.remove v linEnvN) (Env.replace v (`Promise tau) envN) prest arest
        | `Promise tau, (`PromiseStar tau', linEnvN, envN) when tau = tau' ->
          evalArgs linEnvN envN prest arest
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
        let (t, linEnvN, envN) = typecheck linEnv' env' expr in
        if t = expected_ty then
          evalArgs linEnvN envN prest arest
        else begin
          print_endline @@ "Expected: " ^ string_of_ty expected_ty;
          print_endline @@ "Got: " ^ string_of_ty t;
          Env.print string_of_ty env';
          failwith "Type mismatch in function application"
        end
      end in
    match typecheck linEnv env fn with
    | (`Function (argTypes, returnType), _, _) ->
      let (_, linEnv', env') = evalArgs linEnv env argTypes args in
      (returnType, linEnv', env')
    | _ -> failwith "Cannot call a non-function"
    end
  | Write { promiseStar = Variable p; newValue } -> begin
    let (ty, _, _) = typecheck linEnv env newValue in
    if Env.mem p env then
      match Env.find p env with
      | `PromiseStar tau when ty = (tau :> ty) ->
        (`Unit, LinEnv.remove p linEnv, Env.replace p (`Promise tau) env)
      | _ -> failwith "Write location does not match expected type"
    else
      failwith "Write location does not exist"
  end
  | Write _ -> failwith "Write location is not an identifier"
  | Read { promise } -> begin
    match typecheck linEnv env promise with
    | (`PromiseStar tau, linEnv', env')
    | (`Promise tau, linEnv', env') ->
      ((tau :> ty), linEnv', env')
    | _ -> failwith "Read location is not a promise"
  end
  | If { condition; then_branch; else_branch } -> begin
    match typecheck linEnv env condition with
    | (`Bool, _, _) ->
      let (thenType, thenLinEnv, thenEnv) = typecheck linEnv env then_branch
      and (elseType, elseLinEnv, _) = typecheck linEnv env else_branch in
      if thenType <> elseType then
        failwith "Type mismatch between conditional branches"
      else if thenLinEnv <> elseLinEnv then
        failwith "Conditional branches do not consume the same promises"
      else
        (* HACK: thenEnv, elseEnv can replace the type of a promise --
           how do we copy these modified types without allowing new vars
           declared in the branches to escape their scope? *)
        (thenType, thenLinEnv, Env.cover linEnv thenLinEnv env thenEnv)
    | _ -> failwith "If condition is not a boolean"
  end
  | Async { application } ->
    let (_, linEnv', env') = typecheck linEnv env application in
    (`Unit, linEnv', env')
  | For { name; first; last; forBody } ->
    let (firstTy, _, _) = typecheck linEnv env first
    and (lastTy, _, _) = typecheck linEnv env last in
    if firstTy <> `Int || lastTy <> `Int then
      failwith "For loop bounds must be integers"
    else begin
      let envName = Env.add name `Int env in
      match typecheck LinEnv.empty envName forBody with
      | (ty, linEnv', _) when linEnv' = LinEnv.empty ->
        (ty, linEnv, env)
      | _ -> failwith "Unused promises in for loop"
    end
  | While { whileCond; whileBody } ->
    let (condTy, _, _) = typecheck linEnv env whileCond in
    if condTy <> `Bool then
      failwith "While condition must be a bool"
    else begin
      match typecheck LinEnv.empty env whileBody with
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

let rec load_params args linEnv env = match args with
  | [] -> (linEnv, env)
  | (name, `PromiseStar ty)::params ->
    let env' = Env.add name (`PromiseStar ty) env
    and linEnv' = LinEnv.add name linEnv in
    load_params params linEnv' env'
  | (name, ty)::params ->
    let env' = Env.add name ty env in
    load_params params linEnv env'

let typecheck_fn env = function
  | { funcName; retType; params; expr } ->
    let linEnv', env' = load_params params (LinEnv.empty) env in
    let (ty, linEnv'', _) = typecheck linEnv' env' expr in
    if LinEnv.empty <> linEnv'' then
      failwith "Function must eliminate all linear variables"
    else if ty <> retType then
      failwith ("Function '" ^ funcName ^ "' return value does not match declared type. Expected " ^ string_of_ty retType ^
                ", Got: " ^ string_of_ty ty)
    else `Function (List.map snd params, retType)