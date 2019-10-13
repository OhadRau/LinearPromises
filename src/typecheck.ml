open Lang

module Env = struct
  include Map.Make(String)

  let inter =
    let f _key val0 val1 = match (val0, val1) with
    | (Some v0, Some v1) when v0 = v1 -> Some v0
    | (Some _, Some _) -> failwith "Key mismatch in env"
    | (Some v, None) | (None, Some v) -> Some v
    | (None, None) -> None in
    merge f
end
module LinEnv = Set.Make(String)

(* typecheck : linear -> env -> expr -> (ty * linear * env) *)
(* TODO: Assert that delta = {} at the end of every program *)
let rec typecheck linEnv env = function
  | Boolean _ -> (`Bool, linEnv, env)
  | Number _ -> (`Int, linEnv, env)
  | Variable v when Env.mem v env ->
    (Env.find v env, linEnv, env)
  | Variable _ -> failwith "Unknown variable"
  | Let { id; annot; body;
          value = Promise { ty } } ->
    if annot = `PromiseStar ty then begin
      let linEnvP = LinEnv.add id linEnv
      and envP = Env.add id annot env in
      match typecheck linEnvP envP body with
      | (_, linEnv', _) when linEnv' <> linEnv ->
        failwith "Linear variable not consumed"
      | (tau', _, _) -> (tau', linEnv, env)
    end else
      failwith "Unmatched promise type in let expression"
  | Let { id; annot; value; body } ->
    let (ty, linEnv0, env0) = typecheck linEnv env value in
    if ty = annot then begin
      let envId = Env.add id annot env in
      let (tau', linEnv1, env1) = typecheck linEnv envId body in
      (tau', LinEnv.inter linEnv0 linEnv1, Env.inter env0 env1)
    end else
      failwith "Unmatched type in let expression"
  | Apply { fn; args } -> begin
    let rec evalArgs linEnv' env' = function
      | [] -> ([], linEnv', env')
      | (Variable v as arg)::rest -> begin
        match typecheck linEnv env arg with
        | (`PromiseStar _ as p, _, _) ->
          let (tys, linEnvN, envN) = 
            evalArgs (LinEnv.remove v linEnv') (Env.remove v env') rest in
          (p::tys, linEnvN, envN)
        | (t, linEnvN, envN) ->
          let (tys, _, _) = evalArgs linEnv' env' rest in
          (t::tys, linEnvN, envN)
      end
      | expr::rest ->
        let (t, _, _) = typecheck linEnv env expr
        and (tys, linEnvN, envN) = evalArgs linEnv' env' rest in
        (t::tys, linEnvN, envN) in
    let (tys, linEnv', env') = evalArgs linEnv env args in
    match typecheck linEnv env fn with
    | (`Function (argTypes, returnType), _, _) ->
      if List.for_all2 (=) argTypes tys then
        (returnType, linEnv', env')
      else
        failwith "Type mismatch in function application"
    | _ -> failwith "Cannot call a non-function"
  end
  | Write { promiseStar = Variable p; newValue } -> begin
    let (ty, _, _) = typecheck linEnv env newValue in
    if Env.mem p env then
      match Env.find p env with
      | `PromiseStar tau when ty = (tau :> ty) ->
        (`Unit, LinEnv.remove p linEnv, Env.remove p env)
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
      let (thenType, thenLinEnv, _) = typecheck linEnv env then_branch
      and (elseType, elseLinEnv, _) = typecheck linEnv env else_branch in
      if thenType <> elseType then
        failwith "Type mismatch between conditional branches"
      else if thenLinEnv <> elseLinEnv then
        failwith "Conditional branches do not consume the same promises"
      else
        (thenType, thenLinEnv, env)
    | _ -> failwith "If condition is not a boolean"
  end
  | Async { application } ->
    let (_, linEnv', env') = typecheck linEnv env application in
    (`Unit, linEnv', env')
  | Promise _ -> failwith "Promise must be named"
