open Lang
open Contexts

let uniq =
  let counter = ref 0 in
  fun str ->
    let num = !counter in
    counter := num + 1;
    Printf.sprintf "%s%%%d" str num

let original_name id =
  String.split_on_char '%' id |> List.hd

let rec make_unique env = function
  | Variable var -> Variable (Env.find var env)
  | Unit -> Unit
  | Number n -> Number n
  | Boolean b -> Boolean b
  | Let { id; annot; value; body } ->
    let new_id = uniq id in
    let new_env = Env.add id new_id env in
    Let { id = new_id; annot; value = make_unique env value;
          body = make_unique new_env body }
  | If { condition; then_branch; else_branch } ->
    If { condition = make_unique env condition;
         then_branch = make_unique env then_branch;
         else_branch = make_unique env else_branch }
  | Match { matchValue; matchCases } ->
    let matchCases = List.map begin fun pattern ->
      let env =
        List.map (fun name -> (name, uniq name)) pattern.patArgs |> Env.add_bindings env in
      { pattern with patArgs = List.map (fun id -> Env.find id env) pattern.patArgs;
                     patResult = make_unique env pattern.patResult }
    end matchCases in
    Match { matchValue = make_unique env matchValue; matchCases }
  | RecordMatch { matchRecord; matchArgs; matchBody } ->
    let env_body =
      List.map (fun (_, name) -> (name, uniq name)) matchArgs |> Env.add_bindings env in
    let matchArgs =
      List.map (fun (field, id) -> (field, Env.find id env_body)) matchArgs in
    RecordMatch { matchRecord = make_unique env matchRecord; matchArgs;
                  matchBody = make_unique env_body matchBody }
  | Apply { fn; args } ->
    Apply { fn = make_unique env fn; args = List.map (make_unique env) args }
  | ConstructUnion { unionCtor; unionArgs } ->
    ConstructUnion { unionCtor; unionArgs = List.map (make_unique env) unionArgs }
  | ConstructRecord { recordCtor; recordArgs } ->
    let recordArgs =
      List.map (fun (field, value) -> (field, make_unique env value)) recordArgs in
    ConstructRecord{ recordCtor; recordArgs }
  | RecordAccess { record; field } ->
    RecordAccess { record = make_unique env record; field }
  | Promise { ty } -> Promise { ty }
  | Write { promiseStar; newValue; unsafe } ->
    Write { promiseStar = make_unique env promiseStar;
            newValue = make_unique env newValue; unsafe }
  | Read { promise } -> Read { promise = make_unique env promise }
  | Async { application } -> Async { application = make_unique env application }
  | For { name; first; last; forBody } ->
    let new_name = uniq name in
    let env_body = Env.add name new_name env in
    For { name = new_name; first = make_unique env first;
          last = make_unique env last; forBody = make_unique env_body forBody }
  | While { whileCond; whileBody } ->
    While { whileCond = make_unique env whileCond;
            whileBody = make_unique env whileBody }

let preprocess_idents base_env func =
  let env =
    Env.mapi (fun key _value -> key) base_env in
  let env =
    List.map (fun (name, _) -> (name, uniq name)) func.params |> Env.add_bindings env in
  { func with params =
                List.map (fun (name, ty) -> (Env.find name env, ty)) func.params;
              expr = make_unique env func.expr }
