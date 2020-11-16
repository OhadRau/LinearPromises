module Env = struct
  include Map.Make(String)

  let rec from_bindings = function
    | [] -> empty
    | (k, v)::rest -> add k v (from_bindings rest)

  let add_bindings env bindings =
    let new_bindings = from_bindings bindings in
    union (fun _ _ new_binding -> Some new_binding) env new_bindings
  
  let to_string show env =
    fold (fun k v result -> k ^ ": " ^ show v ^ "; " ^ result) env ""

  let print show env =
    print_endline @@ to_string show env
end

module Vars = struct
  include Set.Make(String)
  let concat envs =
    List.fold_left union empty envs
end