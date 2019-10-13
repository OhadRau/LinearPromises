open Lang
open Typecheck

let () =
  (*
    let p : Promise*(Int) = promise Int in
    if true then
      p <- 10
    else
      f(p)
    end
  *)
  let program =
    Let { id="p"; annot=(`PromiseStar `Int); value=(Promise {ty=`Int});
          body=(If { condition=(Boolean true);
                     then_branch=(Write { promiseStar=(Variable "p");
                                          newValue=(Number 10) });
                     else_branch=(Apply { fn=(Variable "f");
                                          args=[Variable "p"] }) }) }
  and delta = LinEnv.empty
  and gamma = Env.empty |> Env.add "f" (`Function ([`PromiseStar `Int], `Unit)) in
  let (ty, _, _) = typecheck delta gamma program in
  print_endline (string_of_ty ty)
