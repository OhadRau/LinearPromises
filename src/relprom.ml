open Lang
open Typecheck

let () =
  let program =
    Let { id="p"; annot=(`PromiseStar `Int); value=(Promise {ty=`Int});
          body=(If { condition=(Boolean true);
                     then_branch=(Write { promiseStar=(Variable "p");
                                          newValue=(Number 10) });
                     else_branch=(Apply { fn=(Variable "f");
                                          args=[Variable "p"] }) }) }
  and delta = LinEnv.empty
  and gamma = Env.empty |> Env.add "f" (`Function ([`PromiseStar `Int], `Unit)) in
  let (_, _, _) = typecheck delta gamma program in
  ()
