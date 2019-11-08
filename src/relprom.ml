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
    (* Let { id="p"; annot=(`PromiseStar `Int); value=(Promise {ty=`Int});
      body = Let { id="q"; annot=(`PromiseStar `Int); value=(Promise {ty=`Int});
          body=(If { condition=(Boolean true);
                          then_branch=(Write { promiseStar=(Variable "p");
                                                newValue=(Number 10) });
                          else_branch=(Apply { fn=(Variable "f");
                                                args=[Variable "p"] }) }) } } *)
    Let { id="p"; annot=(`PromiseStar `Int); value=(Promise {ty=`Int});
          body=(Let { id="_"; annot=`Unit; value =
                  If { condition=(Boolean true);
                        then_branch=(Write { promiseStar=(Variable "p");
                                             newValue=(Number 10) });
                        else_branch=(Apply { fn=(Variable "f");
                                              args=[Variable "p"] }) };
                body = Apply { fn = (Variable "unsafeWrite"); args = [Variable "p"; Number 42] }
          }) }
  and delta = LinEnv.empty
  and gamma = Env.empty |> Env.add "f" (`Function ([`PromiseStar `Int], `Unit))
                        |> Env.add "unsafeWrite" (`Function ([`Promise `Int; `Int], `Unit)) in
  let (ty, _, _) = typecheck delta gamma program in
  print_endline (string_of_ty ty);
  print_endline "---------------";
  let javaProgram = JavaCG.emit program in
  print_endline javaProgram
