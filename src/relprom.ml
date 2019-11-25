open Lang
open Lexing
open Typecheck

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let read eval lexbuf =
  match Parser.program Lexer.read lexbuf with
  | exception Lexer.SyntaxError msg ->
    Printf.fprintf stderr "Syntax error: %s at %s\n"
      (string_of_position lexbuf)
      msg
  | exception _ ->
    Printf.fprintf stderr "Syntax error at %s\n"
      (string_of_position lexbuf)
  | e -> eval e

let read_file eval filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  read eval lexbuf

let () =
  (*
  fun g() {
    let p : Promise*(Int) = promise Int in
    if true then
      p <- 10
    else
      f(p)
    end
    unsafeWrite(p, 42)
  }
  *)
  (*
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
          }) } in *)
  let eval program =
    let gamma = Env.empty |> Env.add "unsafeWrite" (`Function ([`Promise `Int; `Int], `Unit))
                          |> Env.add "f" (`Function ([`PromiseStar `Int], `Unit))
                          |> load_env program.funcs in
    let typecheck_and_print func =
      print_endline (string_of_expr func.expr);
      print_endline "---------------";
      let ty = typecheck_fn gamma func in
      print_endline (func.funcName ^ ": " ^ string_of_ty ty);
      print_endline "---------------" in
    List.iter typecheck_and_print program.funcs;
    let javaProgram = JavaCG.emit_program program in
    print_endline javaProgram in

  read_file eval (if Array.length Sys.argv > 1 then Sys.argv.(1) else "example.txt")
