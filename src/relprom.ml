open Lang
open Lexing
open Typecheck

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let read eval programName lexbuf =
  match Parser.program Lexer.read lexbuf with
  | exception Lexer.SyntaxError msg ->
    Printf.fprintf stderr "Syntax error: %s at %s\n"
      (string_of_position lexbuf)
      msg
  | exception _ ->
    Printf.fprintf stderr "Syntax error at %s\n"
      (string_of_position lexbuf)
  | e -> eval { e with programName }

let read_file eval filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  (* Get the program name by removing the full path, removing the
     file extension, and capitalizing the first character. *)
  let programName = filename
    |> Filename.remove_extension
    |> Filename.basename
    |> JavaCG.make_valid_java_ident
    |> String.capitalize_ascii in
   read eval programName lexbuf

let () =
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
