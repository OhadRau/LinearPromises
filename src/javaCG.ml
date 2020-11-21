open Lang

let make_valid_java_ident string =
  let buff = Buffer.create (String.length string) in
  let add_if_valid index char = match char with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' -> Buffer.add_char buff char
    | '0' .. '9' when index > 0 -> Buffer.add_char buff char
    | _ -> () in
  String.iteri add_if_valid string;
  Buffer.contents buff

let make_id =
  let unique = ref 0 in
  function
  | "_" ->
    let id = "$_" ^ string_of_int !unique in
    unique := !unique + 1;
    id
  | id -> id

let rec java_type = function
  | `Int -> "Integer"
  | `Bool -> "Boolean"
  | `String -> "String"
  | `Unit -> "Unit" (* Unit class *)
  | `Custom name -> name
  | `Promise k -> Printf.sprintf "Promise<%s>" (java_type (k :> ty))
  | `PromiseStar k -> Printf.sprintf "Promise<%s>" (java_type (k :> ty))
  | `Function (_, _) -> failwith "Cannot create temp function variable"

let rec emit_args userTypes = function
  | [] -> ""
  | [arg] -> emit userTypes ~in_expr:true arg
  | arg::args ->
    Printf.sprintf "%s, %s"
      (emit userTypes ~in_expr:true arg) (emit_args userTypes args)

and emit userTypes ?(in_expr=false) ?(is_final=false) = function 
  | Let { id = "_"; value = (For _) as loop; body; _ }
  | Let { id = "_"; value = (While _) as loop; body; _ } ->
    Printf.sprintf "%s %s"
      (emit userTypes ~in_expr loop) (emit userTypes ~is_final body)
  | Let { id; annot; value; body } ->
    let ty = java_type annot in
    Printf.sprintf "%s %s = %s; %s"
      ty (make_id id) (* = *) (emit userTypes ~in_expr:true value) (* ; *) (emit userTypes ~is_final body)
  | Promise { read; write; ty; promiseBody } ->
    let ty' = java_type (ty :> ty) in
    Printf.sprintf "Promise<%s> %s = new Promise<%s>(); Promise<%s> %s = %s; %s"
      ty' read ty' ty' write read (emit userTypes ~is_final promiseBody)
  | If { condition; then_branch; else_branch } when in_expr ->
    Printf.sprintf "((%s) ? (%s) : (%s))"
      (emit userTypes ~in_expr condition)
      (emit userTypes ~in_expr then_branch) (emit userTypes ~in_expr else_branch)
  | If { condition; then_branch; else_branch } ->
    Printf.sprintf "if (%s) { %s } else { %s }"
      (emit userTypes ~in_expr:true condition)
      (emit userTypes ~is_final then_branch) (emit userTypes ~is_final else_branch)
  | For _ | While _ when in_expr ->
    failwith "Cannot use for/while loop inside of an expression"
  | For { name; first; last; forBody } ->
    (* TODO: What if last < first? What if type(first) != int? *)
    Printf.sprintf "for (int %s = %s; %s < %s; %s++) { %s }%s"
      name (* = *) (emit userTypes ~in_expr:true first) (* ; *)
      name (* < *) (emit userTypes ~in_expr:true last)  (* ; *)
      name (* ++ *)
      (emit userTypes forBody)
      (if is_final then "; return Unit.the;" else "")
  | While { whileCond; whileBody } ->
    Printf.sprintf "while (%s) { %s }%s"
    (emit userTypes ~in_expr:true whileCond) (emit userTypes whileBody)
    (if is_final then "; return Unit.the;" else "")
  | e when not in_expr && is_final ->
    Printf.sprintf "return %s;" (emit userTypes ~in_expr:true e)
  | e when not in_expr && not is_final ->
    Printf.sprintf "Unit.ignore(%s);" (emit userTypes ~in_expr:true e)
  | Variable v -> v
  | Unit -> "Unit.the"
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Infix { mode=#compare as mode; left; right } ->
    Printf.sprintf "(%s).compareTo(%s) %s 0"
      (emit userTypes ~in_expr:true left)
      (emit userTypes ~in_expr:true right) (string_of_compare mode)
  | Infix { mode=#arithmetic as mode; left; right } ->
    Printf.sprintf "(%s) %s (%s)"
      (emit userTypes ~in_expr:true left) (string_of_arithmetic mode)
      (emit userTypes ~in_expr:true right)
  | Infix { mode=#logical as mode; left; right } ->
    Printf.sprintf "(%s) %s (%s)"
      (emit userTypes ~in_expr:true left) (string_of_logical mode)
      (emit userTypes ~in_expr:true right)
  | Not e ->
    Printf.sprintf "!(%s)" (emit userTypes ~in_expr:true e)
  | Apply { fn; args } ->
    Printf.sprintf "%s(%s)"
      (emit userTypes ~in_expr:true fn) (emit_args userTypes args)
  | ConstructUnion { unionCtor; unionArgs } ->
    Printf.sprintf "new %s(%s)"
      unionCtor (emit_args userTypes unionArgs)
  | ConstructRecord { recordCtor; recordArgs } -> begin
    let ty = List.find (fun { typeName; _ } -> typeName = recordCtor) userTypes in
    match ty with
    | { typeDefn = Record fields; _ } ->
      let ordered = List.map begin fun (fieldName, _) ->
        let (_, arg) = List.find (fun (argName, _) -> argName = fieldName) recordArgs in
        arg
      end fields in
      Printf.sprintf "new %s(%s)"
        recordCtor (emit_args userTypes ordered)
    | _ -> failwith "Attempted to call record constructor on a union type"
    end
  | RecordAccess { record; field } ->
    Printf.sprintf "(%s).%s"
      (emit userTypes ~in_expr:true record) field
  | Match { matchValue; matchCases } -> begin
    let firstCtor = match matchCases with
      | {patCtor; _}::_ -> patCtor
      | _ -> failwith "Match expr must contain at least one branch" in
    let matchType = List.find begin function
      | {typeDefn=Union cases; _}
        when List.exists (fun (caseName, _) -> caseName = firstCtor) cases -> true
      | _ -> false
    end userTypes in
    let expectedCases = match matchType.typeDefn with
      | Union cases -> cases
      | _ -> failwith "Match expr must match against a union" in
    let ordered = List.map begin fun (ctorName, _) ->
      let {patArgs; patResult; _} = List.find (fun {patCtor; _} -> ctorName = patCtor) matchCases in
      (patArgs, patResult)
    end expectedCases in
    let branches = List.map begin fun (args, result) ->
      Printf.sprintf "(%s) -> {%s}"
        (String.concat ", " args) (emit userTypes ~in_expr:false result)
    end ordered |> String.concat ", " in
    Printf.sprintf "(%s).match(%s)%s"
      (emit userTypes ~in_expr:true matchValue) branches (if in_expr then "" else ";")
  end
  | RecordMatch { matchRecord; matchArgs; matchBody } -> begin
    let firstField = match matchArgs with
      | (field, _)::_ -> field
      | _ -> failwith "Match expr must contain at least one field" in
    let matchType = List.find begin function
      | {typeDefn=Record fields; _}
        when List.exists (fun (fieldName, _) -> fieldName = firstField) fields -> true
      | _ -> false
    end userTypes in
    let expectedFields = match matchType.typeDefn with
      | Record fields -> fields
      | _ -> failwith "Match expr must match against a record" in
    let ordered = List.map begin fun (fieldName, _) ->
      List.find (fun (key, _) -> key = fieldName) matchArgs
    end expectedFields in
    let branches =
      Printf.sprintf "(%s) -> {%s}"
        (String.concat ", " (List.map (fun (_, v) -> v) ordered))
        (emit userTypes ~in_expr:false matchBody) in
    Printf.sprintf "(%s).match(%s)%s"
      (emit userTypes ~in_expr:true matchRecord) branches (if in_expr then "" else ";")
  end
  | Write { promiseStar; newValue; _ } ->
    Printf.sprintf "%s.fulfill(%s)"
      (emit userTypes ~in_expr:true promiseStar) (emit userTypes ~in_expr:true newValue)
  | Read { promise } ->
    Printf.sprintf "%s.get()"
      (emit userTypes ~in_expr:true promise)
  | Async { application } ->
    Printf.sprintf "$_rt.async(new AsyncTask(() -> %s))"
      (emit userTypes ~in_expr:true application)

let rec emit_params = function
  | [] -> ""
  | [arg, ty] -> Printf.sprintf "%s %s" (java_type ty) (make_id arg)
  | (arg, ty)::args ->
    Printf.sprintf "%s %s, %s"
      (java_type ty) (make_id arg) (emit_params args)

let emit_fun userTypes { funcName; retType; params; expr } =
  Printf.sprintf {|
  public static %s %s(%s) {
    %s
  }
|}
  (java_type retType) funcName (emit_params params) (emit userTypes ~is_final:true expr)

let emit_fields fields =
  let java_field (name, ty) =
    Printf.sprintf "    public %s %s;"
      (java_type ty) name in
  List.map java_field fields |> String.concat "\n"

let emit_constructor className fields =
  let java_param (name, ty) =
    Printf.sprintf "%s %s" (java_type ty) name
  and java_assign (name, _) =
    Printf.sprintf "      this.%s = %s;" name name in
  let params = List.map java_param fields |> String.concat ", "
  and assignments = List.map java_assign fields |> String.concat "\n" in
  Printf.sprintf {|
    public %s(%s) {
%s
    }
|}
    className params assignments

let emit_match_params cases =
  let function_type (name, params) =
    let paramTypes = List.map java_type params in
    let functionArgs = "_R"::paramTypes in
    Printf.sprintf "Function%d<%s> %s"
      (List.length functionArgs - 1)
      (String.concat ", " functionArgs)
      ("_case" ^ name) in
  List.map function_type cases |> String.concat ", "

let emit_record_match fields =
  let field_tys = List.map snd fields in
  Printf.sprintf {|
    public<_R> _R match(%s) {
      return %s.apply(%s);
    }
|}
    (emit_match_params [("Record", field_tys)])
    "_caseRecord"
    (fields |> List.map fst |> String.concat ", ")

let emit_case_classes baseClass cases =
  let java_case_class (name, params) =
    let named_fields =
      List.mapi (fun i ty -> (Printf.sprintf "_%d" i, ty)) params in
    let field_decls = emit_fields named_fields
    and constructor = emit_constructor name named_fields in
    Printf.sprintf {|
  public static class %s extends %s {
    // public members
%s

    // constructor
%s

    // match method
    public<_R> _R match(%s) {
      return %s.apply(%s);
    }
  }
|}
      name baseClass field_decls constructor
      (emit_match_params cases)
      ("_case" ^ name)
      (named_fields |> List.map fst |> String.concat ", ") in
  List.map java_case_class cases |> String.concat "\n"

let emit_type { typeName; typeDefn } = match typeDefn with
  | Record fields ->
    Printf.sprintf {|
  public static class %s extends ThreadLockedObject {
    // public members
%s

    // constructor
%s

    // match method
%s
  }
|}
    typeName (emit_fields fields) (emit_constructor typeName fields)
    (emit_record_match fields)
  | Union cases ->
    Printf.sprintf {|
  // union base class
  public static abstract class %s extends ThreadLockedObject {
    public abstract <_R> _R match(%s);
  }
  // union case classes
  %s
|}
    typeName
    (emit_match_params cases)
    (emit_case_classes typeName cases)

let emit_program { programName; funcs; types } =
  let types =
    List.map emit_type types |> String.concat "\n"
  and functions =
    List.map (emit_fun types) funcs |> String.concat "\n" in
  Printf.sprintf {|
import lang.promises.*;

public class %s {
  private static PromiseRuntime $_rt;

// types
%s

// functions
%s

  private static <T> Unit unsafeWrite(Promise<T> p, T v) {
    p.fulfill(v);
    return Unit.the;
  }

  private static String intToString(Integer i) {
    return i.toString();
  }

  private static String boolToString(Boolean b) {
    return b.toString();
  }

  private static String charAt(String s, Integer i) {
    return ((Character) s.charAt(i)).toString();
  }

  private static String substring(String s, Integer a, Integer b) {
    return s.substring(a, b);
  }

  private static String concat(String a, String b) {
    return a + b;
  }

  private static Integer length(String s) {
    return s.length();
  }

  private static Unit print(String s) {
    System.out.print(s);
    return Unit.the;
  }

  private static Unit println(String s) {
    System.out.println(s);
    return Unit.the;
  }

  private static String readFile(String s) {
    try {
      return new String(
        java.nio.file.Files.readAllBytes(
          java.nio.file.Paths.get(s)));
    } catch (java.io.IOException e) {
      return "";
    }
  }

  public static void main(String[] args) {
    $_rt = new PromiseRuntime();
    main();
  }
}
|}
  programName types functions
