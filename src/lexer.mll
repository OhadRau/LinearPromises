{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let int = '-'? ['0'-'9'] ['0'-'9']*

let bool = "true" | "false"

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule inlineComment = parse
  | newline
    { next_line lexbuf; read lexbuf }
  | _
    { inlineComment lexbuf }

and multiLineComment = parse
  | newline
    { next_line lexbuf; multiLineComment lexbuf }
  | "*)"
    { read lexbuf }
  | _
    { multiLineComment lexbuf }

and read = parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }

  | bool as b { BOOL (bool_of_string b) }
  | int as i  { INT (int_of_string i) }

  | "func"    { FUNC }
  | "union"   { UNION }
  | "record"  { RECORD }
  | "begin"   { BEGIN }
  | "end"     { END }
  | "let"     { LET }
  | "in"      { IN }
  | "if"      { IF }
  | "match"   { MATCH }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "for"     { FOR }
  | "to"      { TO }
  | "while"   { WHILE }
  | "promise" { PROMISE }
  | "async"   { ASYNC }

  | "Unit"    { TYPE_UNIT }
  | "Bool"    { TYPE_BOOL }
  | "Int"     { TYPE_INT }
  | "Promise" { TYPE_PROMISE }

  | id as id  { IDENT id }

  | '='       { EQUAL }
  | "=="      { IS_EQUAL }
  | "!="      { NOT_EQUAL }
  | '<'       { LESS_THAN }
  | "<="      { LESS_THAN_EQUAL }
  | '>'       { GREATER_THAN }
  | ">="      { GREATER_THAN_EQUAL }
  | '.'       { DOT }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | '['       { LEFT_BRACKET }
  | ']'       { RIGHT_BRACKET }
  | '{'       { LEFT_BRACE }
  | '}'       { RIGHT_BRACE }
  | ','       { COMMA }
  | '*'       { STAR }
  | ':'       { COLON }
  | ';'       { SEMICOLON }
  | "->"      { RIGHT_ARROW }
  | "<-"      { LEFT_ARROW }
  | "<~"      { LEFT_TILDE_ARROW }
  | '?'       { QUESTION }

  | "--"      { inlineComment lexbuf }
  | "(*"      { multiLineComment lexbuf }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }
