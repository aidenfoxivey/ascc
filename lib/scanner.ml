(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)

open Utilities

type token =
  (* single char tokens *)
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | LEFT_PAREN
  | RIGHT_PAREN
  | SEMICOLON
  | GREATER_THAN
  | LESS_THAN
  | FORE_SLASH
  | BACK_SLASH
  | PERCENT
  | EQUALS
  | AMPERSAND
  | ASTERISK
  | MINUS
  | PLUS
  (* double or triple char tokens *)
  | MINUS_EQUAL
  | DIV_EQUAL
  | MUL_EQUAL
  | AND_EQUALS
  | LEFT_SHIFT
  | RIGHT_SHIFT
  | LEFT_SHIFT_EQUAL
  | RIGHT_SHIFT_EQUAL
  | EQUAL_EQUAL
  | GREATER_THAN_EQUAL
  | LESS_THAN_EQUAL
  | NOT_EQUAL
  | PLUS_PLUS
  | MINUS_MINUS
  | PLUS_EQUAL
  (* literals *)
  | STRING_LITERAL of string
  | INTEGER_LITERAL of int
  | FLOAT_LITERAL of float
  | IDENTIFIER of string
  | ERROR of string
  (* keywords *)
  | AUTO
  | BREAK
  | CASE
  | CHAR
  | CONST
  | CONTINUE
  | DEFAULT
  | DO
  | DOUBLE
  | ELSE
  | ENUM
  | EXTERN
  | FLOAT
  | FOR
  | GOTO
  | IF
  | INLINE
  | INT
  | LONG
  | REGISTER
  | RESTRICT
  | RETURN
  | SHORT
  | SIGNED
  | SIZEOF
  | STATIC
  | STRUCT
  | SWITCH
  | TYPEDEF
  | UNION
  | UNSIGNED
  | VOID
  | VOLATILE
  | WHILE
  | EOF

let pretty_fmt_token (token : token) : string =
  let open Scanner in
  match token with
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | LEFT_BRACKET -> "LEFT_BRACKET"
  | RIGHT_BRACKET -> "RIGHT_BRACKET"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | SEMICOLON -> "SEMICOLON"
  | GREATER_THAN -> "GREATER_THAN"
  | LESS_THAN -> "LESS_THAN"
  | FORE_SLASH -> "FORE_SLASH"
  | BACK_SLASH -> "BACK_SLASH"
  | PERCENT -> "PERCENT"
  | EQUALS -> "EQUALS"
  | AMPERSAND -> "AMPERSAND"
  | ASTERISK -> "ASTERISK"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  (* double or triple char tokens *)
  | MINUS_EQUAL -> "MINUS_EQUAL"
  | DIV_EQUAL -> "DIV_EQUAL"
  | MUL_EQUAL -> "MUL_EQUAL"
  | AND_EQUALS -> "AND_EQUALS"
  | LEFT_SHIFT -> "LEFT_SHIFT"
  | RIGHT_SHIFT -> "RIGHT_SHIFT"
  | LEFT_SHIFT_EQUAL -> "LEFT_SHIFT_EQUAL"
  | RIGHT_SHIFT_EQUAL -> "RIGHT_SHIFT_EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER_THAN_EQUAL -> "GREATER_THAN_EQUAL"
  | LESS_THAN_EQUAL -> "LESS_THAN_EQUAL"
  | NOT_EQUAL -> "NOT_EQUAL"
  | PLUS_PLUS -> "PLUS_PLUS"
  | MINUS_MINUS -> "MINUS_MINUS"
  | PLUS_EQUAL -> "PLUS_EQUAL"
  (* literals *)
  | STRING_LITERAL x -> Printf.sprintf "STRING_LITERAL(%s)" x
  | INTEGER_LITERAL x -> Printf.sprintf "INTEGER_LITERAL(%d)" x
  | FLOAT_LITERAL x -> Printf.sprintf "FLOAT_LITERAL(%f)" x
  | IDENTIFIER x -> Printf.sprintf "IDENTIFIER(%s)" x
  | ERROR x -> Printf.sprintf "ERROR: %s" x
  (* keywords *)
  | AUTO -> "AUTO"
  | BREAK -> "BREAK"
  | CASE -> "CASE"
  | CHAR -> "CHAR"
  | CONST -> "CONST"
  | CONTINUE -> "CONTINUE"
  | DEFAULT -> "DEFAULT"
  | DO -> "DO"
  | DOUBLE -> "DOUBLE"
  | ELSE -> "ELSE"
  | ENUM -> "ENUM"
  | EXTERN -> "EXTERN"
  | FLOAT -> "FLOAT"
  | FOR -> "FOR"
  | GOTO -> "GOTO"
  | IF -> "IF"
  | INLINE -> "INLINE"
  | INT -> "INT"
  | LONG -> "LONG"
  | REGISTER -> "REGISTER"
  | RESTRICT -> "RESTRICT"
  | RETURN -> "RETURN"
  | SHORT -> "SHORT"
  | SIGNED -> "SIGNED"
  | SIZEOF -> "SIZEOF"
  | STATIC -> "STATIC"
  | STRUCT -> "STRUCT"
  | SWITCH -> "SWITCH"
  | TYPEDEF -> "TYPEDEF"
  | UNION -> "UNION"
  | UNSIGNED -> "UNSIGNED"
  | VOID -> "VOID"
  | VOLATILE -> "VOLATILE"
  | WHILE -> "WHILE"
  | EOF -> "EOF"
;;

exception Not_start_with_quote

(* Print a string of tokens to stdout. *)
let rec print_tokens tok =
  match tok with
  | [] -> ()
  | hd :: rest ->
    print_string (pretty_fmt_token hd);
    print_string " ";
    print_tokens rest
;;

let eat_string str =
  let first_char = String.get str 0 in
  if first_char = '"'
  then (
    let pos = String.index_from str 1 '"' in
    let len = String.length str in
    let captured_string = String.sub str 1 (pos - 1) in
    let trimmed_string = String.sub str (pos + 1) (len - pos - 1) in
    captured_string, trimmed_string)
  else raise Not_start_with_quote
;;

let scan_number str =
  let c = String.get str 0 in
  if String.length str = 0
  then ERROR "error"
  else if is_digit c
  then AMPERSAND
  else if is_whitespace c
  then ERROR "df"
  else DO
;;

let rec eat_integer str =
  let c = String.get str 0 in
  if is_digit c
  then String.make 1 c ^ eat_integer (String.sub str 1 (String.length str - 1))
  else ""
;;

let scan_integer str = int_of_string (eat_integer str)
let scan_float str = ()

(* Scan source code as a string and construct an array of type [ token ]. *)
let rec scan str =
  let open Scanner in
  let len = String.length str in
  if len = 0
  then []
  else (
    let c = String.get str 0 in
    if is_whitespace c
    then scan (String.sub str 1 (len - 1))
    else if c = '/' && len > 1 && String.get str 1 = '/'
    then (
      match eat_til_first_newline str with
      | Some x -> scan x
      | None -> [])
    else (
      let status, skip =
        match c with
        | '{' -> [ LEFT_BRACE ], 1
        | '}' -> [ RIGHT_BRACE ], 1
        | '(' -> [ LEFT_PAREN ], 1
        | ')' -> [ RIGHT_PAREN ], 1
        | '[' -> [ LEFT_BRACKET ], 1
        | ']' -> [ RIGHT_BRACKET ], 1
        | '\\' -> [ BACK_SLASH ], 1
        | ';' -> [ SEMICOLON ], 1
        | '*' ->
          let next_c = String.get str 1 in
          if next_c = '=' then [ MUL_EQUAL ], 2 else [ ASTERISK ], 1
        | '&' ->
          let next_c = String.get str 1 in
          if next_c = '=' then [ AND_EQUALS ], 2 else [ AMPERSAND ], 1
        | '/' ->
          let next_c = String.get str 1 in
          if next_c = '=' then [ DIV_EQUAL ], 2 else [ FORE_SLASH ], 1
        | '=' ->
          let next_c = String.get str 1 in
          if next_c = '=' then [ EQUAL_EQUAL ], 2 else [ EQUALS ], 1
        | '-' ->
          let next_c = String.get str 1 in
          if next_c = '=' then [ MINUS_EQUAL ], 2 else [ MINUS ], 1
        | '+' ->
          let next_c = String.get str 1 in
          if next_c = '=' then [ PLUS_EQUAL ], 2 else [ PLUS ], 1
        | '>' ->
          let next_c = String.get str 1 in
          let next_next_c = String.get str 2 in
          if next_c = '='
          then [ GREATER_THAN_EQUAL ], 2
          else if next_c = '>' && next_next_c = '='
          then [ LEFT_SHIFT_EQUAL ], 3
          else if next_c = '>'
          then [ LEFT_SHIFT ], 2
          else [ GREATER_THAN ], 1
        | '<' ->
          let next_c = String.get str 1 in
          let next_next_c = String.get str 2 in
          if next_c = '='
          then [ LESS_THAN_EQUAL ], 2
          else if next_c = '<' && next_next_c = '='
          then [ RIGHT_SHIFT_EQUAL ], 3
          else if next_c = '<'
          then [ RIGHT_SHIFT ], 2
          else [ LESS_THAN ], 1
        | '"' ->
          let captured_string = capture_string str in
          let str_len = String.length captured_string + 3 in
          [ STRING_LITERAL captured_string ], str_len - 1
          (* more complicated for decimal finding *)
        | c when is_digit c ->
          let captured_num = capture_num str in
          let str_len = String.length captured_num in
          [ INTEGER_LITERAL 2 ], str_len
        | c when is_alpha c -> [ AUTO ], 1
        | _ -> [], 1
      in
      status @ scan (String.sub str skip (len - skip))))
;;