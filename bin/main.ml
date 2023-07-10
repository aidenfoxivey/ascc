open Utilities

(* THIS IS A FISH
 *       /`·.¸
 *      /¸...¸`:·
 *  ¸.·´  ¸   `·.¸.·´)
 * : © ):´;      ¸  {
 *  `·.¸ `·  ¸.·´\`·¸)
 *      `\\´´\¸.·´
 *)

module Scanner = struct
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
end

let pretty_fmt_token (token : Scanner.token) : string =
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
;;

let read_file filename =
  let ic = open_in filename in
  try
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    flush stdout;
    close_in ic;
    s
  with
  | e ->
    close_in_noerr ic;
    raise e
;;

let eat_string (str : string) : string =
  let pos = String.index_from str 1 '"' in
  String.sub str 1 (pos - 1)
;;

(* Provide the captured string and the number it corresponds to. *)
let eat_number (str : string) : (int * string) option =
  if str = "" then None else Some (4, "fortnite")
;;

(* Scan source code as a string and construct an array of type [ token ]. *)
let rec scan (str : string) : Scanner.token list =
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
      match eat_til_newline str with
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

(*
General form of numbers in C:
=============================
(tested using clang version 14.0.3 on arm64 darwin 23.0)

`4.00000`
-> perfectly tolerable

`4.`
-> perfectly tolerable

`4 .00000`
-> incorrect, there's a space

`4.00 000`
-> incorrect

`- 4.`
-> correct

`-4.`
-> correct

In fact, we can put in arbitrary whitespace between a `-` and the start of a
number.  Even a newline is still valid. Essentially, we want to eat whitespace
after the number until we get to the end.

These all extend to integers as well from what I can tell.

In fact, clang doesn't complain about casting a floating point number into an
integer without any issue at all.
*)

let rec eat_number str : Scanner.token =
  let c = String.get str 0 in
  if String.length str = 0
  then Scanner.ERROR "error"
  else if is_digit c
  then Scanner.AMPERSAND
  else if is_white_space c
  then Scanner.ERROR "df"
  else Scanner.DO
;;

(* Dump contents of the file to stdout. *)
let print_file_contents filename =
  let contents = read_file filename in
  print_string contents;
  print_newline ()
;;

(* Print a string of tokens to stdout. *)
let rec print_tokens (tok : Scanner.token list) =
  match tok with
  | [] -> ()
  | hd :: rest ->
    print_string (pretty_fmt_token hd);
    print_string " ";
    print_tokens rest
;;

(* Run a file. *)
let run filename =
  let contents = read_file filename in
  print_tokens (scan contents);
  print_newline ()
;;

let main _ =
  match Array.length Sys.argv with
  | 2 -> run (Array.get Sys.argv 1)
  | _ ->
    print_endline "usage: ascc <file>";
    exit 64
;;

let () = main ()
