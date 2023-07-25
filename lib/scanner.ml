(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)

type symbol =
  | Left_brace
  | Right_brace
  | Left_bracket
  | Right_bracket
  | Left_paren
  | Right_paren
  | Semicolon
  | Greater_than
  | Less_than
  | Fore_slash
  | Back_slash
  | Percent
  | Equals
  | Ampersand
  | Asterisk
  | Minus
  | Plus
  | Minus_equal
  | Div_equal
  | Mul_equal
  | And_equals
  | Left_shift
  | Right_shift
  | Left_shift_equal
  | Right_shift_equal
  | Equal_equal
  | Greater_than_equal
  | Less_than_equal
  | Not_equal
  | Plus_plus
  | Minus_minus
  | Plus_equal

let show_symbol (s : symbol) : string =
  match s with
  | Left_brace -> "["
  | Right_brace -> "]"
  | Left_bracket -> "{"
  | Right_bracket -> "}"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Semicolon -> ";"
  | Greater_than -> ">"
  | Less_than -> "<"
  | Fore_slash -> "/"
  | Back_slash -> "\\"
  | Percent -> "%"
  | Equals -> "="
  | Ampersand -> "&"
  | Asterisk -> "*"
  | Minus -> "-"
  | Plus -> "+"
  | Minus_equal -> "-="
  | Div_equal -> "/="
  | Mul_equal -> "*="
  | And_equals -> "&="
  | Left_shift -> "<<"
  | Right_shift -> ">>"
  | Left_shift_equal -> "<<="
  | Right_shift_equal -> ">>="
  | Equal_equal -> "=="
  | Greater_than_equal -> ">="
  | Less_than_equal -> "<="
  | Not_equal -> "!="
  | Plus_plus -> "++"
  | Minus_minus -> "--"
  | Plus_equal -> "+="
;;

type keyword =
  | Auto
  | Break
  | Case
  | Char
  | Const
  | Continue
  | Default
  | Do
  | Double
  | Int
  | Else
  | Long
  | Enum
  | Register
  | Extern
  | Return
  | Float
  | Short
  | For
  | Signed
  | Goto
  | Sizeof
  | If
  | Static
  | Struct
  | Switch
  | Typedef
  | Union
  | Unsigned
  | Void
  | Volatile
  | While

let show_keyword (k : keyword) : string =
  match k with
  | Auto -> "AUTO"
  | Break -> "BREAK"
  | Case -> "CASE"
  | Char -> "CHAR"
  | Const -> "CONST"
  | Continue -> "CONTINUE"
  | Default -> "DEFAULT"
  | Do -> "DO"
  | Double -> "DOUBLE"
  | Int -> "INT"
  | Else -> "ELSE"
  | Long -> "LONG"
  | Enum -> "ENUM"
  | Register -> "REGISTER"
  | Extern -> "EXTERN"
  | Return -> "RETURN"
  | Float -> "FLOAT"
  | Short -> "SHORT"
  | For -> "FOR"
  | Signed -> "SIGNED"
  | Goto -> "GOTO"
  | Sizeof -> "SIZEOF"
  | If -> "IF"
  | Static -> "STATIC"
  | Struct -> "STRUCT"
  | Switch -> "SWITCH"
  | Typedef -> "TYPEDEF"
  | Union -> "UNION"
  | Unsigned -> "UNSIGNED"
  | Void -> "VOID"
  | Volatile -> "VOLATILE"
  | While -> "WHILE"
;;

type constant =
  | Character_constant of char
  | Enumeration_constant of int
  | Floating_constant of float
  | Integer_constant of int
  | String_constant of string

let show_constant (c : constant) : string =
  let open Printf in
  match c with
  | Character_constant c -> sprintf "CHAR(%c)" c
  | Enumeration_constant i -> sprintf "ENUM(%d)" i
  | Floating_constant f -> sprintf "FLOAT(%f)" f
  | Integer_constant i -> sprintf "INT(%d)" i
  | String_constant s -> sprintf "\"%s\"" s
;;

(* Identifiers can just be any string *)
(* type identifier = string *)

(* implement support for escaping characters*)
let consume_c_string (input : string) : string * string =
  let first_char = String.get input 0 in
  if first_char != '"'
  then "", input
  else (
    let buffer = Buffer.create 16 in
    let rec loop i =
      if i < String.length input && input.[i] != '"' && input.[i-1] != '\\'
      then (
        Buffer.add_char buffer input.[i];
        loop (i + 1))
      else i
    in
    let end_pos = loop 0 in
    Buffer.contents buffer, String.sub input end_pos (String.length input - end_pos))
;;

let is_valid_first_char_identifier c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
;;

let is_valid_c_identifier_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
;;

(* Consume a valid C identifier from a string *)
let consume_c_identifier (input : string) : string * string =
  let first_char = String.get input 0 in
  if is_valid_first_char_identifier first_char
  then input, ""
  else (
    let buffer = Buffer.create 16 in
    let rec loop i =
      if i < String.length input && is_valid_c_identifier_char input.[i]
      then (
        Buffer.add_char buffer input.[i];
        loop (i + 1))
      else i
    in
    let end_pos = loop 0 in
    Buffer.contents buffer, String.sub input end_pos (String.length input - end_pos))
;;

type token_type =
  | Constant of constant
  | Keyword of keyword
  | Identifier of string
  | Symbol of symbol

type error =
  { contents : string
  ; column : int
  ; line : int
  }

type token =
  { ttype : token_type
  ; lexeme : string
  ; line : int
  }

type element =
  | Token of token
  | Error of error

(* TODO *)
let show_element element = element

(* Skip until a newline character and return the line without. *)
let eat_til_first_newline (str : string) : string option =
  try
    let pos = String.index str '\n' in
    let len = String.length str in
    Some (String.sub str (pos + 1) (len - pos - 1))
  with
  | Not_found -> None
;;

let rec eat_whitespace (str : string) : string option =
  let len = String.length str in
  if len = 0
  then None
  else (
    let c = String.get str 0 in
    if Utilities.is_whitespace c then eat_whitespace (String.sub str 1 len) else Some str)
;;

(* Print a string of elements to stdout. *)
let print_elements elements =
  List.iter (fun elem -> Printf.printf "%s " (show_element elem)) elements
;;

(* let scan_number str =
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
;; *)

(* let scan_integer str = int_of_string (eat_integer str)
let scan_float str = () *)

(* Scan source code as a string and construct an array of type [ token ]. *)
(* let rec scan str current_line =
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
          let captured_string, rest = eat_string str in
        | '"' ->
          let (captured_string, rest) = eat_string str in
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
;; *)
