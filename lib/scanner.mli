(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)

exception Not_start_with_quote

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

val pretty_fmt_token : token -> string
val print_tokens : token list -> unit
val eat_string : string -> string * string
val scan_number : string -> token
val eat_integer : string -> string * string
val scan_integer : string -> int
val scan : string -> token list
