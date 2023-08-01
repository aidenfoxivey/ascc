module Symbol : sig
  type t =
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

  val show_symbol : t -> string
end

module Keyword : sig
  type t =
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

  val to_string : t -> string
end

module Numerical : sig
  type t =
    | Integer_constant of int
    | Floating_constant of float

  val to_string : t -> string
end

module Constant : sig
  type t =
    | Character_constant of char
    | Enumeration_constant of int
    | String_constant of string
    | Numerical_constant of Numerical.t

  val to_string : t -> string
end

val consume_c_string : string -> string * string
val is_valid_first_char_identifier : char -> bool
val is_valid_c_identifier_char : char -> bool
val consume_c_identifier : string -> string * string

module TokenType : sig
  type t =
    | Constant of Constant.t
    | Keyword of Keyword.t
    | Identifier of string
    | Symbol of Symbol.t

  val to_string : t -> string
end

module ScanError : sig
  type t =
    { contents : string
    ; column : int
    ; line : int
    }

  val to_string : t -> string
end

module Token : sig
  type t =
    { ttype : TokenType.t
    ; lexeme : string
    ; line : int
    }

  val to_string : t -> string
end

module Element : sig
  type t =
    | Token of Token.t
    | ScanError of ScanError.t

  val to_string : t -> string
end

val eat_til_first_newline : string -> string option
val print_elements : Element.t list -> unit
val eat_integer : string -> string
val scan : 'a -> 'a
