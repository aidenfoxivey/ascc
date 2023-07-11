val is_alpha : char -> bool
val is_digit : char -> bool
val is_whitespace : char -> bool
val eat_til_first_newline : string -> string option
val eat_whitespace : string -> string option
val capture_string : string -> string * string
val capture_number : string -> string * float
val eat_integer : string -> string
val scan_integer : string -> int
val print_string_as_bytes : string -> unit
