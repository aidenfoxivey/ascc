module DirectiveType : sig
  type t =
    | Define of string * string
    | Include of string
    | Ifdef of string
    | Ifndef of string
    | Endif
    | If of string
    | Elif of string
end

module Directive : sig
  type t =
    { dtype : DirectiveType.t
    ; line : int64
    }
end

val parse_directive : string -> int64 -> Directive.t option
val strip_comments : string -> string
val preprocess : string -> string
