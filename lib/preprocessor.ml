open Printf
open Utilities

type macro =
  { name : string
  ; args : string list
  ; body : string
  }

let show_macro m = printf "%s %s" m.name m.body
let preprocess input_file output_file = ()
let add_include_dir dir = ()
let add_macro name value = ()
let remove_macro name = ()

(* no support for multiline macros yet *)
let parse_define str =
  match eat_til_first_newline str with
  | Some s ->
    (match String.split_on_char ' ' str with
     | define :: name :: body :: rest -> Some { name; args = []; body }, s
     | _ -> None, str)
  | None -> None, str
;;
