open Printf

type macro =
  { name : string
  ; args : string list
  ; body : string
  }

let show_macro m = 
	printf "%s %s" m.name m.body

let preprocess input_file output_file = ()
let add_include_dir dir = ()
let add_macro name value = ()
let remove_macro name = ()

let parse_define str =
  let first_idx = String.index_from str 0 ' ' in
  let second_idx = String.index_from str first_idx ' ' in
  let third_idx = String.index_from str second_idx ' ' in
  let len = String.length str in
  match String.split_on_char ' ' str with
  | define :: name :: body :: rest ->
    Some { name; args = [ "" ]; body }, String.sub str third_idx len
  | _ -> None, ""
;;
