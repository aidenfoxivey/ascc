type macro =
  { name : string
  ; args : string list
  ; body : string
  }

val show_macro : macro -> unit

(** [preprocess input_file output_file] reads from [input_file] and writes the 
macro definitions to [output_file] *)
val preprocess : string -> string -> unit

(** [add_include_dir dir] adds [dir] to the list of directories to search *)
val add_include_dir : string -> unit

(** [add_define name value] adds [name] to the macro list *)
val add_macro : string -> string -> unit

(** [remove_define name] removes [name] from the macro list *)
val remove_macro : string -> unit

(* val eat_define : string -> string * string *)

val parse_define : string -> macro option * string
