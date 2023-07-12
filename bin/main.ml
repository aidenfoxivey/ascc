(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)

open Ascc.Scanner
open Ascc.Utilities

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

(* Dump contents of the file to stdout. *)
let print_file_contents filename =
  let contents = read_file filename in
  print_string contents;
  print_newline ()
;;

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
