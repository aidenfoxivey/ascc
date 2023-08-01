(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let print_string_as_bytes str =
  String.iter (fun c -> Printf.printf "%02x" (Char.code c)) str
;;

let unimplemented _ = failwith "unimplemented"
