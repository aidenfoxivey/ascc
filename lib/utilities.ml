(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)


let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

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
    if is_whitespace c then eat_whitespace (String.sub str 1 len) else Some str)
;;



let capture_number str = "fort", 4.0

let print_string_as_bytes str =
  String.iter (fun c -> Printf.printf "%02X" (Char.code c)) str
;;
