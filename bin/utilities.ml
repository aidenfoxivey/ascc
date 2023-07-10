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
let eat_til_newline (str : string) : string option =
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

let capture_string (str : string) : string * string =
  let pos = String.index str '"' in
  let len = String.length str in
  let captured_string = String.sub str 0 pos in
  let trimmed_string = String.sub str (pos + 1) len in
  captured_string, trimmed_string
;;

(* TODO: rewrite *)
let index_func str f =
  let len = String.length str in
  let rec loop i =
    if i >= len then None else if f (String.get str i) then Some i else loop (i + 1)
  in
  loop 0
;;