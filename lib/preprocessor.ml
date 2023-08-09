(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)

(*
   * Notes:
   * - strip the existing lines and replace them with the good old \n character
   * - multiline comments must be removed
   *
   *
   *
   *
*)

module DirectiveType = struct
  type t =
    | Define of string * string
    | Include of string
    | Ifdef of string
    | Ifndef of string
    | Endif
    | If of string
    | Elif of string
end

module Directive = struct
  type t =
    { dtype : DirectiveType.t
    ; line : int64
    }
end

let parse_directive (line : string) (line_number : int64) : Directive.t option =
  if String.length line > 0 && line.[0] = '#'
  then (
    let tokens = Str.split (Str.regexp {|[ \t]+|}) line in
    let directive =
      match tokens with
      | "#" :: "define" :: name :: value :: _ | "#define" :: name :: value :: _ ->
        Some (DirectiveType.Define (name, value))
      | "#" :: "include" :: filename :: _ | "#include" :: filename :: _ ->
        Some (DirectiveType.Include filename)
      | "#" :: "ifdef" :: label :: _ | "#ifdef" :: label :: _ ->
        Some (DirectiveType.Ifdef label)
      | "#" :: "ifndef" :: label :: _ | "#ifndef" :: label :: _ ->
        Some (DirectiveType.Ifndef label)
      | "#" :: "endif" :: _ | "#endif" :: _ -> Some DirectiveType.Endif
      | "#" :: "if" :: label :: _ | "#if" :: label :: _ -> Some (DirectiveType.If label)
      | "#" :: "elif" :: label :: _ | "#elif" :: label :: _ ->
        Some (DirectiveType.Elif label)
      | _ -> None
    in
    match directive with
    | Some d -> Some { dtype = d; line = line_number }
    | None -> None)
  else None
;;

let strip_comments src =
  let comments = Str.regexp {|//.*|} in
  Str.global_replace comments src ""
;;

let preprocess src =
  let stripped = strip_comments src in
  stripped
;;
