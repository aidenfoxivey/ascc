(* Copyright 2023 Aiden Fox Ivey. Subject to the 3-Clause BSD license. *)

open Core
open Utilities
open Preprocessor

let%expect_test "trim one new line" =
  match eat_til_first_newline "\ncool" with
  | Some s ->
    printf "%s" s;
    [%expect {| cool |}]
  | None ->
    print_string "Failed";
    [%expect.unreachable]
;;

let%expect_test "multiple new lines" =
  match eat_til_first_newline "\n\n\n\tcool" with
  | Some s ->
    print_string_as_bytes s;
    [%expect_exact {|0A0A09636F6F6C|}]
  | None ->
    print_string "Failed";
    [%expect.unreachable]
;;

let%expect_test "capture string" =
  let s1, s2 = capture_string "\"Fortnite is the best game\" I've ever played" in
  printf "%s" s1;
  [%expect {|Fortnite is the best game|}]
;;

let%expect_test "capture string 2" =
  let s1, s2 = capture_string "\"Fortnite is the best game\" I've ever played" in
  printf "%s" s2;
  [%expect {|I've ever played|}]
;;

let%expect_test "foo" =
  show_macro { name = "FOO"; args = [ "" ]; body = "5" };
  [%expect {|FOO 5|}]
;;

let%expect_test "parse define" =
  match parse_define "#define BAR 897\n" with
  | Some m, _ ->
    show_macro m;
    [%expect {|BAR 897|}]
  | _, s ->
    print_string s;
    [%expect.unreachable]
;;
