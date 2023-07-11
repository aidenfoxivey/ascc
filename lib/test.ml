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
  [%expect {||}]
;;

let%expect_test "parse define" =
  match parse_define "#define BAR 897" with
  | Some m, _ -> show_macro m; [%expect {||}]
  | _, _ -> print_string "forn"; [%expect.unreachable]
;;

(* The fail doesn't actually work. - It's not how expect tests are supposed to function. *)
(* let%expect_test "capture string fail" =
  let s1, s2 = capture_string "advanced \"Fortnite is the best game\" I've ever played" in
  printf "%s" s1;
  [%expect.unreachable]
  [@@expect.uncaught_exn 
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Ascc.Utilities.Not_start_with_quote)
  Raised at Ascc__Utilities.capture_string in file "lib/utilities.ml", line 46, characters 2-28
  Called from Ascc__Test.(fun) in file "lib/test.ml", line 37, characters 15-87
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;; *)
