open OUnit2
open Word_hex
open Game
open Board
open Dictionary

(* Game Tests *)

let game_tests = []

(* Board Tests *)

let board_tests = []

(* Dictionary Tests *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [of_to_test str inp out]*)
let of_to_test str inp out =
  str >:: fun _ ->
  assert_equal ~printer:(pp_list pp_string) out
    HashDict.(inp |> of_list |> to_list)

let hwy = [ "hello"; "world"; "yay" ]

let dictionary_tests =
  [
    of_to_test "of/to_list empty list" [] [];
    of_to_test "of/to_list one-element list" [ "hello" ] [ "hello" ];
    of_to_test "of/to_list multi-element list" hwy hwy;
    of_to_test "of/to_list repeated-element list"
      [ "hello"; "hello"; "world"; "yay"; "yay"; "yay" ]
      hwy;
    of_to_test "of/to_list uppercase list" [ "HELLO"; "WORLD"; "YAY" ] hwy;
    of_to_test "of/to_list mixedcase list" [ "hElLo"; "WORLD"; "yay" ] hwy;
    of_to_test "of/to_list non-alphabetical lst" [ "yay"; "world"; "hello" ] hwy;
  ]

let suite =
  "test suite for word_hex"
  >::: List.flatten [ game_tests; board_tests; dictionary_tests ]

let () = run_test_tt_main suite
