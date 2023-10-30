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

let pp_option opt = Option.get opt

(** [of_to_test str inp out] makes a test case named [str] checking if [inp]
    matches a list of strings [out] that is made into and pulled from a
    dictionary*)
let of_to_test str inp out =
  str >:: fun _ ->
  assert_equal ~printer:(pp_list pp_string) out
    Dictionary.(inp |> of_list |> to_list)

let test_dict = Dictionary.of_list []

(** [insert_test str inp out] testing whether insert runs and contains has the
    inserted [inp], both functions return [out]*)
let insert_test str inp out =
  "insert test"
  >::: [
         ( str >:: fun _ ->
           assert_equal out
             (Dictionary.insert_opt inp test_dict)
             ~printer:pp_option );
         ( "contains " ^ str >:: fun _ ->
           assert_equal out
             (Dictionary.contains_opt inp test_dict)
             ~printer:pp_option );
       ]

let hwy = [ "hello"; "world"; "yay" ]

let dictionary_tests =
  [
    (* of_list & to_list *)
    of_to_test "of/to_list empty list" [] [];
    of_to_test "of/to_list one-element list" [ "hello" ] [ "hello" ];
    of_to_test "of/to_list multi-element list" hwy hwy;
    of_to_test "of/to_list repeated-element list"
      [ "hello"; "hello"; "world"; "yay"; "yay"; "yay" ]
      hwy;
    of_to_test "of/to_list uppercase list" [ "HELLO"; "WORLD"; "YAY" ] hwy;
    of_to_test "of/to_list mixedcase list" [ "hElLo"; "WORLD"; "yay" ] hwy;
    of_to_test "of/to_list non-alphabetical lst" [ "yay"; "world"; "hello" ] hwy;
    (* contains_opt *)
    ( "contains empty dict" >:: fun _ ->
      assert_equal None Dictionary.([] |> of_list |> contains_opt "hello") );
    ( "contains only elem" >:: fun _ ->
      assert_equal (Some "hello")
        Dictionary.([ "hello" ] |> of_list |> contains_opt "hello") );
    ( "contains multiple elem" >:: fun _ ->
      assert_equal (Some "world")
        Dictionary.(hwy |> of_list |> contains_opt "world") );
    ( "contains not in multiple elem" >:: fun _ ->
      assert_equal None Dictionary.(hwy |> of_list |> contains_opt "test") );
    ( "contains non-lower case" >:: fun _ ->
      assert_equal (Some "hello")
        Dictionary.([ "HeLlO" ] |> of_list |> contains_opt "HELLO") );
    (* instrt_opt *)
    insert_test "insert to empty list" "hello" (Some "hello");
    insert_test "insert to filled list" "world" (Some "world");
    ( "test" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "test" ]
        Dictionary.(test_dict |> to_list) );
  ]

let suite =
  "test suite for word_hex"
  >::: List.flatten [ game_tests; board_tests; dictionary_tests ]

let () = run_test_tt_main suite
