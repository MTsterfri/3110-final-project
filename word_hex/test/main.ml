open OUnit2
open Word_hex

(*****************************************************************)
(* Game Tests *)
(*****************************************************************)

let game_tests = []

(*****************************************************************)
(* Board Tess *)
(*****************************************************************)

let board_tests = []

(*****************************************************************)
(* trieDictionary *)
(*****************************************************************)

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

let of_to_test str i o =
  str >:: fun _ ->
  assert_equal ~printer:(pp_list pp_string) o
    TrieDictionary.(of_list i |> to_list)

(** helper function that takes in a message [str], word to be found [c], input
    [i] as a list of strings to be made into a dicitonary, and an output [o] of
    string option. Tests the contains and find function with these values*)
let contains_test str c i o =
  let trie = TrieDictionary.of_list i in
  "contains " ^ str >:: fun _ -> assert_equal o TrieDictionary.(contains c trie)

let find_test str c i o =
  let trie = TrieDictionary.of_list i in
  "find " ^ str >:: fun _ -> assert_equal o TrieDictionary.(find c trie)

let hwy = [ "hello"; "world"; "yay" ]
let hhw = [ "helio"; "hello"; "world" ]

let dictionary_tests =
  [
    ( "empty dictionary" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) []
        TrieDictionary.(to_list empty) );
    (* insert *)
    ( "insert to empty trie" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "hello" ]
        TrieDictionary.(empty |> insert "hello" |> to_list) );
    ( "insert to new branch" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "hello"; "world" ]
        TrieDictionary.(empty |> insert "hello" |> insert "world" |> to_list) );
    ( "insert shared branch" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "helio"; "hello" ]
        TrieDictionary.(empty |> insert "hello" |> insert "helio" |> to_list) );
    ( "insert new and shared" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) hhw
        TrieDictionary.(
          empty |> insert "hello" |> insert "helio" |> insert "world" |> to_list)
    );
    ( "insert case test" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "hello" ]
        TrieDictionary.(empty |> insert "HeLlO" |> to_list) );
    ( "insert already in trie" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_string) [ "hello" ]
        TrieDictionary.(empty |> insert "hello" |> insert "hello" |> to_list) );
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
    (* contains/find *)
    contains_test "empty trie" "hello" [] false;
    find_test "empty trie" "hello" [] None;
    contains_test "only elem" "hello" [ "hello" ] true;
    find_test "only elem" "hello" [ "hello" ] (Some "hello");
    contains_test "multiple elem" "helio" hhw true;
    find_test "multiple elem" "helio" hhw (Some "helio");
    contains_test "not in multiple elem" "test" hhw false;
    find_test "not in multiple elem" "test" hhw None;
    contains_test "non-lowercase" "hElLo" [ "hElLo" ] true;
    find_test "none-lowercase" "heLLo" [ "HeLlO" ] (Some "hello");
  ]

let suite =
  "test suite for word_hex"
  >::: List.flatten [ game_tests; board_tests; dictionary_tests ]

let () = run_test_tt_main suite
