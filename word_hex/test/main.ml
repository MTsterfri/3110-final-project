open OUnit2
open Word_hex
open Board
open Multi
open Game

(*****************************************************************)
(* Game Tests *)
(*****************************************************************)

let make_test (name : string) actual expected =
  name >:: fun _ -> assert_equal actual expected

module D = TrieDictionary.Make
module DList = ListDictionary.Make

let alphabet_dict = D.of_list [ "abcdefg"; "hi"; "hello"; "fg" ]
let alphabet_dict_no_pangram = D.of_list [ "abcde"; "hi"; "hello"; "fg"; "abb" ]

let alphabet_board =
  MultiBoard.board_of_letters
    (Option.get (MultiBoard.shape_of_string "Hex"))
    [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G' ]

let alphabet_game =
  Game.build_of_board None
    (Option.get (MultiBoard.shape_of_string "Hex"))
    alphabet_dict alphabet_board

let highest_score_dict =
  D.of_list [ "conflict"; "cliff"; "fill"; "fiction"; "inn" ]

let highest_score_board =
  MultiBoard.board_of_letters
    (Option.get (MultiBoard.shape_of_string "Hex"))
    [ 'I'; 'L'; 'O'; 'F'; 'T'; 'N'; 'C' ]

let highest_score_game =
  Game.build_of_board None
    (Option.get (MultiBoard.shape_of_string "Hex"))
    highest_score_dict highest_score_board

let game_tests =
  [
    make_test "contains_pangram(); game contains pangram"
      (Game.contains_pangram alphabet_dict alphabet_board)
      true;
    make_test "contains_pangram(); game doesn't contain pangram"
      (Game.contains_pangram alphabet_dict_no_pangram alphabet_board)
      false;
    make_test
      "all_filtered_words_game(); only includes words with the center letter \
       and contain at least four letters"
      (DList.to_list
         (Game.all_filtered_words_board alphabet_dict alphabet_board))
      [ "abcdefg" ];
    make_test "get_highest_possible_score(); includes a pangram"
      (Game.get_highest_possible_score highest_score_game)
      28;
    make_test "score_calc_board(); score of the pangram"
      (Game.score_calc_board "conflict" highest_score_board)
      15;
    make_test
      "calculate_rank_str(); testing truncation with score and rank \
       calculations"
      (Game.calculate_rank_str 4 28)
      "Solid";
  ]

(*****************************************************************)
(* Board Tests *)
(*****************************************************************)

module BoardTest (B : BoardType) = struct
  let tests = []
end

module HexTest = BoardTest (HexBoard)
module TwoHexTest = BoardTest (TwoHex)
module TripleTest = BoardTest (TripleBoard)
module FlowerTest = BoardTest (FlowerBoard)
module HoneycombTest = BoardTest (Honeycomb)

let board_tests =
  HexTest.tests @ TwoHexTest.tests @ TripleTest.tests @ FlowerTest.tests
  @ HoneycombTest.tests

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

module DictTest (D : Dictionary.Dictionary) = struct
  (** [insert_test str o i] helper function that takes in a test name [str],
      input [i] of a dictionary, and the expected output list [o]. Tests the
      insert function*)
  let insert_test str o i =
    str >:: fun _ ->
    assert_equal ~printer:(pp_list pp_string) o D.(i |> to_list)

  (** helper function that take is a test name [str], input list [i], and
      expected output [o]. Tests both the to_list and of_list functions.*)
  let of_to_test str i o =
    str >:: fun _ ->
    assert_equal ~printer:(pp_list pp_string) o D.(of_list i |> to_list)

  (** helper function that takes in a message [str], word to be found [c], input
      [i] as a list of strings to be made into a dicitonary, and an output [o]
      of string option. Tests the contains function with these values*)
  let contains_test str c i o =
    let dict = D.of_list i in
    "contains " ^ str >:: fun _ -> assert_equal o D.(contains c dict)

  (** helper function that takes in a test name [str], word to be found [c],
      input [i] as a list of strings to be made into a dicitonary, and an output
      [o] of string option. Tests the find function with these values*)
  let find_test str c i o =
    let dict = D.of_list i in
    "find " ^ str >:: fun _ -> assert_equal o D.(find c dict)

  (** [remove_test str c i o] takes in a test name [str], word to be removed
      [c], input [i] as a list of strings to be made into a dicitonary, and an
      output [o] of a dictionary as a list. Tests the find remove with these
      values*)
  let remove_test str c i o =
    "remove " ^ str >:: fun _ ->
    assert_equal o D.(of_list i |> remove c |> to_list)

  (** helper function that takes in a test name [str], char list [cl], string
      list input [i] and expected output from of_char_list [o]*)
  let char_test str cl i o =
    "char_test " ^ str >:: fun _ ->
    assert_equal ~printer:(pp_list pp_string) o D.(of_list i |> of_char_list cl)

  let hwy = [ "hello"; "world"; "yay" ]
  let hhw = [ "helio"; "hello"; "world" ]

  let tests =
    [
      ( "empty dictionary" >:: fun _ ->
        assert_equal ~printer:(pp_list pp_string) [] D.(to_list empty) );
      (* insert *)
      insert_test "insert to empty dict" [ "hello" ] D.(empty |> insert "hello");
      insert_test "insert to new branch" [ "hello"; "world" ]
        D.(empty |> insert "hello" |> insert "world");
      insert_test "insert shared branch" [ "helio"; "hello" ]
        D.(empty |> insert "hello" |> insert "helio");
      insert_test "insert new and shared" hhw
        D.(empty |> insert "hello" |> insert "helio" |> insert "world");
      insert_test "insert case test" [ "hello" ] D.(empty |> insert "HeLlO");
      insert_test "insert already in dict" [ "hello" ]
        D.(empty |> insert "hello" |> insert "hello");
      (* of_list & to_list *)
      of_to_test "of/to_list empty list" [] [];
      of_to_test "of/to_list one-element list" [ "hello" ] [ "hello" ];
      of_to_test "of/to_list multi-element list" hwy hwy;
      of_to_test "of/to_list repeated-element list"
        [ "hello"; "hello"; "world"; "yay"; "yay"; "yay" ]
        hwy;
      of_to_test "of/to_list uppercase list" [ "HELLO"; "WORLD"; "YAY" ] hwy;
      of_to_test "of/to_list mixedcase list" [ "hElLo"; "WORLD"; "yay" ] hwy;
      of_to_test "of/to_list non-alphabetical lst"
        [ "yay"; "world"; "hello" ]
        hwy;
      (* contains/find *)
      contains_test "empty dict" "hello" [] false;
      find_test "empty dict" "hello" [] None;
      contains_test "only elem" "hello" [ "hello" ] true;
      find_test "only elem" "hello" [ "hello" ] (Some "hello");
      contains_test "multiple elem" "helio" hhw true;
      find_test "multiple elem" "helio" hhw (Some "helio");
      contains_test "not in multiple elem" "test" hhw false;
      find_test "not in multiple elem" "test" hhw None;
      contains_test "non-lowercase" "hElLo" [ "hElLo" ] true;
      find_test "none-lowercase" "heLLo" [ "HeLlO" ] (Some "hello");
      (* remove *)
      remove_test "empty" "hello" [] [];
      remove_test "one element dict" "hello" [ "hello" ] [];
      remove_test "not in one element dict" "goodbye" [ "hello" ] [ "hello" ];
      remove_test "multiword dict" "helio" hhw [ "hello"; "world" ];
      remove_test "not in multiword dict" "goodbye" hhw hhw;
      remove_test "middle word" "hangout"
        [ "hang"; "hangout"; "hangouts" ]
        [ "hang"; "hangouts" ];
      remove_test "uppercase" "HeLlO" [ "hello" ] [];
      (*of_char_list*)
      char_test "empty" [ 'a' ] [] [];
      char_test "one element include" [ 'h' ] [ "h" ] [ "h" ];
      char_test "one element not included" [ 'x' ] [ "h" ] [];
      char_test "empty char list" [] [ "h" ] [];
      char_test "filter top" [ 'h'; 'e' ] [ "h"; "g"; "e" ] [ "e"; "h" ];
      char_test "full word" [ 'h'; 'e'; 'l'; 'o' ] [ "hello" ] [ "hello" ];
      char_test "one path"
        [ 'h'; 'e'; 'l'; 'i'; 'o' ]
        [ "hello"; "helio"; "hellos"; "hell"; "howdy" ]
        [ "helio"; "hell"; "hello" ];
      char_test "multiple paths"
        [ 'h'; 'e'; 'l'; 'i'; 'o'; 'w'; 'r'; 'd' ]
        [
          "hello"; "helio"; "hellos"; "hell"; "howdy"; "world"; "word"; "waddle";
        ]
        [ "helio"; "hell"; "hello"; "word"; "world" ];
    ]
end

module ListDictTests = DictTest (ListDictionary.Make)
module TrieDictTests = DictTest (TrieDictionary.Make)

let suite =
  "test suite for word_hex"
  >::: List.flatten
         [ game_tests; board_tests; ListDictTests.tests; TrieDictTests.tests ]

let () = run_test_tt_main suite
