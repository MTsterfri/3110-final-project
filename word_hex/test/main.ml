open OUnit2
(* open Word_hex *)
(* Uncomment these when implemented *)
(* open Game *)
(* open Board *)
(* open Dictionary *)

(* Game Tests *)

let game_tests = [ "Test 1" >:: fun _ -> assert_equal 1 2]

(* Board Tests *)

let board_tests = []

(* Dictionary Tests *)

let dictionary_tests = []

let suite =
  "test suite for word_hex"
  >::: List.flatten [ game_tests; board_tests; dictionary_tests ]

let () = run_test_tt_main suite