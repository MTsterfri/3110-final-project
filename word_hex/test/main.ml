open OUnit2
(* open Word_hex *)
(* Uncomment these when implemented *)
(* open Game *)
(* open Board *)
(* open Dictionary *)

(* Game Tests *)

let game_tests = []

(* Board Tests *)

let board_tests = []

(* Dictionary Tests *)

let dictionary_tests = []

let suite =
  "test suite for word_hex"
  >::: List.flatten [ game_tests; board_tests; dictionary_tests ]

let () = run_test_tt_main suite
