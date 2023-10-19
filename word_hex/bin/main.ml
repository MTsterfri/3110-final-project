open Word_hex
open Board
open Dictionary
open Game
module G = Game (HexBoard)

(* read-eval-print loop *)
let rec repl (game : G.t) : unit =
  G.print game;
  print_string "Type a word: ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      print_newline ();
      repl (G.update game input)

(* Main Processing *)
let () =
  Random.self_init ();
  print_endline "\n\nWelcome to Word Hex!\n";
  print_endline
    "How to play: Type any word you can construct from what is given.\n";
  print_endline "Press enter to continue";
  let _ = read_line () in
  print_endline "Please wait while the game is set up...\n";
  let dict_lst = [] in
  (*TODO: UPDATE DICT_LST*)
  let dict = Dictionary.of_list dict_lst in
  let game = G.build None dict in
  repl game
