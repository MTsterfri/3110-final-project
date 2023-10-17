open Word_hex
open Board
open Dictionary
open Game

let () =
  print_endline "\n\nWelcome to Word Hex!\n";
  print_endline
    "How to play: Type any word you can construct from what is given.\n";
  print_endline "Press enter to continue";
  let _ = read_line () in
  print_endline "Please wait while the game is set up...\n"
(* let dict_lst = [] in let dict = HashDict.build dict_lst in let game_module =
   Game (HexBoard) (HashDict) in *)
