open Word_hex
open Board
open Dictionary
open Game
module G = Game (HexBoard)

let command (input : string) (g : G.t) (dict : Dictionary.t) : G.t =
  match input with
  | "#help" ->
      print_newline ();
      print_endline
        "The list of commands are \n\
        \ #help - shows list of commands \n\
        \ #new - starts a new game \n\
        \ #found - lists the words you have already found";
      print_newline ();
      g
  | "#new" -> G.build None dict
  | "#found" ->
      print_newline ();
      print_endline "Words Found So Far:";
      ignore
        (List.map
           (fun x ->
             print_string x;
             print_newline ())
           (G.found g));
      print_newline ();
      g
  | _ ->
      print_endline "Not a Valid Command";
      print_newline ();
      g

(* read-eval-print loop *)
let rec repl (game : G.t) (dict : Dictionary.t) : unit =
  G.print game;
  print_string "Type a word: ";
  let input = read_line () in
  if String.length input > 0 && input.[0] = '#' then
    repl (command input game dict) dict
  else
    match input with
    | "" -> print_endline "bye"
    | _ ->
        print_newline ();
        repl (G.update game input) dict

(* Main Processing *)
let () =
  Random.self_init ();
  print_endline "\n\nWelcome to Word Hex!\n";
  print_endline
    "How to play: Type any word you can construct from what is given.\n";
  print_endline "Press enter to continue";
  let _ = read_line () in
  print_endline "Please wait while the game is set up...\n";
  let dict_lst = Array.to_list (Arg.read_arg "data/enable1.txt") in
  (*TODO: UPDATE DICT_LST*)
  let dict = Dictionary.of_list dict_lst in
  let game = G.build None dict in
  repl game dict
