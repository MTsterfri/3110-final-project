open Word_hex
open Board
open Multi
open Game
module G = Game
module D = TrieDictionary.Make

let rec command (input : string) (g : G.t) (dict : D.t) : G.t =
  match input with
  | "#help" ->
      print_newline ();
      print_endline
        "The list of commands are \n\
        \ #help - shows list of commands \n\
        \ #new - starts a new game \n\
        \ #found - lists the words you have already found\n\
        \ #shuffle - shuffles the letters on the game board\n\
        \ #reset - resets the current game";
      print_newline ();
      g
  | "#new" -> G.build None (choose_shape ()) dict
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
  | "#shuffle" -> G.shuffle g
  | "#reset" -> G.reset g
  | _ ->
      print_endline "Not a Valid Command";
      print_newline ();
      g

(* read-eval-print loop *)
and repl (game : G.t) (dict : D.t) : unit =
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

and choose_shape () : MultiBoard.shape =
  print_string
    "Choose a shape for your game board: \n\
    \ \n\
    \   - Hex \n\
    \   - TwoHex \n\
    \   - Triple";
  print_newline ();
  let input = read_line () in
  let shape_option = MultiBoard.shape_of_string input in
  match shape_option with
  | Some shape -> shape
  | None ->
      print_newline ();
      print_string "Not a valid board shape. Please choose again!";
      print_newline ();
      choose_shape ()

(* Main Processing *)
let () =
  Random.self_init ();
  print_endline "\n\nWelcome to Word Hex!\n";
  let shape = choose_shape () in
  print_endline
    "How to play: Type any word you can construct from what is given.\n";
  print_endline "Press enter to continue";
  let _ = read_line () in
  print_endline "Please wait while the game is set up...\n";
  let dict_lst = Array.to_list (Arg.read_arg "data/enable1.txt") in
  (*TODO: UPDATE DICT_LST*)
  let dict = D.of_list dict_lst in
  let game = G.build None shape dict in
  repl game dict
