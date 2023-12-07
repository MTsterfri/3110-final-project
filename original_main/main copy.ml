(* open Word_hex
open Board
open Game
module G = Game (HexBoard)
module D = TrieDictionary.Make

let command (input : string) (g : G.t) (dict : D.t) : G.t =
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
  | "#shuffle" -> G.shuffle g
  | "#reset" -> G.reset g
  | _ ->
      print_endline "Not a Valid Command";
      print_newline ();
      g

(* read-eval-print loop *)
let rec repl (game : G.t) (dict : D.t) : unit =
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
  let dict = D.of_list dict_lst in
  let game = G.build None dict in
  repl game dict



(* new main *)
open Word_hex
open Board
open Game
module G = Game (HexBoard)
module D = TrieDictionary.Make

let command (input : string) (g : G.t) (dict : D.t) : G.t =
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
  | "#shuffle" -> G.shuffle g
  | "#reset" -> G.reset g
  | _ ->
      print_endline "Not a Valid Command";
      print_newline ();
      g

(* read-eval-print loop *)
let rec loop (game : G.t) (dict : D.t) : unit =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false -> (
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "See terminal to play game" 280 200 20 Color.darkgray;
      end_drawing ();
      G.print game;
      print_string "Type a word: ";
      let input = read_line () in
      if String.length input > 0 && input.[0] = '#' then
        loop (command input game dict) dict
      else
        match input with
        | "" -> print_endline "bye"
        | _ ->
            print_newline ();
            loop (G.update game input) dict)

let setup () =
  Raylib.set_config_flags [ Raylib.ConfigFlags.Window_undecorated ];
  Raylib.init_window 800 450 "Word Hex";
  Raylib.set_target_fps 60

let rec intro_loop (game : G.t) (dict : D.t) : unit =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "\n\nWelcome to Word Hex!\n" 300 50 20 Color.darkgray;
      draw_text
        "How to play: Type any word you can construct from what is given.\n" 75
        200 20 Color.darkgray;
      draw_text "Press enter to continue" 290 300 20 Color.darkgray;
      end_drawing ();
      if is_key_down Key.Enter then loop game dict else intro_loop game dict

(* opening gui that sets up the game and dictionary *)
let rec opening () : unit =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "Please wait while game is set up..." 280 200 20 Color.darkgray;
      end_drawing ();
      (* let _ = read_line () in print_endline "Please wait while the game is
         set up...\n"; *)
      let dict =
        let dict_lst = Array.to_list (Arg.read_arg "data/enable1.txt") in
        D.of_list dict_lst
      in
      let game = G.build None dict in
      intro_loop game dict

let () =
  Random.self_init ();
  setup () |> opening

(* let command (input : string) (g : G.t) (dict : D.t) : G.t = match input with
   | "#help" -> print_newline (); print_endline "The list of commands are \n\ \
   #help - shows list of commands \n\ \ #new - starts a new game \n\ \ #found -
   lists the words you have already found\n\ \ #shuffle - shuffles the letters
   on the game board\n\ \ #reset - resets the current game"; print_newline (); g
   | "#new" -> G.build None dict | "#found" -> print_newline (); print_endline
   "Words Found So Far:"; ignore (List.map (fun x -> print_string x;
   print_newline ()) (G.found g)); print_newline (); g | "#shuffle" -> G.shuffle
   g | "#reset" -> G.reset g | _ -> print_endline "Not a Valid Command";
   print_newline (); g

   (* read-eval-print loop *) let rec repl (game : G.t) (dict : D.t) : unit =
   G.print game; print_string "Type a word: "; let input = read_line () in if
   String.length input > 0 && input.[0] = '#' then repl (command input game
   dict) dict else match input with | "" -> print_endline "bye" | _ ->
   print_newline (); repl (G.update game input) dict

   (* Main Processing *) let () = Random.self_init (); print_endline
   "\n\nWelcome to Word Hex!\n"; print_endline "How to play: Type any word you
   can construct from what is given.\n"; print_endline "Press enter to
   continue"; let _ = read_line () in print_endline "Please wait while the game
   is set up...\n"; let dict_lst = Array.to_list (Arg.read_arg
   "data/enable1.txt") in (*TODO: UPDATE DICT_LST*) let dict = D.of_list
   dict_lst in let game = G.build None dict in repl game dict *) *)
