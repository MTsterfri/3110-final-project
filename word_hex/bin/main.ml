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
        \ #reset - resets the current game\n\
        \ #rankings - shows the rankings and the minimum score required to \
         earn each rank";
      print_newline ();
      g
  | "#new" ->
      if not (G.contains_pangram dict (G.get_board g)) then (
        print_endline "Please note that this board does not contain a pangram.";
        print_newline ())
      else ();
      G.build None (choose_shape ()) dict
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
  | "#rankings" -> G.print_rankings g
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
  print_string "Choose a shape for your game board: \n \n   - Hex \n   - TwoHex";
  print_newline ();
  print_string "Board shape: ";
  let input = read_line () in
  let shape_option = MultiBoard.shape_of_string input in
  match shape_option with
  | Some shape -> shape
  | None ->
      print_newline ();
      print_string "Not a valid board shape. Please choose again!";
      print_newline ();
      choose_shape ()

(* Beginning of GUI loops*)
let setup () =
  Raylib.set_config_flags [ Raylib.ConfigFlags.Window_undecorated ];
  Raylib.init_window 800 450 "Word Hex";
  Raylib.set_target_fps 60

(* read-eval-print loop *)
let rec other_loop () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "See terminal to play game" 280 200 20 Color.darkgray;
      end_drawing ();
      (* command line*)
      print_endline "\n\nWelcome to Word Hex!\n";
      print_endline
        "How to play: Construct words using a hexagon that must contain the \
         center letter and has the option of the outer ones";
      let shape = choose_shape () in
      print_endline "Please wait while the game is set up...\n";
      let dict_lst = Array.to_list (Arg.read_arg "data/enable1.txt") in
      let dict = D.of_list dict_lst in
      let game = G.build None shape dict in
      repl game dict

let rec one_loop (text_box : Raylib.Rectangle.t) text (game : G.t) (dict : D.t)
    =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text ("Score : " ^ string_of_int 0) 30 25 20 Color.darkgray;
      draw_text ("Rank : " ^ "Beginner") 30 45 20 Color.darkgray;
      draw_text "Unimplemented One Hex" 280 200 20 Color.darkgray;
      end_drawing ();
      one_loop text_box text game dict

let setup_one_loop () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "Please wait while game is set up..." 280 200 20 Color.darkgray;
      end_drawing ();
      let dict_lst = Array.to_list (Arg.read_arg "data/enable1.txt") in
      let dict = D.of_list dict_lst in
      let game =
        G.build None (Option.get (MultiBoard.shape_of_string "Hex")) dict
      in
      let text_box = Rectangle.create 0. 0. 0. 0. in
      let text =
        let text = Bytes.create 20 in
        Bytes.fill text 0 20 '\000'
      in
      one_loop text_box text game dict

(* completed up to here *)
let rec intro_loop () =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "\n\nWelcome to Word Hex!\n" 300 40 20 Color.darkgray;
      draw_text
        "How to play: Construct words using a hexagon that must contain\n\
        \      the center letter and has the option of the outer ones" 75 190 20
        Color.darkgray;
      draw_text "Select a board shape to begin" 250 290 20 Color.darkgray;
      draw_rectangle 250 350 100 50 Color.blue;
      draw_rectangle 450 350 100 50 Color.blue;
      draw_text "One Hex" 265 370 18 Color.white;
      draw_text "Other \nShape" 470 355 18 Color.white;
      end_drawing ();
      let mousePos = get_mouse_position () in
      let x = Vector2.x mousePos in
      let y = Vector2.y mousePos in
      let isDown = is_mouse_button_pressed MouseButton.Left in
      let on_one = x >= 250. && x <= 350. && y >= 350. && y <= 400. in
      let on_other = x >= 450. && x <= 550. && y >= 350. && y <= 400. in
      if isDown && on_other then other_loop ()
      else if isDown && on_one then setup_one_loop ()
      else intro_loop ()

let () =
  Random.self_init ();
  setup () |> intro_loop
