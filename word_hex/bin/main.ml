open Word_hex
open Board
open Multi
open Game
module D = TrieDictionary.Make

(* Scoreboard *)

let rec choose_shape () : MultiBoard.shape =
  print_string
    "Choose a shape for your game board: \n\
    \ \n\
    \   - Hex \n\
    \   - TwoHex \n\
    \   - Triple\n\
    \   - Flower\n\
    \   - Honeycomb";
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

let update (game_name : string) (player : string) (shape : string)
    (char_list : string) (score : int) (rank : string) (found_words : string)
    (hps : int) (scoreboard : (string * Yojson.Basic.t) list) :
    (string * Yojson.Basic.t) list =
  let assoc_lst (str : string) =
    `Assoc
      [
        ("shape", `String shape);
        ("char_list", `String char_list);
        ("score", `Int score);
        ("rank", `String rank);
        ("found_words", `String str);
        ("hps", `Int hps);
      ]
  in
  match List.assoc_opt game_name scoreboard with
  | Some g -> (
      let removed_game_lst = List.remove_assoc game_name scoreboard in
      match g with
      | `Assoc lst -> (
          match List.assoc_opt player lst with
          | Some p ->
              let old_found_words =
                match p with
                | `Assoc p_lst -> List.assoc "found_words" p_lst
                | _ -> failwith "Must be `Assoc"
              in

              let old_found_words_str =
                match old_found_words with
                | `String str -> str
                | _ -> failwith "Must be `String"
              in
              let combined_lst =
                String.split_on_char ' ' (old_found_words_str ^ found_words)
              in
              let no_dup_lst = DList.to_list (DList.of_list combined_lst) in
              let no_dup_str =
                String.uppercase_ascii
                  (List.fold_left
                     (fun acc elem -> elem ^ " " ^ acc)
                     "" no_dup_lst)
              in
              let remove_lst = List.remove_assoc player lst in
              (game_name, `Assoc ((player, assoc_lst no_dup_str) :: remove_lst))
              :: removed_game_lst
          | None ->
              (game_name, `Assoc ((player, assoc_lst found_words) :: lst))
              :: removed_game_lst)
      | _ -> failwith "Must be an `Assoc")
  | None ->
      (game_name, `Assoc [ (player, assoc_lst found_words) ]) :: scoreboard

let scoreboard = ref []
let game_name = ref ""

let player_name = ref ""

and make_json (data : Game.data) (game : string) (player : string) :
    Yojson.Basic.t =
  `Assoc
    [
      ( game,
        `Assoc
          [
            ( player,
              `Assoc
                [
                  ("shape", `String data.shape);
                  ("char_list", `String data.char_list);
                  ("score", `Int data.score);
                  ("rank", `String data.rank);
                  ("found_words", `String data.found_words);
                  ("hps", `Int data.highest_possible_score);
                ] );
          ] );
    ]

and save_json (sb : Yojson.Basic.t) (file : string) =
  let o = open_out file in
  let str = Yojson.Basic.pretty_to_string sb in
  output_string o str;
  close_out o

let set_scoreboard (file : string) : unit =
  let o = open_in file in
  match Yojson.Basic.from_channel o with
  | `Assoc json -> scoreboard := json
  | _ -> failwith "Must be an `Assoc"

let update_scoreboard (file : string) =
  let o = open_in file in
  (match Yojson.Basic.from_channel o with
  | `Assoc json -> scoreboard := json
  | _ -> failwith "Must be an `Assoc");
  close_in o

let get_build_board_data_player_match (game_name : string)
    (player_name : string) (custom_words : string list option) (dict : D.t)
    (p_lst : (string * Yojson.Basic.t) list) : Game.data =
  match List.assoc "shape" p_lst with
  | `String sh -> (
      match List.assoc "char_list" p_lst with
      | `String cl -> (
          match List.assoc "score" p_lst with
          | `Int sc -> (
              match List.assoc "rank" p_lst with
              | `String r -> (
                  match List.assoc "found_words" p_lst with
                  | `String fw -> (
                      match List.assoc "hps" p_lst with
                      | `Int hps ->
                          {
                            shape = sh;
                            char_list = cl;
                            score = sc;
                            rank = r;
                            found_words = fw;
                            highest_possible_score = hps;
                          }
                      | _ -> failwith "Must be `String")
                  | _ -> failwith "Must be `String")
              | _ -> failwith "Must be `String")
          | _ -> failwith "Must be `Int")
      | _ -> failwith "Must be `String")
  | _ -> failwith "Must be `String"

let get_build_board_data_game_match (game_name : string)
    (custom_words : string list option) (dict : D.t)
    (lst : (string * Yojson.Basic.t) list) : Game.data =
  let player, stats = List.hd lst in
  match stats with
  | `Assoc stats_lst -> (
      match List.assoc "shape" stats_lst with
      | `String sh -> (
          match List.assoc "char_list" stats_lst with
          | `String cl -> (
              match List.assoc "hps" stats_lst with
              | `Int hps ->
                  {
                    shape = sh;
                    char_list = cl;
                    score = 0;
                    rank = "Beginner";
                    found_words = "";
                    highest_possible_score = hps;
                  }
              | _ -> failwith "Must be `String")
          | _ -> failwith "Must be `String")
      | _ -> failwith "Must be `String")
  | _ -> failwith "Must be `Assoc"

let build_board (game_name : string) (player_name : string)
    (custom_words : string list option) (dict : D.t) : Game.t =
  match List.assoc_opt game_name !scoreboard with
  | Some g -> (
      match g with
      | `Assoc lst -> (
          match List.assoc_opt player_name lst with
          | Some p -> (
              match p with
              | `Assoc p_lst ->
                  Game.build_of_data
                    (get_build_board_data_player_match game_name player_name
                       custom_words dict p_lst)
                    dict
              | _ -> failwith "Must be a `Assoc")
          | None ->
              Game.build_of_data
                (get_build_board_data_game_match game_name custom_words dict lst)
                dict)
      | _ -> failwith "Must be an `Assoc")
  | None -> Game.build None (choose_shape ()) dict

let rec command (input : string) (g : Game.t) (dict : D.t) : Game.t =
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
         earn each rank\n\
        \ #solution - shows the list of all possible words\n\
        \ #save - saves player data for the game\n\
        \ #quit - exit the game";
      print_newline ();
      g
  | "#new" ->
      get_game_name ();
      get_player_name ();
      let new_game = build_board !game_name !player_name None dict in
      if not (Game.contains_pangram dict (Game.get_board new_game)) then (
        print_newline ();
        print_endline "Please note that this board does not contain a pangram.";
        print_newline ())
      else ();
      new_game
  | "#found" ->
      print_newline ();
      print_endline "Words Found So Far:";
      ignore
        (List.map
           (fun x ->
             print_string x;
             print_newline ())
           (Game.found g));
      print_newline ();
      g
  | "#shuffle" -> Game.shuffle g
  | "#reset" -> Game.reset g
  | "#rankings" -> Game.print_rankings g
  | "#solution" ->
      print_newline ();
      print_string (Game.all_filtered_words_game_str g);
      print_newline ();
      g
  | "#save" ->
      set_scoreboard "data/scoreboard.json";
      let data = Game.get_game_data g in
      save_json
        (`Assoc
          (update !game_name !player_name data.shape data.char_list data.score
             data.rank data.found_words data.highest_possible_score !scoreboard))
        "data/scoreboard.json";

      update_scoreboard "data/scoreboard.json";
      g
  | _ ->
      print_endline "Not a Valid Command";
      print_newline ();
      g

(* read-eval-print loop *)
and repl (game : Game.t) (dict : D.t) : unit =
  Game.print game;
  print_string "Type a word: ";
  let input = read_line () in
  if input = "#quit" then print_endline "Thanks for playing!"
  else if String.length input > 0 && input.[0] = '#' then
    repl (command input game dict) dict
  else
    match input with
    | "" -> repl game dict
    | _ ->
        print_newline ();
        repl (Game.update game input) dict

and get_game_name () : unit =
  print_newline ();
  print_string "Enter a name for your game: ";
  let input = read_line () in
  game_name := input

and get_player_name () : unit =
  print_newline ();
  print_string "Enter your player name: ";
  let input = read_line () in
  player_name := input;
  print_newline ()

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
      get_game_name ();
      get_player_name ();
      print_endline "Please wait while the game is set up...\n";
      let dict_lst = Array.to_list (Arg.read_arg "data/enable1.txt") in
      let dict = D.of_list dict_lst in
      set_scoreboard "data/scoreboard.json";
      let game = build_board !game_name !player_name None dict in
      if not (Game.contains_pangram dict (Game.get_board game)) then (
        print_endline "Please note that this board does not contain a pangram.";
        print_newline ())
      else ();
      repl game dict

(** helper function to collect words *)
let rec pop_keys key text len =
  let ikey = Uchar.to_int key in
  if ikey >= 32 && ikey <= 125 && len < 20 then (
    Bytes.set text len (Char.uppercase_ascii (Uchar.to_char key));
    pop_keys (Raylib.get_char_pressed ()) text (len + 1))
  else len

let rec one_loop (text_box : Raylib.Rectangle.t) text (game : Game.t)
    (len : int) (dict : D.t) =
  let open Raylib in
  match window_should_close () with
  | true -> close_window ()
  | false ->
      let len =
        let len = pop_keys (get_char_pressed ()) text len in
        if is_key_pressed Key.Backspace && len > 0 then (
          Bytes.set text (len - 1) '\000';
          len - 1)
        else len
      in
      let str =
        String.sub (String.lowercase_ascii (String.of_bytes text)) 0 len
      in
      let game_update =
        if is_key_pressed Key.Enter then
          (* commented out for testing in terminal *)
          (* let _ = Game.print game in let _ = print_endline ("\"" ^ str ^
             "\"") in let _ = print_endline (string_of_bool (D.contains str
             dict)) in *)
          Game.update game str
        else game
      in
      let len =
        if
          is_key_pressed Key.Enter
          && Game.get_score game < Game.get_score game_update
        then
          let _ = Bytes.fill text 0 20 '\000' in
          0
        else len
      in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text
        ("Score : " ^ string_of_int (Game.get_score game))
        30 25 20 Color.darkgray;
      draw_text ("Rank : " ^ Game.get_rank_str game) 30 45 20 Color.darkgray;
      draw_text
        (MultiBoard.string_of_board (Game.get_board game))
        320 100 30 Color.black;

      (* begin drawing text *)
      draw_text (Bytes.to_string text)
        ((Rectangle.x text_box |> Int.of_float) + 5)
        ((Rectangle.y text_box |> Int.of_float) + 4)
        40 Color.blue;
      draw_text
        (Printf.sprintf "max chars: %i/20" len)
        330 415 20 Color.darkgray;

      end_drawing ();
      one_loop text_box text game_update len dict

(* crates a one hex gui *)
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
        Game.build None (Option.get (MultiBoard.shape_of_string "Hex")) dict
      in
      let text_box = Rectangle.create 275. 375. 240. 40. in
      let text = Bytes.create 20 in
      Bytes.fill text 0 20 '\000';
      one_loop text_box text game 0 dict

(* home screen for word_hex *)
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
      (* one hex *)
      draw_rectangle 250 350 100 50 Color.blue;
      (* other shape *)
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
