open Board
open Multi
module D = TrieDictionary.Make
module DList = ListDictionary.Make

(** The signature of a word_hex game. *)
module type GameType = sig
  type t

  type rank =
    | QueenBee of int
    | Genius of int
    | Amazing of int
    | Great of int
    | Nice of int
    | Solid of int
    | Good of int
    | MovingUp of int
    | GoodStart of int
    | Beginner of int

  val build : string list option -> MultiBoard.shape -> D.t -> t

  val build_of_board :
    string list option -> MultiBoard.shape -> D.t -> MultiBoard.t -> t

  val get_board : t -> MultiBoard.t
  val get_dict : t -> D.t
  val get_score : t -> int
  val get_rank_str : t -> string
  val update : t -> string -> t
  val found : t -> string list
  val shuffle : t -> t
  val reset : t -> t

  val best_board :
    int -> MultiBoard.shape -> string list option -> D.t -> MultiBoard.t

  val contains_pangram : D.t -> MultiBoard.t -> bool
  val all_filtered_words_board : D.t -> MultiBoard.t -> DList.t
  val all_filtered_words_game_str : t -> string
  val get_highest_possible_score : t -> int
  val score_calc_board : string -> MultiBoard.t -> int
  val calculate_rank_str : int -> int -> string
  val print_rankings : t -> t
  val print : t -> unit
end

module Game : GameType = struct
  type rank =
    | QueenBee of int
    | Genius of int
    | Amazing of int
    | Great of int
    | Nice of int
    | Solid of int
    | Good of int
    | MovingUp of int
    | GoodStart of int
    | Beginner of int

  type t = {
    score : int;
    rank : rank;
    found_words : string list;
    board : MultiBoard.t;
    dictionary : D.t;
    message : string;
    highest_possible_score : int;
  }

  let rec build (words : string list option) (shape : MultiBoard.shape)
      (dict : D.t) : t =
    let chosen_board = best_board 500 shape None dict in
    let hs = highest_possible_board_score chosen_board dict in
    {
      score = 0;
      rank = Beginner 0;
      found_words = [];
      board = chosen_board;
      dictionary = dict;
      message = "";
      highest_possible_score = hs;
    }

  and build_of_board (words : string list option) (shape : MultiBoard.shape)
      (dict : D.t) (board : MultiBoard.t) : t =
    let hs = highest_possible_board_score board dict in
    {
      score = 0;
      rank = Beginner 0;
      found_words = [];
      board;
      dictionary = dict;
      message = "";
      highest_possible_score = hs;
    }

  and get_board (game : t) : MultiBoard.t = game.board
  and get_dict (game : t) : D.t = game.dictionary
  and get_score (game : t) : int = game.score
  and get_rank_str (game : t) : string = rank_to_string game.rank

  and calculate_ranks (game : t) : rank list =
    let hs = float_of_int game.highest_possible_score in
    [
      Beginner (int_of_float (0.0 *. hs));
      GoodStart (int_of_float (0.02 *. hs));
      MovingUp (int_of_float (0.05 *. hs));
      Good (int_of_float (0.08 *. hs));
      Solid (int_of_float (0.15 *. hs));
      Nice (int_of_float (0.25 *. hs));
      Great (int_of_float (0.4 *. hs));
      Amazing (int_of_float (0.5 *. hs));
      Genius (int_of_float (0.7 *. hs));
      QueenBee (int_of_float hs);
    ]

  and rank_to_string (rank : rank) : string =
    match rank with
    | QueenBee pts -> "QueenBee"
    | Genius pts -> "Genius"
    | Amazing pts -> "Amazing"
    | Great pts -> "Great"
    | Nice pts -> "Nice"
    | Solid pts -> "Solid"
    | Good pts -> "Good"
    | MovingUp pts -> "Moving Up"
    | GoodStart pts -> "Good Start"
    | Beginner pts -> "Beginner"

  (**Returns true if a word [word] is a new word (it has not already in
     [found_words]), otherwise returns false.*)
  and new_word (word : string) (found_words : string list) : bool =
    match found_words with
    | [] -> true
    | h :: t ->
        if h = String.uppercase_ascii word then false else new_word word t

  (**Returns true if a word [word] is a valid word. [word] is valid if it has
     not already been found in this game, if it is contained in the board of
     [game], and is contained in the dictionary of [game]. Returns false
     otherwise.*)
  and valid_word (word : string) (game : t) : bool =
    let new_word = new_word word game.found_words in
    let valid_board_word = MultiBoard.contains word game.board in
    let valid_dictionary_word = D.contains word game.dictionary in
    new_word && valid_board_word && valid_dictionary_word

  and score_calc_board (word : string) (board : MultiBoard.t) =
    if String.length word = 4 then 1
    else if MultiBoard.is_pangram word board then 7 + String.length word
    else String.length word

  and update (game : t) (word : string) : t =
    let original_score = game.score in

    let original_found_words = game.found_words in
    if new_word word original_found_words = false then
      { game with message = "You already found that word!" }
    else if valid_word word game then
      let points = score_calc_board word game.board in
      let new_score = original_score + points in
      {
        score = new_score;
        rank = calculate_rank game;
        found_words = String.uppercase_ascii word :: original_found_words;
        board = game.board;
        dictionary = game.dictionary;
        message = word ^ " +" ^ string_of_int points;
        highest_possible_score =
          highest_possible_board_score game.board game.dictionary;
      }
    else { game with message = "Not a valid word. :(" }

  and found (game : t) : string list = game.found_words

  and shuffle (game : t) : t =
    let shuffled_board = MultiBoard.shuffle game.board in
    { game with board = shuffled_board }

  and reset (game : t) : t =
    { game with found_words = []; score = 0; message = "" }

  and all_filtered_words_board (dict : D.t) (board : MultiBoard.t) : DList.t =
    let board_data = MultiBoard.board_data board in
    let words_lst =
      List.fold_left
        (fun acc (elem : char * char list) ->
          let center, letters = elem in
          let char_lst = center :: letters in
          let lower_char_lst =
            List.map (fun char -> Char.lowercase_ascii char) char_lst
          in
          let all_words = D.of_char_list lower_char_lst dict in
          List.filter (fun word -> MultiBoard.contains word board) all_words
          @ acc)
        [] board_data
    in
    DList.of_list words_lst

  and all_filtered_words_game_str (game : t) : string =
    let filtered_words =
      DList.to_list (all_filtered_words_board game.dictionary game.board)
    in
    List.fold_left (fun acc elem -> elem ^ "\n" ^ acc) "\n" filtered_words

  and highest_possible_board_score (board : MultiBoard.t) (dict : D.t) : int =
    let words = DList.to_list (all_filtered_words_board dict board) in
    List.fold_left (fun acc word -> acc + score_calc_board word board) 0 words

  and get_highest_possible_score (game : t) : int = game.highest_possible_score

  and calculate_rank (game : t) : rank =
    let hs = float_of_int game.highest_possible_score in
    let s = game.score in
    let good_start = int_of_float (hs *. 0.02) in
    let moving_up = int_of_float (hs *. 0.05) in
    let good = int_of_float (hs *. 0.08) in
    let solid = int_of_float (hs *. 0.15) in
    let nice = int_of_float (hs *. 0.25) in
    let great = int_of_float (hs *. 0.4) in
    let amazing = int_of_float (hs *. 0.5) in
    let genius = int_of_float (hs *. 0.7) in

    if 0 <= s && s < good_start then Beginner 0
    else if good_start <= s && s < moving_up then
      GoodStart (int_of_float (hs *. 0.02))
    else if moving_up <= s && s < good then MovingUp (int_of_float (hs *. 0.05))
    else if good <= s && s < solid then Good (int_of_float (hs *. 0.08))
    else if solid <= s && s < nice then Solid (int_of_float (hs *. 0.15))
    else if nice <= s && s < great then Nice (int_of_float (hs *. 0.25))
    else if great <= s && s < nice then Great (int_of_float (hs *. 0.4))
    else if amazing <= s && s < genius then Amazing (int_of_float (hs *. 0.5))
    else if genius <= s && s < 1 then Genius (int_of_float (hs *. 0.7))
    else QueenBee 1

  and calculate_rank_str (s : int) (high_score : int) : string =
    let hs = float_of_int high_score in
    let good_start = int_of_float (hs *. 0.02) in
    let moving_up = int_of_float (hs *. 0.05) in
    let good = int_of_float (hs *. 0.08) in
    let solid = int_of_float (hs *. 0.15) in
    let nice = int_of_float (hs *. 0.25) in
    let great = int_of_float (hs *. 0.4) in
    let amazing = int_of_float (hs *. 0.5) in
    let genius = int_of_float (hs *. 0.7) in

    if 0 <= s && s < good_start then "Beginner"
    else if good_start <= s && s < moving_up then "GoodStart"
    else if moving_up <= s && s < good then "MovingUp"
    else if good <= s && s < solid then "Good"
    else if solid <= s && s < nice then "Solid"
    else if nice <= s && s < great then "Nice"
    else if great <= s && s < nice then "Great"
    else if amazing <= s && s < genius then "Amazing"
    else if genius <= s && s < 1 then "Genius"
    else "QueenBee"

  and contains_pangram (dict : D.t) (board : MultiBoard.t) : bool =
    let words = DList.to_list (all_filtered_words_board dict board) in
    List.exists (fun word -> MultiBoard.is_pangram word board) words

  and good_boards_list : MultiBoard.t list ref = ref []

  and find_best_board (count : int) (shape : MultiBoard.shape) (dict : D.t)
      (custom_words : string list option) : MultiBoard.t option =
    match count with
    | 0 -> None
    | x ->
        let board = MultiBoard.build shape custom_words in
        if contains_pangram dict board then Some board
        else
          let old_good_boards_list = !good_boards_list in
          good_boards_list := board :: old_good_boards_list;
          find_best_board (x - 1) shape dict custom_words

  and best_board (count : int) (shape : MultiBoard.shape)
      (custom_words : string list option) (dict : D.t) : MultiBoard.t =
    match find_best_board count shape dict custom_words with
    | Some board -> board
    | None ->
        let good_boards = !good_boards_list in
        List.fold_left
          (fun best_board_result elem ->
            if
              highest_possible_board_score elem dict
              > highest_possible_board_score best_board_result dict
            then elem
            else best_board_result)
          (List.hd good_boards) good_boards

  and print (game : t) : unit =
    let score = string_of_int game.score in
    print_newline ();
    print_endline ("Score: " ^ score);
    print_endline ("Rank: " ^ rank_to_string game.rank ^ "\n\n");
    MultiBoard.print game.board;
    print_endline game.message;
    print_newline ()

  and print_rankings (game : t) : t =
    let rank_lst = calculate_ranks game in
    let rank_str =
      List.fold_left
        (fun acc elem ->
          match elem with
          | QueenBee pts ->
              acc ^ "QueenBee            " ^ string_of_int pts ^ "\n"
          | Genius pts ->
              acc ^ "Genius              " ^ string_of_int pts ^ "\n"
          | Amazing pts ->
              acc ^ "Amazing             " ^ string_of_int pts ^ "\n"
          | Great pts -> acc ^ "Great               " ^ string_of_int pts ^ "\n"
          | Nice pts -> acc ^ "Nice                " ^ string_of_int pts ^ "\n"
          | Solid pts -> acc ^ "Solid               " ^ string_of_int pts ^ "\n"
          | Good pts -> acc ^ "Good                " ^ string_of_int pts ^ "\n"
          | MovingUp pts ->
              acc ^ "Moving Up           " ^ string_of_int pts ^ "\n"
          | GoodStart pts ->
              acc ^ "Good Start          " ^ string_of_int pts ^ "\n"
          | Beginner pts ->
              acc ^ "Beginner            " ^ string_of_int pts ^ "\n")
        ("\n" ^ "Rank                Minimum Score" ^ "\n\n")
        rank_lst
    in
    print_endline rank_str;
    game
end
