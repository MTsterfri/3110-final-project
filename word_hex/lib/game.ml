open Board
open Multi
module D = TrieDictionary.Make
module DList = ListDictionary.Make

(** The signature of a word_hex game. *)
module type GameType = sig
  type t

  type rank =
    | QueenBee of float
    | Genius of float
    | Amazing of float
    | Great of float
    | Nice of float
    | Solid of float
    | Good of float
    | MovingUp of float
    | GoodStart of float
    | Beginner of float

  val build : string list option -> MultiBoard.shape -> D.t -> t
  val update : t -> string -> t
  val found : t -> string list
  val shuffle : t -> t
  val reset : t -> t
  val print : t -> unit
end

module Game : GameType = struct
  type rank =
    | QueenBee of float
    | Genius of float
    | Amazing of float
    | Great of float
    | Nice of float
    | Solid of float
    | Good of float
    | MovingUp of float
    | GoodStart of float
    | Beginner of float

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
    let chosen_board = MultiBoard.build shape words in
    let hs = highest_possible_board_score chosen_board dict in
    {
      score = 0;
      rank = Beginner 0.0;
      found_words = [];
      board = chosen_board;
      dictionary = dict;
      message = "";
      highest_possible_score = hs;
    }

  and calculate_ranks (game : t) : rank list =
    let hs = float_of_int game.highest_possible_score in
    [
      Beginner (0.0 *. hs);
      GoodStart (0.02 *. hs);
      MovingUp (0.05 *. hs);
      Good (0.8 *. hs);
      Solid (0.15 *. hs);
      Nice (0.25 *. hs);
      Great (0.4 *. hs);
      Amazing (0.5 *. hs);
      Genius (0.7 *. hs);
      QueenBee hs;
    ]

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

  (**Returns the number of points associated with a word [word]. The number of
     points associated with [word] is equal to the length of [word], unless
     [word] is only four letters long, for which it is then only associated with
     one point.*)
  and score_calc_game (word : string) (game : t) : int =
    if String.length word = 4 then 1
    else if MultiBoard.is_pangram word game.board then 7 + String.length word
    else String.length word

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
      let points = score_calc_game word game in
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
          let all_words = D.of_char_list char_lst dict in
          List.filter (fun word -> String.contains word center) all_words @ acc)
        [] board_data
    in
    DList.of_list words_lst

  and all_filtered_words_game (game : t) : DList.t =
    let board_data = MultiBoard.board_data game.board in
    let words_lst =
      List.fold_left
        (fun acc (elem : char * char list) ->
          let center, letters = elem in
          let char_lst = center :: letters in
          let all_words = D.of_char_list char_lst game.dictionary in
          List.filter (fun word -> String.contains word center) all_words @ acc)
        [] board_data
    in
    DList.of_list words_lst

  and highest_possible_board_score (board : MultiBoard.t) (dict : D.t) : int =
    let words = DList.to_list (all_filtered_words_board dict board) in
    List.fold_left (fun acc word -> acc + score_calc_board word board) 0 words

  and get_highest_possible_score (game : t) : int = game.highest_possible_score

  and calculate_rank (game : t) : rank =
    let hs = float_of_int game.highest_possible_score in
    let score = float_of_int game.score in
    let num = score /. hs in
    if 0.0 <= num && num < 0.02 then Beginner 0.0
    else if 0.02 <= num && num < 0.05 then GoodStart (hs *. 0.02)
    else if 0.05 <= num && num < 0.08 then MovingUp (hs *. 0.05)
    else if 0.08 <= num && num < 0.15 then Good (hs *. 0.08)
    else if 0.15 <= num && num < 0.25 then Solid (hs *. 0.15)
    else if 0.25 <= num && num < 0.4 then Nice (hs *. 0.25)
    else if 0.4 <= num && num < 0.5 then Great (hs *. 0.4)
    else if 0.5 <= num && num < 0.7 then Amazing (hs *. 0.5)
    else if 0.7 <= num && num < 1.0 then Genius (hs *. 0.7)
    else QueenBee 1.0

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
      (custom_words : string list option) (game : t) : MultiBoard.t =
    match find_best_board count shape game.dictionary custom_words with
    | Some board -> board
    | None ->
        let good_boards = !good_boards_list in
        List.fold_left
          (fun best_board_result elem ->
            if
              highest_possible_board_score elem game.dictionary
              > highest_possible_board_score best_board_result game.dictionary
            then elem
            else best_board_result)
          (List.hd good_boards) good_boards

  and print (game : t) : unit =
    let score = string_of_int game.score in
    print_endline ("Score: " ^ score ^ "\n\n");
    MultiBoard.print game.board;
    print_endline game.message;
    print_newline ()
end
