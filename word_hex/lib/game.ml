open Board
open Dictionary

(** The signature of a word_hex game. *)
module type GameType = sig
  type t

  val build : string list option -> HashDict.t -> t
  val update : t -> string -> t
  val found : t -> string list
  val print : t -> unit
end

module Game (Board : BoardType) : GameType = struct
  module Board : BoardType = Board

  type t = {
    score : int;
    found_words : string list;
    board : Board.t;
    dictionary : HashDict.t;
    message : string;
  }

  let build (words : string list option) (dict : HashDict.t) : t =
    {
      score = 0;
      found_words = [];
      board = Board.build words;
      dictionary = dict;
      message = "";
    }

  (**Returns true if a word [word] is a new word (it has not already in
     [found_words]), otherwise returns false.*)
  let rec new_word (word : string) (found_words : string list) : bool =
    match found_words with
    | [] -> true
    | h :: t ->
        if h = String.uppercase_ascii word then false else new_word word t

  (**Returns true if a word [word] is a valid word. [word] is valid if it has
     not already been found in this game, if it is contained in the board of
     [game], and is contained in the dictionary of [game]. Returns false
     otherwise.*)
  let valid_word (word : string) (game : t) : bool =
    let new_word = new_word word game.found_words in
    let valid_board_word = Board.contains word game.board in
    let valid_dictionary_word = HashDict.contains word game.dictionary in
    new_word && valid_board_word && valid_dictionary_word

  (**Returns the number of points associated with a word. The number of points
     associated with a word is equal to the length of the word, unless the word
     is only four letters long, for which it is then only associated with one
     point.*)
  let score_calc (word : string) : int =
    if String.length word = 4 then 1 else String.length word

  let update (game : t) (word : string) : t =
    let original_score = game.score in
    let original_found_words = game.found_words in
    if new_word word original_found_words = false then
      { game with message = "You already found that word!" }
    else if valid_word word game then
      let new_score = original_score + score_calc word in
      {
        score = new_score;
        found_words = String.uppercase_ascii word :: original_found_words;
        board = game.board;
        dictionary = game.dictionary;
        message = word ^ " +" ^ string_of_int new_score;
      }
    else game

  let update (game : t) (word : string) : t = failwith "Unimplemented"
  let found (game : t) : string list = failwith "Unimplemented"
  let print (game : t) : unit = failwith "Unimplemented"
end
