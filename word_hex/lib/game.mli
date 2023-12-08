(** Representation of a Word Hex game. *)

open Board
open Multi

module D = TrieDictionary.Make
(** Module for dictionaries represented as tries. *)

module DList = ListDictionary.Make
(** Module for dictionaries represented as lists. *)

(** The signature of a Word Hex game. *)
module type GameType = sig
  type t
  (** Representation type of the Word Hex game. *)

  (**Representation type of the possible ranks earned during game play. Each
     rank stores the minimum score needed to reach each rank.*)
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

  type data = {
    shape : string;
    char_list : string;
    score : int;
    rank : string;
    found_words : string;
    highest_possible_score : int;
  }
  (**Representation type of the game data. *)

  val build : string list option -> MultiBoard.shape -> D.t -> t
  (** Given a list of custom words and the number of hexes that the game board
      should have, returns a Word Hex game. *)

  val build_of_board :
    string list option -> MultiBoard.shape -> D.t -> MultiBoard.t -> t
  (** Given a list of custom words, a shape of the board, and a dictionary,
      returns a new board. *)

  val build_of_data : data -> D.t -> t
  (** Given board data and a dictionary, makes a new game. *)

  val get_game_data : t -> data
  (**Given a game, returns the data stored in the game. *)

  val get_board : t -> MultiBoard.t
  (** Returns the board of a given game. *)

  val get_dict : t -> D.t
  (**Returns the dictionary of a given game. *)

  val get_score : t -> int
  (**Returns the score of a given game. *)

  val get_rank_str : t -> string
  (**Returns a string version of the rank of a given game. *)

  val update : t -> string -> t
  (** Given an original game and a guessed word, returns the updated game. *)

  val found : t -> string list
  (** Given a game, returns a list of the words already found in the game. *)

  val shuffle : t -> t
  (**Given a game, returns the same game except with the board shuffled. *)

  val reset : t -> t
  (**Given a game, resets it. Resetting a game means keeping its board and
     dictionary, but erasing all previously found words, deleting the previous
     message (if there was one), and resetting the score to 0. *)

  val best_board :
    int -> MultiBoard.shape -> string list option -> D.t -> MultiBoard.t
  (**Given a count of boards, the shape of the board, a list of custom words,
     and a dictionary, returns the best of the given number of boards. While
     making the boards, returns the first board that includes a pangram. If the
     board does not contain a pangram, returns the board with the highest
     possible score. *)

  val contains_pangram : D.t -> MultiBoard.t -> bool
  (**Returns whether or not the given game contains a pangram. *)

  val all_filtered_words_board : D.t -> MultiBoard.t -> DList.t
  (**Given a game, returns a ListDictionary of the words in the dictionary that
     contain the letters in the board of the game, filtered on the fact that the
     words must contain the center letter. *)

  val all_filtered_words_game_str : t -> string
  (**Returns a string of all the words in a game that can earn the player
     points. Each word in the game is on a new line in the string. *)

  val get_highest_possible_score : t -> int
  (**Returns the highest possible score a player can earn by playing a game. *)

  val score_calc_board : string -> MultiBoard.t -> int
  (**Given a board and a word, calculates the score earned by that word on the
     board. *)

  val calculate_rank_str : int -> int -> string
  (**Given a player's current score and the high score of a game, return the
     string version of the player's current rank. *)

  val print_rankings : t -> t
  (**Given a game, prints the rankings and the score needed to reach each
     ranking for that game. Returns the game. *)

  val print : t -> unit
  (** Given a game, prints a visual representation of it. *)
end

module Game : GameType
(** A game based on a board [Board] and a dictionary [Dictionary]. *)
