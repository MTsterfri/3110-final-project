open Board
open Multi
module D = TrieDictionary.Make
module DList = ListDictionary.Make

(** The signature of a word_hex game. *)
module type GameType = sig
  type t
  (** Representation type of the word_hex game. *)

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
  (** Given a list of custom words [words] and the number of hexes that the game
      board should have [hexes], returns a word_hex game. *)

  val build_of_board :
    string list option -> MultiBoard.shape -> D.t -> MultiBoard.t -> t

  val get_board : t -> MultiBoard.t
  (** Returns the board [board] of a given game [game]*)

  val get_dict : t -> D.t
  (**Returns the dictionary [dict] of a given game [game]*)

  val get_score : t -> int
  (**Returns the score [score] of a given game [game]*)

  val update : t -> string -> t
  (** Given an original game [game] and a guessed word [word], returns the
      updated game *)

  val found : t -> string list
  (** Given a game [game], returns a list of the words already found in the
      game. *)

  val shuffle : t -> t
  (**Given a game [game], returns the same [game] except with the board
     shuffled.*)

  val reset : t -> t
  (**Given a game [game], resets [game]. Resetting [game] means keeping its
     board and dictionary, but erasing all previously found words, deleting the
     previous message (if there was one), and resetting the score to 0.*)

  val best_board :
    int -> MultiBoard.shape -> string list option -> D.t -> MultiBoard.t
  (*Given a count of boards [count], the shape of the board [shape], a list of
    custom words [custom_words], a dictionary [dict], returns the best of
    [count] number of boards. While making the boards, returns the first board
    that includes a pangram. If the board does not contain a pangram, returns
    the board with the highest possible score. *)

  val contains_pangram : D.t -> MultiBoard.t -> bool
  (**Returns whether or not the given game [game]*)

  val all_filtered_words_game : t -> DList.t
  (**Given a game [game], returns a ListDictionary of the words in the
     dictionary that contain the letters in the board of the [game], filtered on
     the fact that the words must contain the center letter.*)

  val get_highest_possible_score : t -> int
  val score_calc_game : string -> t -> int
  val print_rankings : t -> t

  val print : t -> unit
  (** Given a game [game], prints a visual representation of [game]. *)
end

module Game : GameType
(** A game based on a board [Board] and a dictionary [Dictionary]. *)
