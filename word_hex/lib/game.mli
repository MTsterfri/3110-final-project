open Board
open Multi
module D = TrieDictionary.Make

(** The signature of a word_hex game. *)
module type GameType = sig
  type t
  (** Representation type of the word_hex game. *)

  val build : string list option -> MultiBoard.shape -> D.t -> t
  (** Given a list of custom words [words] and the number of hexes that the game
      board should have [hexes], returns a word_hex game. *)

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

  val print : t -> unit
  (** Given a game [game], prints a visual representation of [game]. *)
end

module Game : GameType
(** A game based on a board [Board] and a dictionary [Dictionary]. *)
