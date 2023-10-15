open Board
open Dictionary

(** The signature of a word_hex game. *)
module type GameType = sig
  type t
  (** Representation type of the word_hex game. *)

  val build : string list option -> int -> t
  (** Given a list of custom words [words] and the number of hexes that the game
      board should have [hexes], returns a word_hex game. *)

  val update : t -> string -> t
  (** Given an original game [game] and a guessed word [word], returns the
      updated game *)

  val found : t -> string list
  (** Given a game [game], returns a list of the words already found in the
      game. *)

  val print : t -> unit
  (** Given a game [game], prints a visual representation of [game]. *)
end

(** A game based on a board [Board] and a dictionary [Dictionary]. *)
(* module Game (Board : BoardType) (Dictionary : DictionaryType) : GameType *)