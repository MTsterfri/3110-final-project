open Board
open Dictionary

(** The signature of a word_hex game. *)
module type GameType = sig
  type t
  (** Representation type of the word_hex game. *)

  val build : int -> t
  (** Builds a word_hex game, given an integer parameter [n] describing the size
      of n-grams and a list of strings [corpus]. Requires: [n > 0]. *)

  val generate_next : t -> string list -> string option
  (** Given a list of strings [prompt], generate the next word. Returns [None]
      if the model can't generate a word. Requires: the length of [prompt] must
      be at least [n - 1], where [n] is the n-gram size. *)

  val list_ngrams : t -> string list list
  (** Produce the list of n-grams that are stored in the model. No n-gram should
      be produced more than once. *)

  val size : t -> int
  (** Get the parameter [n], for a n-gram model. *)
end

(** A model based on a sampleable bag [Bag]. *)
(* module Game (Board : BoardType) (Dictionary : DictionaryType): GameType *)
