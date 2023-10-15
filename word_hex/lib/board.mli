(** The signature of word_hex boards. *)
module type BoardType = sig
  type t
  (** Type representing the data in the board. *)

  val build : string list option -> int -> t
  (** Convert a sampleable bag to a list of items. *)

  val contains : string -> t -> bool
  (** Returns if the board contains the string in a valid way. A board contains
      a word in a valid way if all the letters are found within one hex (group
      of seven letters grouped in a hexagon), and the center letter of that hex
      is included in the word. Additionally, the word is only valid if it is at
      least four characters long. *)

  val shuffle : t -> t
  (** Returns a board that has the same configuration but with the letters of
      each hexagon shuffle. If there are multiple hexes in the board, returns
      the exact same board*)

  val print : t -> unit
  (** Print visual representation of the board in the terminal. *)
end

module Board : BoardType
(** A Word Hex Board. *)
