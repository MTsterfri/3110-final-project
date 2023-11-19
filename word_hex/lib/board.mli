(** The signature of word_hex boards. *)
module type BoardType = sig
  type t
  (** Type representing the data in the board. *)

  val build : string list option -> t
  (** Given a list of custom words and the number of hexes that the board should
      have, produces a board representation type t that includes some of the
      words in that list. Requires that the list of words contains only strings
      made with uppercase A-Z; no spaces, no puctuation, or any other characters*)

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

  val get_letters : t -> char list
  (** Gets a list of letters that can be used to remake the same board with the
      function board_of_letters*)

  val board_of_letters : char list -> t
  (** Makes a board from the the output of get_letters*)
end

module HexBoard : BoardType
(** A Word Hex Board with only one hex (a hex is a group of 7 letters arranged
    in a hexagon). *)

module TwoHex : BoardType
(** A Word Hex Board with two hexes (a hex is a group of 7 letters arranged in a
    hexagon) arranged so that two of the outer letters overlap. *)

module TripleBoard : BoardType
(** A Word Hex Board with four hexes (a hex is a group of 7 letters arranged in
    a hexagon) that overlap such that there is one hex in the center and three
    hexes surrounding the center hex, where the center letters of the outer
    hexes are also outer letters of the center hex. *)

module FlowerBoard : BoardType
(** A Word Hex Board with four hexes (a hex is a group of 7 letters arranged in
    a hexagon) arranged so that there is one hex in the center and three hexes
    surrounding the center hex, where two outer letters from each outer hex
    overlap with the outer letters of the center board. *)
