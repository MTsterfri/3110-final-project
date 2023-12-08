(** The signature of the module that manages word_hex boards. *)
module type MultiType = sig
  type t
  (** Type representing the data in the board. *)

  type shape
  (** Type representing the shape of the board. *)

  val shape_of_string : string -> shape option
  (** Returns Some shape corresponding to the given string. If the string does
      not correspond to any shape, returns None. Valid strings are "Hex",
      "TwoHex", "Triple", and Flower*)

  val string_of_shape : shape -> string
  (** Returns the string corresponding to the given shape. Possible strings are
      "Hex", "TwoHex", "Triple", and "Flower"*)

  val shape_of_board : t -> shape
  (** Returns the shape of the given board.*)

  val build : shape -> string list option -> t
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

  val is_pangram : string -> t -> bool
  (** Returns if the given word contains all the letters of one hex on the
      multiboard*)

  val shuffle : t -> t
  (** Returns a board that has the same configuration but with the letters of
      each hexagon shuffle. If there are multiple hexes in the board, returns
      the exact same board*)

  val string_of_board : t -> string
  (** Returns a string that visually represents the board when printed*)

  val print : t -> unit
  (** Print visual representation of the board in the terminal. *)

  val get_letters : t -> char list
  (** Gets a list of letters that can be used to remake the same board with the
      function board_of_letters*)

  val board_of_letters : shape -> char list -> t
  (** Makes a board from the the output of get_letters*)

  val board_data : t -> (char * char list) list
  (** Returns a list of tuples, where each tuple represents one hex in a
      multiboard. The first element of each tuple is the center character, the
      second element is a list of the other six characters of the hex. *)
end

module MultiBoard : MultiType
(** A Word Hex Board with only one hex (a hex is a group of 7 letters arranged
    in a hexagon). *)
