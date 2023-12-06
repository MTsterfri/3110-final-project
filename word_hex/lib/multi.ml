open Board

module type MultiType = sig
  type t
  type shape

  val shape_of_string : string -> shape option
  val string_of_shape : shape -> string
  val shape_of_board : t -> shape
  val build : shape -> string list option -> t
  val contains : string -> t -> bool
  val is_pangram : string -> t -> bool
  val shuffle : t -> t
  val print : t -> unit
  val get_letters : t -> char list
  val board_of_letters : shape -> char list -> t
  val board_data : t -> (char * char list) list
end

module MultiBoard : MultiType = struct
  type t =
    | HexB of HexBoard.t
    | TwoHexB of TwoHex.t

  type shape =
    | OneHex
    | TwoHex

  let shape_of_string (s : string) : shape option =
    match s with
    | "Hex" -> Some OneHex
    | "TwoHex" -> Some TwoHex
    | _ -> None

  let string_of_shape (s : shape) : string =
    match s with
    | OneHex -> "Hex"
    | TwoHex -> "TwoHex"

  let shape_of_board (b : t) : shape =
    match b with
    | HexB _ -> OneHex
    | TwoHexB _ -> TwoHex

  let build (s : shape) (input : string list option) : t =
    match s with
    | OneHex -> HexB (HexBoard.build input)
    | TwoHex -> TwoHexB (TwoHex.build input)

  let contains (word : string) (board : t) : bool =
    match board with
    | HexB b -> HexBoard.contains word b
    | TwoHexB b -> TwoHex.contains word b

  let is_pangram (word : string) (board : t) : bool =
    match board with
    | HexB b -> HexBoard.is_pangram word b
    | TwoHexB b -> TwoHex.is_pangram word b

  let shuffle (board : t) : t =
    match board with
    | HexB b -> HexB (HexBoard.shuffle b)
    | TwoHexB b -> TwoHexB (TwoHex.shuffle b)

  let print (board : t) : unit =
    match board with
    | HexB b -> HexBoard.print b
    | TwoHexB b -> TwoHex.print b

  let get_letters (board : t) : char list =
    match board with
    | HexB b -> HexBoard.get_letters b
    | TwoHexB b -> TwoHex.get_letters b

  let board_of_letters (s : shape) (lst : char list) : t =
    match s with
    | OneHex -> HexB (HexBoard.board_of_letters lst)
    | TwoHex -> TwoHexB (TwoHex.board_of_letters lst)

  let board_data (board : t) : (char * char list) list =
    match board with
    | HexB b -> HexBoard.board_data b
    | TwoHexB b -> TwoHex.board_data b
end