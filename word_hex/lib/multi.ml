open Board

module type MultiType = sig
  type t
  type shape

  val shape_of_string : string -> shape option
  val string_of_shape : shape -> string
  val shape_of_board : t -> shape
  val build : shape -> string list option -> t
  val contains : string -> t -> bool
  val shuffle : t -> t
  val print : t -> unit
end

module MultiBoard : MultiType = struct
  type t = HexB of HexBoard.t
  type shape = OneHex

  let shape_of_string (s : string) : shape option =
    match s with
    | "OneHex" -> Some OneHex
    | _ -> None

  let string_of_shape (s : shape) : string =
    match s with
    | OneHex -> "OneHex"

  let shape_of_board (b : t) : shape =
    match b with
    | HexB _ -> OneHex

  let build (s : shape) (input : string list option) : t =
    match s with
    | OneHex -> HexB (HexBoard.build input)

  let contains (word : string) (board : t) : bool =
    match board with
    | HexB b -> HexBoard.contains word b

  let shuffle (board : t) : t =
    match board with
    | HexB b -> HexB (HexBoard.shuffle b)

  let print (board : t) : unit =
    match board with
    | HexB b -> HexBoard.print b
end
