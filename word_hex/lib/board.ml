(** The signature of word_hex boards. *)
module type BoardType = sig
  type t

  val build : string list option -> t
  val contains : string -> t -> bool
  val shuffle : t -> t
  val print : t -> unit
  val get_letters : t -> char list
  val board_of_letters : char list -> t
end

(*******************************************************)
(***************** HELPER FUNCTIONS ********************)

type hex = {
  center : char;
  h0 : char;
  h1 : char;
  h2 : char;
  h3 : char;
  h4 : char;
  h5 : char;
}
(** Type representing a single hex of a board (6 letters in a hexagon pattern
    around a 7th centeral letter)*)

(** Type representing the 7 positions on a hex. Center is the central letter,
    while Hn for n in (0..5) inclusive goes around the hexagon clockwise with H0
    starting at 12:00. *)
type hex_pos =
  | Center
  | H0
  | H1
  | H2
  | H3
  | H4
  | H5

(** Association List for uppercase english vowels*)
let vowel_list = [ (0, 'A'); (4, 'E'); (8, 'I'); (14, 'O'); (20, 'U') ]

(** Association List for common uppercase english consonants*)
let common_consonant_list =
  [
    (1, 'B');
    (2, 'C');
    (3, 'D');
    (5, 'F');
    (6, 'G');
    (7, 'H');
    (11, 'L');
    (12, 'M');
    (13, 'N');
    (15, 'P');
    (17, 'R');
    (18, 'S');
    (19, 'T');
    (24, 'Y');
  ]

(** Association List for uncommon uppercase english consonants*)
let uncommon_consonant_list =
  [ (9, 'J'); (10, 'K'); (16, 'Q'); (21, 'V'); (22, 'W'); (23, 'X'); (25, 'Z') ]

(* Debugging Helper function to print a character list*)
let rec print_char_list_helper lst =
  match lst with
  | [] -> ()
  | h :: t ->
      print_char h;
      print_string ";";
      print_char_list_helper t

(* Debugging function to print a character list*)

let print_char_list lst =
  print_newline ();
  print_string "[";
  print_char_list_helper lst;
  print_string "]";
  print_newline ()

(** union lst1 lst2 is the union of the two lists; it contains every element
    from both lists, and assuming no element is repeated in either list, no
    element is repeated in the final value. Requires, no repeated elements in
    lst1 or lst 2*)
let rec union lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: union t (List.filter (fun c -> c <> h) lst2)

(** Takes the first n elements from the list. If the length of the list is
    shorter than n, returns the whole list. Requires, n >= 0.*)
let rec take n lst =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t

(** Association List for letter combinations*)
let combinations =
  [
    (1, [ 'I'; 'N'; 'G' ]);
    (2, [ 'E'; 'D' ]);
    (3, [ 'U'; 'N' ]);
    (4, [ 'E'; 'R' ]);
    (5, [ 'C'; 'O' ]);
    (6, [ 'D'; 'E' ]);
    (7, [ 'R'; 'E' ]);
    (8, [ 'E'; 'X' ]);
  ]

(** Picks n random elements from lst. Ensures no two elements in the list
    returned come from the same 'index' of the list. This means, if all elements
    of lst are unique, all elements of the returned list will also be unique.
    Requires 0 <= n <= List.length lst*)
let rec pick_random (lst : ('a * 'b) list) (n : int) : 'b list =
  let len = List.length lst in
  if n == 0 then []
  else
    let key, pick = List.nth lst (Random.int len) in
    pick :: pick_random (List.remove_assoc key lst) (n - 1)

(** Randomizes the order of lst and appends result to acc*)
let rec randomize (acc : 'a list) (lst : 'a list) : 'a list =
  let len = List.length lst in
  match lst with
  | [] -> acc
  | _ ->
      let next = List.nth lst (Random.int len) in
      randomize (acc @ [ next ]) (List.filter (fun x -> x != next) lst)

(** Returns if the given hex contains the word. The given hex contains the word
    all characters of the word are found within the hex*)
let rec hex_contains (h : hex) (word : string) : bool =
  let hlst = [ h.center; h.h0; h.h1; h.h2; h.h3; h.h4; h.h5 ] in
  String.fold_left
    (fun bl chr -> bl && List.exists (fun lst_chr -> lst_chr = chr) hlst)
    true word

(*******************************************************)
(***************** HEX BOARD MODULE ********************)

(** A Word Hex Board. *)
module HexBoard : BoardType = struct
  type t = hex

  let build_random () : t =
    let combo =
      match pick_random combinations 1 with
      | [] -> assert false
      | c :: _ -> c
    in
    let vowels = pick_random vowel_list 2 in
    let common_consonants = pick_random common_consonant_list 4 in
    let uncommon_consonants = pick_random uncommon_consonant_list 1 in
    let long_list =
      union combo (vowels @ common_consonants @ uncommon_consonants)
    in
    let final_list = take 7 long_list in
    let random_list = randomize [] final_list in
    {
      center = List.nth random_list 0;
      h0 = List.nth random_list 1;
      h1 = List.nth random_list 2;
      h2 = List.nth random_list 3;
      h3 = List.nth random_list 4;
      h4 = List.nth random_list 5;
      h5 = List.nth random_list 6;
    }

  let build_custom (input : string list) : t = failwith "Unimplemented"

  let build (input : string list option) : t =
    match input with
    | None -> build_random ()
    | Some input_lst -> build_random ()

  let contains (word : string) (board : t) : bool =
    let word_upper = String.uppercase_ascii word in
    String.length word >= 4
    && String.contains word_upper board.center
    && hex_contains board word_upper

  let shuffle (board : t) : t =
    let outer =
      randomize []
        [ board.h0; board.h1; board.h2; board.h3; board.h4; board.h5 ]
    in
    {
      center = board.center;
      h0 = List.nth outer 0;
      h1 = List.nth outer 1;
      h2 = List.nth outer 2;
      h3 = List.nth outer 3;
      h4 = List.nth outer 4;
      h5 = List.nth outer 5;
    }

  let print (board : t) : unit =
    let short = "     " in
    let long = "         " in
    print_string short;
    print_char board.h0;
    print_newline ();
    print_char board.h5;
    print_string long;
    print_char board.h1;
    print_newline ();
    print_string short;
    print_char board.center;
    print_newline ();
    print_char board.h4;
    print_string long;
    print_char board.h2;
    print_newline ();
    print_string short;
    print_char board.h3;
    print_newline ()

  let get_letters b = [ b.center; b.h0; b.h1; b.h2; b.h3; b.h4; b.h5 ]

  let board_of_letters lst =
    assert (List.length lst = 7);
    {
      center = List.nth lst 0;
      h0 = List.nth lst 1;
      h1 = List.nth lst 2;
      h2 = List.nth lst 3;
      h3 = List.nth lst 4;
      h4 = List.nth lst 5;
      h5 = List.nth lst 6;
    }
end

(*******************************************************)
(***************** TWO HEX MODULE ********************)

module TwoHex : BoardType = struct
  type t = hex * hex

  let build = failwith "Unimplemented"
  let contains = failwith "Unimplemented"
  let shuffle = failwith "Unimplemented"
  let print = failwith "Unimplemented"
  let get_letters = failwith "Unimplemented"
  let board_of_letters = failwith "Unimplemented"
end

(**********************************************************)
(***************** TRIPLE BOARD MODULE ********************)

module TripleBoard : BoardType = struct
  type t = hex

  let build = failwith "Unimplemented"
  let contains = failwith "Unimplemented"
  let shuffle = failwith "Unimplemented"
  let print = failwith "Unimplemented"
  let get_letters = failwith "Unimplemented"
  let board_of_letters = failwith "Unimplemented"
end

(*******************************************************)
(***************** FLOWER BOARD MODULE ********************)

module FlowerBoard : BoardType = struct
  type t = hex

  let build = failwith "Unimplemented"
  let contains = failwith "Unimplemented"
  let shuffle = failwith "Unimplemented"
  let print = failwith "Unimplemented"
  let get_letters = failwith "Unimplemented"
  let board_of_letters = failwith "Unimplemented"
end
