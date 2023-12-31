(** The signature of word_hex boards. *)
module type BoardType = sig
  type t

  val rep_ok : t -> bool
  val build : string list option -> t
  val contains : string -> t -> bool
  val is_pangram : string -> t -> bool
  val shuffle : t -> t
  val string_of_board : t -> string
  val print : t -> unit
  val get_letters : t -> char list
  val board_of_letters : char list -> t
  val board_data : t -> (char * char list) list
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
    (* (18, 'S');  *)
    (* Actual Spelling Bee Doesn't include 'S'*)
    (19, 'T');
  ]

(** Association List for uncommon uppercase english consonants*)
let uncommon_consonant_list =
  [
    (9, 'J');
    (10, 'K');
    (* (16, 'Q'); *)
    (* Don't know how to get Q to always accompany a U*)
    (21, 'V');
    (22, 'W');
    (23, 'X');
    (25, 'Z');
    (24, 'Y');
  ]

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

(** difference lst1 lst2 is the difference of the two lists; it contains every
    element in lst1 that is not in lst2*)
let rec difference lst1 lst2 =
  match lst1 with
  | [] -> []
  | h :: t ->
      if List.exists (fun x -> x = h) lst2 then difference t lst2
      else h :: difference t lst2

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

(** Returns if all characters of the word are found within the given hex*)
let rec hex_contains_helper (h : hex) (word : string) : bool =
  let hlst = [ h.center; h.h0; h.h1; h.h2; h.h3; h.h4; h.h5 ] in
  String.fold_left
    (fun bl chr -> bl && List.exists (fun lst_chr -> lst_chr = chr) hlst)
    true word

(** Returns if the given hex contains the word. The given hex contains the word
    all characters of the word are found within the hex, and the center letter
    of the hex is found within the word.*)
let hex_contains (h : hex) (word : string) : bool =
  String.contains word h.center && hex_contains_helper h word

let hex_is_pangram (h : hex) (word : string) : bool =
  String.contains word h.center
  && String.contains word h.h0 && String.contains word h.h1
  && String.contains word h.h2 && String.contains word h.h3
  && String.contains word h.h4 && String.contains word h.h5

let hex_build_random () : hex =
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

(** Takes in a lst of 7-n characters, representing letters on a partially filled
    in hex. Returns a list of n additional letters to complete the hex *)
let fill_in_n (lst : char list) (n : int) : char list =
  assert (List.length lst = 7 - n);
  let combo =
    match pick_random combinations 1 with
    | [ c ] -> c
    | _ -> assert false
  in
  let vowels = pick_random vowel_list 2 in
  let cc = pick_random common_consonant_list 4 in
  let uc = pick_random uncommon_consonant_list 1 in
  let extras_other = difference (vowels @ cc @ uc) combo in
  let extras = combo @ randomize [] extras_other in
  let extra_letters = difference extras lst in
  take n extra_letters

let list_contains (elem : 'a) (lst : 'a list) : bool =
  List.exists (fun x -> x = elem) lst

(* Checks that each character in the hex is unique*)
let hex_rep_ok (h : hex) : bool =
  (not (list_contains h.center [ h.h0; h.h1; h.h2; h.h3; h.h4; h.h5 ]))
  && (not (list_contains h.h0 [ h.h1; h.h2; h.h3; h.h4; h.h5; h.center ]))
  && (not (list_contains h.h1 [ h.h2; h.h3; h.h4; h.h5; h.center; h.h0 ]))
  && (not (list_contains h.h2 [ h.h3; h.h4; h.h5; h.center; h.h0; h.h1 ]))
  && (not (list_contains h.h3 [ h.h4; h.h5; h.center; h.h0; h.h1; h.h2 ]))
  && (not (list_contains h.h4 [ h.h5; h.center; h.h0; h.h1; h.h2; h.h3 ]))
  && not (list_contains h.h5 [ h.center; h.h0; h.h1; h.h2; h.h3; h.h4 ])

let list_hex (h : hex) : char list =
  [ h.center; h.h0; h.h1; h.h2; h.h3; h.h4; h.h5 ]

(*******************************************************)
(***************** HEX BOARD MODULE ********************)

(** A Word Hex Board. *)
module HexBoard : BoardType = struct
  type t = hex

  let rep_ok h = hex_rep_ok h
  let build_random () : t = hex_build_random ()
  let build_custom (input : string list) : t = failwith "Unimplemented"

  let build (input : string list option) : t =
    match input with
    | None -> build_random ()
    | Some input_lst -> build_random ()

  let contains (word : string) (board : t) : bool =
    let word_upper = String.uppercase_ascii word in
    String.length word >= 4 && hex_contains board word_upper

  let is_pangram (word : string) (board : t) : bool =
    let word_upper = String.uppercase_ascii word in
    hex_is_pangram board word_upper

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

  let string_of_board (board : t) : string =
    let short = "     " in
    let long = "         " in
    short ^ String.make 1 board.h0 ^ "\n" ^ String.make 1 board.h5 ^ long
    ^ String.make 1 board.h1 ^ "\n" ^ short ^ String.make 1 board.center ^ "\n"
    ^ String.make 1 board.h4 ^ long ^ String.make 1 board.h2 ^ "\n" ^ short
    ^ String.make 1 board.h3 ^ "\n"

  let print (board : t) : unit = print_string (string_of_board board)
  (* let short = " " in let long = " " in print_string short; print_char
     board.h0; print_newline (); print_char board.h5; print_string long;
     print_char board.h1; print_newline (); print_string short; print_char
     board.center; print_newline (); print_char board.h4; print_string long;
     print_char board.h2; print_newline (); print_string short; print_char
     board.h3; print_newline () *)

  let get_letters b = list_hex b

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

  let board_data (b : t) : (char * char list) list =
    [ (b.center, [ b.h0; b.h1; b.h2; b.h3; b.h4; b.h5 ]) ]
end

(*******************************************************)
(***************** TWO HEX MODULE **********************)

module TwoHex : BoardType = struct
  type t = hex * hex

  let rep_ok (b1, b2) =
    hex_rep_ok b1 && hex_rep_ok b2 && b1.h2 = b2.h0 && b1.h3 = b2.h5

  let build_random () : t =
    let b1 = hex_build_random () in
    let b2_extra_letters = randomize [] (fill_in_n [ b1.h2; b1.h3 ] 5) in
    let b2 =
      {
        center = List.nth b2_extra_letters 0;
        h0 = b1.h2;
        h1 = List.nth b2_extra_letters 1;
        h2 = List.nth b2_extra_letters 2;
        h3 = List.nth b2_extra_letters 3;
        h4 = List.nth b2_extra_letters 4;
        h5 = b1.h3;
      }
    in
    (b1, b2)

  let build (input : string list option) : t =
    ignore input;
    build_random ()

  let contains (word : string) ((b1, b2) : t) : bool =
    let word_upper = String.uppercase_ascii word in
    String.length word >= 4
    && (hex_contains b1 word_upper || hex_contains b2 word_upper)

  let is_pangram (word : string) ((b1, b2) : t) : bool =
    let word_upper = String.uppercase_ascii word in
    hex_is_pangram b1 word_upper || hex_is_pangram b2 word_upper

  let shuffle_outer (b1, b2) =
    let b1_outer = [ b1.h0; b1.h1; b1.h4; b1.h5 ] in
    let b2_outer = [ b2.h1; b2.h2; b2.h3; b2.h4 ] in
    let b1_random = randomize [] b1_outer in
    let b2_random = randomize [] b2_outer in
    ( {
        center = b1.center;
        h0 = List.nth b1_random 0;
        h1 = List.nth b1_random 1;
        h2 = b1.h2;
        h3 = b1.h3;
        h4 = List.nth b1_random 2;
        h5 = List.nth b1_random 3;
      },
      {
        center = b2.center;
        h0 = b2.h0;
        h1 = List.nth b2_random 0;
        h2 = List.nth b2_random 1;
        h3 = List.nth b2_random 2;
        h4 = List.nth b2_random 3;
        h5 = b2.h5;
      } )

  let flip (b1, b2) =
    ( {
        center = b2.center;
        h0 = b2.h2;
        h1 = b2.h1;
        h2 = b2.h0;
        h3 = b2.h5;
        h4 = b2.h4;
        h5 = b2.h3;
      },
      {
        center = b1.center;
        h0 = b1.h2;
        h1 = b1.h1;
        h2 = b1.h0;
        h3 = b1.h5;
        h4 = b1.h4;
        h5 = b1.h3;
      } )

  let shuffle b =
    let b' = shuffle_outer b in
    if Random.int 3 = 0 then flip b' else b'

  let string_of_board ((b1, b2) : t) : string =
    let short = "     " in
    let med = "         " in
    let long = med ^ " " in
    short ^ String.make 1 b1.h0 ^ "\n" ^ String.make 1 b1.h5 ^ med
    ^ String.make 1 b1.h1 ^ "\n" ^ short ^ String.make 1 b1.center ^ "\n"
    ^ String.make 1 b1.h4 ^ med ^ String.make 1 b1.h2 ^ "\n" ^ short
    ^ String.make 1 b1.h3 ^ med ^ String.make 1 b2.h1 ^ "\n" ^ long
    ^ String.make 1 b2.center ^ "\n" ^ short ^ String.make 1 b2.h4 ^ med
    ^ String.make 1 b2.h2 ^ "\n" ^ long ^ String.make 1 b2.h3 ^ "\n"

  let print board = print_string (string_of_board board)
  (* let short = " " in let med = " " in let long = med ^ " " in print_string
     short; print_char b1.h0; print_newline (); print_char b1.h5; print_string
     med; print_char b1.h1; print_newline (); print_string short; print_char
     b1.center; print_newline (); print_char b1.h4; print_string med; print_char
     b1.h2; print_newline (); print_string short; print_char b1.h3; print_string
     med; print_char b2.h1; print_newline (); print_string long; print_char
     b2.center; print_newline (); print_string short; print_char b2.h4;
     print_string med; print_char b2.h2; print_newline (); print_string long;
     print_char b2.h3; print_newline () *)

  let get_letters (b1, b2) = list_hex b1 @ list_hex b2

  let board_of_letters lst =
    assert (List.length lst = 14);
    ( {
        center = List.nth lst 0;
        h0 = List.nth lst 1;
        h1 = List.nth lst 2;
        h2 = List.nth lst 3;
        h3 = List.nth lst 4;
        h4 = List.nth lst 5;
        h5 = List.nth lst 6;
      },
      {
        center = List.nth lst 7;
        h0 = List.nth lst 8;
        h1 = List.nth lst 9;
        h2 = List.nth lst 10;
        h3 = List.nth lst 11;
        h4 = List.nth lst 12;
        h5 = List.nth lst 13;
      } )

  let board_data ((b1, b2) : t) : (char * char list) list =
    [
      (b1.center, [ b1.h0; b1.h1; b1.h2; b1.h3; b1.h4; b1.h5 ]);
      (b2.center, [ b2.h0; b2.h1; b2.h2; b2.h3; b2.h4; b2.h5 ]);
    ]
end

(**********************************************************)
(***************** TRIPLE BOARD MODULE ********************)

module TripleBoard : BoardType = struct
  type t = {
    left : hex;
    right : hex;
    down : hex;
    center : hex;
  }

  let rep_ok ({ left = lh; right = rh; down = dh; center = ch } : t) : bool =
    hex_rep_ok lh && hex_rep_ok rh && hex_rep_ok dh && hex_rep_ok ch
    && ch.h0 = lh.h1 && ch.h0 = rh.h5 && ch.h5 = lh.center && ch.h1 = rh.center
    && ch.center = lh.h2 && ch.center = rh.h4 && ch.center = dh.h0
    && ch.h4 = lh.h3 && ch.h4 = dh.h5 && ch.h2 = rh.h3 && ch.h2 = dh.h1
    && ch.h3 = dh.center

  let build input =
    ignore input;
    (* First make the center hex*)
    let ch = hex_build_random () in
    (* Next make left hex - needs 3 additional letters*)
    let left_extra_letters = fill_in_n [ ch.center; ch.h0; ch.h4; ch.h5 ] 3 in
    let lh =
      {
        center = ch.h5;
        h0 = List.nth left_extra_letters 0;
        h1 = ch.h0;
        h2 = ch.center;
        h3 = ch.h4;
        h4 = List.nth left_extra_letters 1;
        h5 = List.nth left_extra_letters 2;
      }
    in
    (* Next make right hex - needs 3 additional letters*)
    let right_extra_letters = fill_in_n [ ch.center; ch.h0; ch.h1; ch.h2 ] 3 in
    let rh =
      {
        center = ch.h1;
        h0 = List.nth right_extra_letters 0;
        h1 = List.nth right_extra_letters 1;
        h2 = List.nth right_extra_letters 2;
        h3 = ch.h2;
        h4 = ch.center;
        h5 = ch.h0;
      }
    in
    (* Next make left hex - needs 3 additional letters*)
    let down_extra_letters = fill_in_n [ ch.center; ch.h2; ch.h3; ch.h4 ] 3 in
    let dh =
      {
        center = ch.h3;
        h0 = ch.center;
        h1 = ch.h2;
        h2 = List.nth down_extra_letters 0;
        h3 = List.nth down_extra_letters 1;
        h4 = List.nth down_extra_letters 2;
        h5 = ch.h4;
      }
    in
    { left = lh; right = rh; down = dh; center = ch }

  let contains (word : string)
      ({ left = lh; right = rh; down = dh; center = ch } : t) : bool =
    let word_upper = String.uppercase_ascii word in
    String.length word >= 4
    && (hex_contains lh word_upper || hex_contains rh word_upper
      || hex_contains dh word_upper || hex_contains ch word_upper)

  let is_pangram (word : string)
      ({ left = lh; right = rh; down = dh; center = ch } : t) : bool =
    let word_upper = String.uppercase_ascii word in
    hex_is_pangram lh word_upper
    || hex_is_pangram rh word_upper
    || hex_is_pangram dh word_upper
    || hex_is_pangram ch word_upper

  let shuffle_outer ({ left = lh; right = rh; down = dh; center = ch } : t) : t
      =
    let lh_outer = [ lh.h0; lh.h4; lh.h5 ] in
    let rh_outer = [ rh.h0; rh.h1; rh.h2 ] in
    let dh_outer = [ dh.h2; dh.h3; dh.h4 ] in
    let lh_random = randomize [] lh_outer in
    let rh_random = randomize [] rh_outer in
    let dh_random = randomize [] dh_outer in
    let lh_new =
      {
        center = lh.center;
        h0 = List.nth lh_random 0;
        h1 = lh.h1;
        h2 = lh.h2;
        h3 = lh.h3;
        h4 = List.nth lh_random 1;
        h5 = List.nth lh_random 2;
      }
    in
    let rh_new =
      {
        center = rh.center;
        h0 = List.nth rh_random 0;
        h1 = List.nth rh_random 1;
        h2 = List.nth rh_random 2;
        h3 = rh.h3;
        h4 = rh.h4;
        h5 = rh.h5;
      }
    in
    let dh_new =
      {
        center = dh.center;
        h0 = dh.h0;
        h1 = dh.h1;
        h2 = List.nth dh_random 0;
        h3 = List.nth dh_random 1;
        h4 = List.nth dh_random 2;
        h5 = dh.h5;
      }
    in
    { left = lh_new; right = rh_new; down = dh_new; center = ch }

  let flip_left ({ left = lh; right = rh; down = dh; center = ch } : t) : t =
    {
      left =
        {
          center = lh.center;
          h0 = lh.h4;
          h1 = lh.h3;
          h2 = lh.h2;
          h3 = lh.h1;
          h4 = lh.h0;
          h5 = lh.h5;
        };
      right =
        {
          center = dh.center;
          h0 = dh.h4;
          h1 = dh.h3;
          h2 = dh.h2;
          h3 = rh.h3;
          h4 = rh.h4;
          h5 = dh.h5;
        };
      down =
        {
          center = rh.center;
          h0 = dh.h0;
          h1 = dh.h1;
          h2 = rh.h2;
          h3 = rh.h1;
          h4 = rh.h0;
          h5 = rh.h5;
        };
      center =
        {
          center = ch.center;
          h0 = ch.h4;
          h1 = ch.h3;
          h2 = ch.h2;
          h3 = ch.h1;
          h4 = ch.h0;
          h5 = ch.h5;
        };
    }

  let flip_right ({ left = lh; right = rh; down = dh; center = ch } : t) : t =
    {
      left =
        {
          center = dh.center;
          h0 = dh.h2;
          h1 = dh.h1;
          h2 = lh.h2;
          h3 = lh.h3;
          h4 = dh.h4;
          h5 = dh.h3;
        };
      right =
        {
          center = rh.center;
          h0 = rh.h2;
          h1 = rh.h1;
          h2 = rh.h0;
          h3 = rh.h5;
          h4 = rh.h4;
          h5 = rh.h3;
        };
      down =
        {
          center = lh.center;
          h0 = dh.h0;
          h1 = lh.h1;
          h2 = lh.h0;
          h3 = lh.h5;
          h4 = lh.h4;
          h5 = dh.h5;
        };
      center =
        {
          center = ch.center;
          h0 = ch.h2;
          h1 = ch.h1;
          h2 = ch.h0;
          h3 = ch.h5;
          h4 = ch.h4;
          h5 = ch.h3;
        };
    }

  let flip_down ({ left = lh; right = rh; down = dh; center = ch } : t) : t =
    {
      left =
        {
          center = rh.center;
          h0 = rh.h0;
          h1 = lh.h1;
          h2 = lh.h2;
          h3 = rh.h3;
          h4 = rh.h2;
          h5 = rh.h1;
        };
      right =
        {
          center = lh.center;
          h0 = lh.h0;
          h1 = lh.h5;
          h2 = lh.h4;
          h3 = lh.h3;
          h4 = rh.h4;
          h5 = rh.h5;
        };
      down =
        {
          center = dh.center;
          h0 = dh.h0;
          h1 = dh.h5;
          h2 = dh.h4;
          h3 = dh.h3;
          h4 = dh.h2;
          h5 = dh.h1;
        };
      center =
        {
          center = ch.center;
          h0 = ch.h0;
          h1 = ch.h5;
          h2 = ch.h4;
          h3 = ch.h3;
          h4 = ch.h2;
          h5 = ch.h1;
        };
    }

  let shuffle b =
    let b' = shuffle_outer b in
    match Random.int 3 with
    | 0 -> flip_left b'
    | 1 -> flip_right b'
    | 2 -> flip_down b'
    | _ -> assert false

  let string_of_board ({ left = lh; right = rh; down = dh; center = ch } : t) :
      string =
    let short = "     " in
    let med = "         " in
    let long = med ^ " " in
    short ^ String.make 1 lh.h0 ^ med ^ String.make 1 rh.h0 ^ "\n"
    ^ String.make 1 lh.h5 ^ med ^ String.make 1 lh.h1 ^ med
    ^ String.make 1 rh.h1 ^ "\n" ^ short ^ String.make 1 lh.center ^ med
    ^ String.make 1 rh.center ^ "\n" ^ String.make 1 lh.h4 ^ med
    ^ String.make 1 lh.h2 ^ med ^ String.make 1 rh.h2 ^ "\n" ^ short
    ^ String.make 1 lh.h3 ^ med ^ String.make 1 rh.h3 ^ "\n" ^ long
    ^ String.make 1 dh.center ^ "\n" ^ short ^ String.make 1 dh.h4 ^ med
    ^ String.make 1 dh.h2 ^ "\n" ^ long ^ String.make 1 dh.h3 ^ "\n"

  let print (board : t) : unit = print_string (string_of_board board)
  (* let short = " " in let med = " " in let long = med ^ " " in print_string
     short; print_char lh.h0; print_string med; print_char rh.h0; print_newline
     (); print_char lh.h5; print_string med; print_char lh.h1; print_string med;
     print_char rh.h1; print_newline (); print_string short; print_char
     lh.center; print_string med; print_char rh.center; print_newline ();
     print_char lh.h4; print_string med; print_char lh.h2; print_string med;
     print_char rh.h2; print_newline (); print_string short; print_char lh.h3;
     print_string med; print_char rh.h3; print_newline (); print_string long;
     print_char dh.center; print_newline (); print_string short; print_char
     dh.h4; print_string med; print_char dh.h2; print_newline (); print_string
     long; print_char dh.h3; print_newline () *)

  let get_letters ({ left = lh; right = rh; down = dh; center = ch } : t) :
      char list =
    list_hex lh @ list_hex rh @ list_hex dh @ list_hex ch

  let board_of_letters lst =
    assert (List.length lst = 28);
    {
      left =
        {
          center = List.nth lst 0;
          h0 = List.nth lst 1;
          h1 = List.nth lst 2;
          h2 = List.nth lst 3;
          h3 = List.nth lst 4;
          h4 = List.nth lst 5;
          h5 = List.nth lst 6;
        };
      right =
        {
          center = List.nth lst 7;
          h0 = List.nth lst 8;
          h1 = List.nth lst 9;
          h2 = List.nth lst 10;
          h3 = List.nth lst 11;
          h4 = List.nth lst 12;
          h5 = List.nth lst 13;
        };
      down =
        {
          center = List.nth lst 14;
          h0 = List.nth lst 15;
          h1 = List.nth lst 16;
          h2 = List.nth lst 17;
          h3 = List.nth lst 18;
          h4 = List.nth lst 19;
          h5 = List.nth lst 20;
        };
      center =
        {
          center = List.nth lst 21;
          h0 = List.nth lst 22;
          h1 = List.nth lst 23;
          h2 = List.nth lst 24;
          h3 = List.nth lst 25;
          h4 = List.nth lst 26;
          h5 = List.nth lst 27;
        };
    }

  let board_data ({ left = lh; right = rh; down = dh; center = ch } : t) :
      (char * char list) list =
    [
      (lh.center, [ lh.h0; lh.h1; lh.h2; lh.h3; lh.h4; lh.h5 ]);
      (rh.center, [ rh.h0; rh.h1; rh.h2; rh.h3; rh.h4; rh.h5 ]);
      (dh.center, [ dh.h0; dh.h1; dh.h2; dh.h3; dh.h4; dh.h5 ]);
      (ch.center, [ ch.h0; ch.h1; ch.h2; ch.h3; ch.h4; ch.h5 ]);
    ]
end

(*******************************************************)
(***************** FLOWER BOARD MODULE ********************)

module FlowerBoard : BoardType = struct
  type t = {
    top : hex;
    down : hex;
    side : hex;
    center : hex;
  }

  let rep_ok ({ top = th; down = dh; side = sh; center = ch } : t) : bool =
    hex_rep_ok th && hex_rep_ok dh && hex_rep_ok sh && hex_rep_ok ch
    && ch.h0 = th.h2 && ch.h1 = sh.h5 && ch.h2 = sh.h4 && ch.h3 = dh.h1
    && ch.h4 = dh.h0 && ch.h5 = th.h3

  let build input =
    ignore input;
    (* First make the center hex*)
    let ch = hex_build_random () in
    (* Next make top hex - needs 5 additional letters*)
    let top_extra_letters = randomize [] (fill_in_n [ ch.h0; ch.h5 ] 5) in
    let th =
      {
        center = List.nth top_extra_letters 0;
        h0 = List.nth top_extra_letters 1;
        h1 = List.nth top_extra_letters 2;
        h2 = ch.h0;
        h3 = ch.h5;
        h4 = List.nth top_extra_letters 3;
        h5 = List.nth top_extra_letters 4;
      }
    in
    (* Next make down hex - needs 5 additional letters*)
    let down_extra_letters = randomize [] (fill_in_n [ ch.h3; ch.h4 ] 5) in
    let dh =
      {
        center = List.nth down_extra_letters 0;
        h0 = ch.h4;
        h1 = ch.h3;
        h2 = List.nth down_extra_letters 1;
        h3 = List.nth down_extra_letters 2;
        h4 = List.nth down_extra_letters 3;
        h5 = List.nth down_extra_letters 4;
      }
    in
    (* Next make side hex - needs 5 additional letters*)
    let side_extra_letters = randomize [] (fill_in_n [ ch.h1; ch.h2 ] 5) in
    let sh =
      {
        center = List.nth side_extra_letters 0;
        h0 = List.nth side_extra_letters 1;
        h1 = List.nth side_extra_letters 2;
        h2 = List.nth side_extra_letters 3;
        h3 = List.nth side_extra_letters 4;
        h4 = ch.h2;
        h5 = ch.h1;
      }
    in
    { top = th; down = dh; side = sh; center = ch }

  let contains (word : string)
      ({ top = th; down = dh; side = sh; center = ch } : t) : bool =
    let word_upper = String.uppercase_ascii word in
    String.length word >= 4
    && (hex_contains th word_upper || hex_contains dh word_upper
      || hex_contains sh word_upper || hex_contains ch word_upper)

  let is_pangram (word : string)
      ({ top = th; down = dh; side = sh; center = ch } : t) : bool =
    let word_upper = String.uppercase_ascii word in
    hex_is_pangram th word_upper
    || hex_is_pangram dh word_upper
    || hex_is_pangram sh word_upper
    || hex_is_pangram ch word_upper

  let shuffle_outer ({ top = th; down = dh; side = sh; center = ch } : t) : t =
    let th_outer = [ th.h0; th.h1; th.h4; th.h5 ] in
    let dh_outer = [ dh.h2; dh.h3; dh.h4; dh.h5 ] in
    let sh_outer = [ sh.h0; sh.h1; sh.h2; sh.h3 ] in
    let th_random = randomize [] th_outer in
    let dh_random = randomize [] dh_outer in
    let sh_random = randomize [] sh_outer in
    let th_new =
      {
        center = th.center;
        h0 = List.nth th_random 0;
        h1 = List.nth th_random 1;
        h2 = th.h2;
        h3 = th.h3;
        h4 = List.nth th_random 2;
        h5 = List.nth th_random 3;
      }
    in
    let dh_new =
      {
        center = dh.center;
        h0 = dh.h0;
        h1 = dh.h1;
        h2 = List.nth dh_random 0;
        h3 = List.nth dh_random 1;
        h4 = List.nth dh_random 2;
        h5 = List.nth dh_random 3;
      }
    in
    let sh_new =
      {
        center = sh.center;
        h0 = List.nth sh_random 0;
        h1 = List.nth sh_random 1;
        h2 = List.nth sh_random 2;
        h3 = List.nth sh_random 3;
        h4 = sh.h4;
        h5 = sh.h5;
      }
    in
    { top = th_new; down = dh_new; side = sh_new; center = ch }

  let flip_top ({ top = th; down = dh; side = sh; center = ch } : t) : t =
    {
      top =
        {
          center = th.center;
          h0 = th.h5;
          h1 = th.h4;
          h2 = th.h3;
          h3 = th.h2;
          h4 = th.h1;
          h5 = th.h0;
        };
      down =
        {
          center = sh.center;
          h0 = sh.h5;
          h1 = sh.h4;
          h2 = sh.h3;
          h3 = sh.h2;
          h4 = sh.h1;
          h5 = sh.h0;
        };
      side =
        {
          center = dh.center;
          h0 = dh.h5;
          h1 = dh.h4;
          h2 = dh.h3;
          h3 = dh.h2;
          h4 = dh.h1;
          h5 = dh.h0;
        };
      center =
        {
          center = ch.center;
          h0 = ch.h5;
          h1 = ch.h4;
          h2 = ch.h3;
          h3 = ch.h2;
          h4 = ch.h1;
          h5 = ch.h0;
        };
    }

  let flip_down ({ top = th; down = dh; side = sh; center = ch } : t) : t =
    {
      top =
        {
          center = sh.center;
          h0 = sh.h1;
          h1 = sh.h0;
          h2 = sh.h5;
          h3 = sh.h4;
          h4 = sh.h3;
          h5 = sh.h2;
        };
      down =
        {
          center = dh.center;
          h0 = dh.h1;
          h1 = dh.h0;
          h2 = dh.h5;
          h3 = dh.h4;
          h4 = dh.h3;
          h5 = dh.h2;
        };
      side =
        {
          center = th.center;
          h0 = th.h1;
          h1 = th.h0;
          h2 = th.h5;
          h3 = th.h4;
          h4 = th.h3;
          h5 = th.h2;
        };
      center =
        {
          center = ch.center;
          h0 = ch.h1;
          h1 = ch.h0;
          h2 = ch.h5;
          h3 = ch.h4;
          h4 = ch.h3;
          h5 = ch.h2;
        };
    }

  let flip_side ({ top = th; down = dh; side = sh; center = ch } : t) : t =
    {
      top =
        {
          center = dh.center;
          h0 = dh.h3;
          h1 = dh.h2;
          h2 = dh.h1;
          h3 = dh.h0;
          h4 = dh.h5;
          h5 = dh.h4;
        };
      down =
        {
          center = th.center;
          h0 = th.h3;
          h1 = th.h2;
          h2 = th.h1;
          h3 = th.h0;
          h4 = th.h5;
          h5 = th.h4;
        };
      side =
        {
          center = sh.center;
          h0 = sh.h3;
          h1 = sh.h2;
          h2 = sh.h1;
          h3 = sh.h0;
          h4 = sh.h5;
          h5 = sh.h4;
        };
      center =
        {
          center = ch.center;
          h0 = ch.h3;
          h1 = ch.h2;
          h2 = ch.h1;
          h3 = ch.h0;
          h4 = ch.h5;
          h5 = ch.h4;
        };
    }

  let shuffle b =
    let b' = shuffle_outer b in
    match Random.int 3 with
    | 0 -> flip_top b'
    | 1 -> flip_down b'
    | 2 -> flip_side b'
    | _ -> assert false

  let string_of_board ({ top = th; down = dh; side = sh; center = ch } : t) :
      string =
    let short = "     " in
    let med = "         " in
    let long = med ^ " " in
    short ^ String.make 1 th.h0 ^ "\n" ^ String.make 1 th.h5 ^ med
    ^ String.make 1 th.h1 ^ "\n" ^ short ^ String.make 1 th.center ^ "\n"
    ^ String.make 1 th.h4 ^ med ^ String.make 1 th.h2 ^ med
    ^ String.make 1 sh.h0 ^ "\n" ^ short ^ String.make 1 th.h3 ^ med
    ^ String.make 1 sh.h5 ^ med ^ String.make 1 sh.h1 ^ "\n" ^ long
    ^ String.make 1 ch.center ^ med ^ String.make 1 sh.center ^ "\n" ^ short
    ^ String.make 1 dh.h0 ^ med ^ String.make 1 sh.h4 ^ med
    ^ String.make 1 sh.h2 ^ "\n" ^ String.make 1 dh.h5 ^ med
    ^ String.make 1 dh.h1 ^ med ^ String.make 1 sh.h3 ^ "\n" ^ short
    ^ String.make 1 dh.center ^ "\n" ^ String.make 1 dh.h4 ^ med
    ^ String.make 1 dh.h2 ^ "\n" ^ short ^ String.make 1 dh.h3 ^ "\n"

  let print b = print_string (string_of_board b)

  let get_letters ({ top = th; down = dh; side = sh; center = ch } : t) :
      char list =
    list_hex th @ list_hex dh @ list_hex sh @ list_hex ch

  let board_of_letters (lst : char list) : t =
    assert (List.length lst = 28);
    {
      top =
        {
          center = List.nth lst 0;
          h0 = List.nth lst 1;
          h1 = List.nth lst 2;
          h2 = List.nth lst 3;
          h3 = List.nth lst 4;
          h4 = List.nth lst 5;
          h5 = List.nth lst 6;
        };
      down =
        {
          center = List.nth lst 7;
          h0 = List.nth lst 8;
          h1 = List.nth lst 9;
          h2 = List.nth lst 10;
          h3 = List.nth lst 11;
          h4 = List.nth lst 12;
          h5 = List.nth lst 13;
        };
      side =
        {
          center = List.nth lst 14;
          h0 = List.nth lst 15;
          h1 = List.nth lst 16;
          h2 = List.nth lst 17;
          h3 = List.nth lst 18;
          h4 = List.nth lst 19;
          h5 = List.nth lst 20;
        };
      center =
        {
          center = List.nth lst 21;
          h0 = List.nth lst 22;
          h1 = List.nth lst 23;
          h2 = List.nth lst 24;
          h3 = List.nth lst 25;
          h4 = List.nth lst 26;
          h5 = List.nth lst 27;
        };
    }

  let board_data ({ top = th; down = dh; side = sh; center = ch } : t) :
      (char * char list) list =
    [
      (th.center, [ th.h0; th.h1; th.h2; th.h3; th.h4; th.h5 ]);
      (dh.center, [ dh.h0; dh.h1; dh.h2; dh.h3; dh.h4; dh.h5 ]);
      (sh.center, [ sh.h0; sh.h1; sh.h2; sh.h3; sh.h4; sh.h5 ]);
      (ch.center, [ ch.h0; ch.h1; ch.h2; ch.h3; ch.h4; ch.h5 ]);
    ]
end

(*******************************************************)
(***************** HONEY COMB MODULE ********************)

module Honeycomb : BoardType = struct
  type t = hex * hex * hex * hex * hex * hex

  let rep_ok (b1, b2, b3, b4, b5, b6) =
    hex_rep_ok b1 && hex_rep_ok b2 && hex_rep_ok b3 && hex_rep_ok b4
    && hex_rep_ok b5 && hex_rep_ok b6
    && (b1.h1 = b3.h0 && b1.h1 = b5.h5)
    && b1.center = b3.h5 && b3.h1 = b5.center
    && (b1.h2 = b3.center && b1.h2 = b4.h0 && b1.h2 = b5.h4)
    && (b1.h3 = b2.h0 && b1.h3 = b3.h4 && b1.h3 = b4.h5)
    && (b3.h2 = b4.h1 && b3.h2 = b5.h3 && b3.h2 = b6.h0)
    && (b2.h1 = b3.h3 && b2.h1 = b4.center && b2.h1 = b6.h5)
    && b2.center = b4.h4 && b4.h2 = b6.center && b2.h2 = b4.h3 && b2.h2 = b6.h4

  let build input =
    ignore input;
    (* First build the middle upper hex b3 *)
    let b3 = hex_build_random () in
    (* Next fill in middle lower hex b4 - needs three additional letters *)
    let b4_extra_letters = fill_in_n [ b3.center; b3.h2; b3.h3; b3.h4 ] 3 in
    let b4 =
      {
        center = b3.h3;
        h0 = b3.center;
        h1 = b3.h2;
        h2 = List.nth b4_extra_letters 0;
        h3 = List.nth b4_extra_letters 1;
        h4 = List.nth b4_extra_letters 2;
        h5 = b3.h4;
      }
    in
    (* Next fill in left upper hex b1 - needs three additional letters *)
    let b1_extra_letters = fill_in_n [ b3.center; b3.h0; b3.h4; b3.h5 ] 3 in
    let b1 =
      {
        center = b3.h5;
        h0 = List.nth b1_extra_letters 0;
        h1 = b3.h0;
        h2 = b3.center;
        h3 = b3.h4;
        h4 = List.nth b1_extra_letters 1;
        h5 = List.nth b1_extra_letters 2;
      }
    in
    (* Next fill in left lower hex b2 - needs three additional letters *)
    let b2_extra_letters = fill_in_n [ b4.center; b4.h3; b4.h4; b4.h5 ] 3 in
    let b2 =
      {
        center = b4.h4;
        h0 = b4.h5;
        h1 = b4.center;
        h2 = b4.h3;
        h3 = List.nth b2_extra_letters 0;
        h4 = List.nth b2_extra_letters 1;
        h5 = List.nth b2_extra_letters 2;
      }
    in
    (* Next fill in right upper hex b5 - needs three additional letters *)
    let b5_extra_letters = fill_in_n [ b3.center; b3.h0; b3.h1; b3.h2 ] 3 in
    let b5 =
      {
        center = b3.h1;
        h0 = List.nth b5_extra_letters 0;
        h1 = List.nth b5_extra_letters 1;
        h2 = List.nth b5_extra_letters 2;
        h3 = b3.h2;
        h4 = b3.center;
        h5 = b3.h0;
      }
    in
    (* Next fill in right lower hex b6 - needs three additional letters *)
    let b6_extra_letters = fill_in_n [ b4.center; b4.h1; b4.h2; b4.h3 ] 3 in
    let b6 =
      {
        center = b4.h2;
        h0 = b4.h1;
        h1 = List.nth b6_extra_letters 0;
        h2 = List.nth b6_extra_letters 1;
        h3 = List.nth b6_extra_letters 2;
        h4 = b4.h3;
        h5 = b4.center;
      }
    in
    (b1, b2, b3, b4, b5, b6)

  let contains (word : string) ((b1, b2, b3, b4, b5, b6) : t) : bool =
    let word_upper = String.uppercase_ascii word in
    String.length word >= 4
    && (hex_contains b1 word_upper || hex_contains b2 word_upper
      || hex_contains b3 word_upper || hex_contains b4 word_upper
      || hex_contains b5 word_upper || hex_contains b6 word_upper)

  let is_pangram (word : string) ((b1, b2, b3, b4, b5, b6) : t) : bool =
    let word_upper = String.uppercase_ascii word in
    hex_is_pangram b1 word_upper
    || hex_is_pangram b2 word_upper
    || hex_is_pangram b3 word_upper
    || hex_is_pangram b4 word_upper
    || hex_is_pangram b5 word_upper
    || hex_is_pangram b6 word_upper

  let shuffle b = b

  let string_of_board ((b1, b2, b3, b4, b5, b6) : t) : string =
    let short = "     " in
    let long = "         " in
    short ^ String.make 1 b1.h0 ^ long ^ String.make 1 b5.h0 ^ "\n"
    ^ String.make 1 b1.h5 ^ long ^ String.make 1 b1.h1 ^ long
    ^ String.make 1 b5.h1 ^ "\n" ^ short ^ String.make 1 b1.center ^ long
    ^ String.make 1 b3.h1 ^ "\n" ^ String.make 1 b1.h4 ^ long
    ^ String.make 1 b1.h2 ^ long ^ String.make 1 b5.h2 ^ "\n" ^ short
    ^ String.make 1 b1.h3 ^ long ^ String.make 1 b3.h2 ^ "\n"
    ^ String.make 1 b2.h5 ^ long ^ String.make 1 b2.h1 ^ long
    ^ String.make 1 b6.h1 ^ "\n" ^ short ^ String.make 1 b2.center ^ long
    ^ String.make 1 b4.h2 ^ "\n" ^ String.make 1 b2.h4 ^ long
    ^ String.make 1 b2.h2 ^ long ^ String.make 1 b6.h2 ^ "\n" ^ short
    ^ String.make 1 b2.h3 ^ long ^ String.make 1 b6.h3 ^ "\n"

  let print b = print_string (string_of_board b)

  let get_letters ((b1, b2, b3, b4, b5, b6) : t) : char list =
    list_hex b1 @ list_hex b2 @ list_hex b3 @ list_hex b4 @ list_hex b5
    @ list_hex b6

  let board_of_letters (lst : char list) : t =
    assert (List.length lst = 42);
    ( {
        center = List.nth lst 0;
        h0 = List.nth lst 1;
        h1 = List.nth lst 2;
        h2 = List.nth lst 3;
        h3 = List.nth lst 4;
        h4 = List.nth lst 5;
        h5 = List.nth lst 6;
      },
      {
        center = List.nth lst 7;
        h0 = List.nth lst 8;
        h1 = List.nth lst 9;
        h2 = List.nth lst 10;
        h3 = List.nth lst 11;
        h4 = List.nth lst 12;
        h5 = List.nth lst 13;
      },
      {
        center = List.nth lst 14;
        h0 = List.nth lst 15;
        h1 = List.nth lst 16;
        h2 = List.nth lst 17;
        h3 = List.nth lst 18;
        h4 = List.nth lst 19;
        h5 = List.nth lst 20;
      },
      {
        center = List.nth lst 21;
        h0 = List.nth lst 22;
        h1 = List.nth lst 23;
        h2 = List.nth lst 24;
        h3 = List.nth lst 25;
        h4 = List.nth lst 26;
        h5 = List.nth lst 27;
      },
      {
        center = List.nth lst 28;
        h0 = List.nth lst 29;
        h1 = List.nth lst 30;
        h2 = List.nth lst 31;
        h3 = List.nth lst 32;
        h4 = List.nth lst 33;
        h5 = List.nth lst 34;
      },
      {
        center = List.nth lst 35;
        h0 = List.nth lst 36;
        h1 = List.nth lst 37;
        h2 = List.nth lst 38;
        h3 = List.nth lst 39;
        h4 = List.nth lst 40;
        h5 = List.nth lst 41;
      } )

  let board_data ((b1, b2, b3, b4, b5, b6) : t) : (char * char list) list =
    [
      (b1.center, [ b1.h0; b1.h1; b1.h2; b1.h3; b1.h4; b1.h5 ]);
      (b2.center, [ b2.h0; b2.h1; b2.h2; b2.h3; b2.h4; b2.h5 ]);
      (b3.center, [ b3.h0; b3.h1; b3.h2; b3.h3; b3.h4; b3.h5 ]);
      (b4.center, [ b4.h0; b4.h1; b4.h2; b4.h3; b4.h4; b4.h5 ]);
      (b5.center, [ b5.h0; b5.h1; b5.h2; b5.h3; b5.h4; b5.h5 ]);
      (b6.center, [ b6.h0; b6.h1; b6.h2; b6.h3; b6.h4; b6.h5 ]);
    ]
end
