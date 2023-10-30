(* The signature of a mutable word dictionary. That is, a collection of words
   that are non-repeating and in lowercase form*)
type t
(** Type representing the data in the dictionary. *)

val to_list : t -> string list
(** [build dict] takes a dictionary [dict] and returns its elements as a list of
    words sorted in alphabetical order*)

val of_list : string list -> t
(** [build lst] takes a list of string words and makes them into a dictionary of
    lowercase words*)

val contains_opt : string -> t -> string option
(** [contains str t] returns [Some lowercase_str] if [str] is contained inside
    dictionary [t] regardless of character case. Otherwise returns [None] *)

val insert_opt : string -> t -> string option
(** [insert str t] inserts the word [str] in lowercase into a dictionary [t] and
    returns [Some lowercase_str]. If the dicitonary already contains [str]
    changes nothing and returns [None]*)

val remove_opt : string -> t -> string option
(** [remove str t] removes the word [str] from a dictionary [t] regardless of
    case and returns the removed element as [Some lowercase_str]. If a
    dictionary does not contain the word, it makes no changes and returns [None]*)
