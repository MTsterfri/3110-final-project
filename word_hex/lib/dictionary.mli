(** The signature of a word dictionary. That is, a collection of words that are
    non-repeating and in lowercase form*)
module type DictionaryType = sig
  type t
  (** Type representing the data in the dictionary. *)

  val to_list : t -> string list
  (** [build dict] takes a dictionary [dict] and returns its elements as a list
      of words sorted in alphabetical order*)

  val of_list : string list -> t
  (** [build lst] takes a list of string words and makes them into a dictionary
      of lowercase words*)

  val contains : string -> t -> bool
  (** [contains str t] returns whether or not a word [str] is contained within
      the dictionary [t] regardless of character case*)

  val insert : string -> t -> unit
  (** [insert str t] inserts the word [str] in lowercase into a dictionary [t].
      If the dicitonary already contains [str] returns the unchanged dictionary*)

  val remove : string -> t -> unit
  (** [remove str t] removes the word [str] from a dictionary [t] regardless of
      case. If a dictionary does not contain the word, it returns the original
      dictionary*)
end

module HashDict : DictionaryType
(** Dictionary such that words are stored in a HashMap in order to improve
    efficiency. *)
