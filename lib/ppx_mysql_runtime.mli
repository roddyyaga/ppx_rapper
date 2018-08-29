(********************************************************************************)

(** {1 Public functions and values}                                             *)

(********************************************************************************)

val identity : 'a -> 'a

val map_option : ('a -> 'b) -> 'a option -> 'b option

val get_option : 'a option -> 'a
