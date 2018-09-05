(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val identity : 'a -> 'a

module Stdlib : sig
  module Array : sig
    include module type of struct
        include Array
    end
  end

  module Option : sig
    val map : ('a -> 'b) -> 'a option -> 'b option
    val get : 'a option -> 'a
  end

  val ( = ) : 'a -> 'a -> bool
end
