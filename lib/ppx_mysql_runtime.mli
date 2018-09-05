(********************************************************************************)

(** {1 Public functions and values}                                             *)

(********************************************************************************)

val identity : 'a -> 'a

val map_option : ('a -> 'b) -> 'a option -> 'b option

val get_option : 'a option -> 'a

module Stdlib : sig
  module String : sig
    include module type of struct
        include String
    end
  end

  module Array : sig
    include module type of struct
        include Array
    end
  end

  module List : sig
    include module type of struct
        include List
    end
  end

  val ( = ) : 'a -> 'a -> bool
end
