module type CUSTOM = sig
  type t

  val t : t Caqti_type.t
end

module Dynparam : sig
  type t = Pack : 'a Caqti_type.t * 'a -> t

  val empty : t

  val add : 'a Caqti_type.t -> 'a -> t -> t
end

