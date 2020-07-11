module type CUSTOM = sig
  type t

  val t : t Caqti_type.t
end

module Dynparam : sig
  type t = Pack : 'a Caqti_type.t * 'a -> t

  val empty : t

  val add : 'a Caqti_type.t -> 'a -> t -> t
end

module Rapper : sig
  val load_many :
    ('row -> 'parent) * ('parent -> 'key) ->
    (('row -> 'child) * ('parent -> 'child list -> 'parent)) list ->
    'row list ->
    'parent list
end
