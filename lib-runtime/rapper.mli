module type CUSTOM = sig
  type t

  val t : t Caqti_type.t
end

val load_many :
  ('row -> 'parent) * ('parent -> 'key) ->
  (('row -> 'child) * ('parent -> 'child list -> 'parent)) list ->
  'row list ->
  'parent list

module Internal : sig
  module Dynparam : sig
    type t = Pack : 'a Caqti_type.t * 'a -> t

    val empty : t

    val add : 'a Caqti_type.t -> 'a -> t -> t
  end
end

module type IO = sig
  type +'a t

  val return : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  module Stream : Caqti_stream.S with type 'a future := 'a t
end

module type CONTEXT = sig
  module Rapper_io : sig
    type +'a future

    val map : ('a -> 'b) -> 'a future -> 'b future

    val fail : 'e -> ('a, 'e) result future

    module Stream : Caqti_stream.S with type 'a future := 'a future

    module type CONNECTION =
      Caqti_connection_sig.S
        with type 'a future := 'a future
         and type ('a, 'err) stream := ('a, 'err) Stream.t
  end
end

module Make_context (Io : IO) :
  CONTEXT
    with type 'a Rapper_io.future := 'a Io.t
     and module Rapper_io.Stream = Io.Stream
