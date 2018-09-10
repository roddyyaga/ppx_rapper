module type PPX_CONTEXT =
sig
  type dbh

  module IO : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Prepared: sig
    type stmt
    type stmt_result

    val create : dbh -> string -> stmt IO.t
    val execute_null : stmt -> string option array -> stmt_result IO.t
    val fetch : stmt_result -> string option array option IO.t
    val close : stmt -> unit IO.t
  end
end

module Stdlib : sig
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

  module Option : sig
    type 'a t = 'a option =
      | None
      | Some of 'a

    val map : ('a -> 'b) -> 'a t -> 'b t
    val get : 'a t -> 'a
  end

  module Result : sig
    type ('a, 'e) t = ('a, 'e) result =
      | Ok of 'a
      | Error of 'e

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  val ( = ) : 'a -> 'a -> bool
end

exception Deserialization_error of string * string

val identity : 'a -> 'a

val int_of_string_exn : string -> int
val int32_of_string_exn : string -> int32
val int64_of_string_exn : string -> int64
