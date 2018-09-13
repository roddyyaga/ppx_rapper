module type PPX_CONTEXT_ARG =
sig
  module IO : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Prepared : sig
    type dbh
    type stmt
    type stmt_result
    type error = [ `Mysql_exception of exn ]

    val create : dbh -> string -> (stmt, [> error ]) result IO.t
    val execute_null : stmt -> string option array -> (stmt_result, [> error ]) result IO.t
    val fetch : stmt_result -> (string option array option, [> error ]) result IO.t
    val close : stmt -> (unit, [> error ]) result IO.t
  end
end

module type PPX_CONTEXT =
sig
  module IO : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module IO_result : sig
    type ('a, 'e) t = ('a, 'e) result IO.t
    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  module Prepared: sig
    type dbh
    type stmt
    type stmt_result
    type error = [ `Mysql_exception of exn ]

    val create : dbh -> string -> (stmt, [> error ]) result IO.t
    val execute_null : stmt -> string option array -> (stmt_result, [> error ]) result IO.t
    val fetch : stmt_result -> (string option array option, [> error ]) result IO.t
    val close : stmt -> (unit, [> error ]) result IO.t
    val with_stmt : dbh -> string -> (stmt -> ('a, [> error ] as 'e) result IO.t) -> ('a, 'e) result IO.t
  end
end

module Make_context : functor (M : PPX_CONTEXT_ARG) -> PPX_CONTEXT with type 'a IO.t = 'a M.IO.t and type Prepared.dbh = M.Prepared.dbh

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
