module type PPX_CONTEXT_ARG = sig
  module IO : sig
    type 'a t

    val return : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Prepared : sig
    type dbh

    type stmt

    type stmt_result

    type error

    val create : dbh -> string -> (stmt, error) result IO.t

    val execute_null : stmt -> string option array -> (stmt_result, error) result IO.t

    val fetch : stmt_result -> (string option array option, error) result IO.t

    val close : stmt -> (unit, error) result IO.t
  end
end

module type PPX_CONTEXT = sig
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

  module Prepared : sig
    type dbh

    type stmt

    type stmt_result

    type error

    type wrapped_error = [`Mysql_error of error]

    val create : dbh -> string -> (stmt, [> wrapped_error]) result IO.t

    val execute_null 
      : stmt -> string option array -> (stmt_result, [> wrapped_error]) result IO.t

    val fetch 
      : stmt_result -> (string option array option, [> wrapped_error]) result IO.t

    val close : stmt -> (unit, [> wrapped_error]) result IO.t

    val with_stmt 
      :  dbh
      -> string
      -> (stmt -> ('a, ([> wrapped_error] as 'e)) result IO.t)
      -> ('a, 'e) result IO.t
  end
end

module Make_context (M : PPX_CONTEXT_ARG) :
  PPX_CONTEXT
  with type 'a IO.t = 'a M.IO.t
   and type Prepared.dbh = M.Prepared.dbh
   and type Prepared.error = M.Prepared.error = struct
  module IO = struct
    include M.IO

    let ( >>= ) = bind
  end

  module IO_result = struct
    type ('a, 'e) t = ('a, 'e) result IO.t

    let bind x f =
      IO.bind x (function
          | Ok v ->
              f v
          | Error _ as e ->
              IO.return e )


    let ( >>= ) = bind
  end

  module Prepared = struct
    type dbh = M.Prepared.dbh

    type stmt = M.Prepared.stmt

    type stmt_result = M.Prepared.stmt_result

    type error = M.Prepared.error

    type wrapped_error = [`Mysql_error of error]

    let wrap f x =
      IO.bind (f x)
      @@ function
      | Ok _ as ok ->
          IO.return ok
      | Error err ->
          IO.return @@ Error (`Mysql_error err)


    let create dbd sql = wrap (M.Prepared.create dbd) sql

    let close stmt = wrap M.Prepared.close stmt

    let execute_null stmt args = wrap (M.Prepared.execute_null stmt) args

    let fetch stmt_res = wrap M.Prepared.fetch stmt_res

    let with_stmt dbh sql f =
      IO_result.bind (create dbh sql)
      @@ fun stmt ->
      IO.bind (f stmt)
      @@ fun res ->
      IO.bind (close stmt)
      @@ function
      | Ok () ->
          IO.return res
      | Error _ as e ->
          IO.return e
  end
end

module Stdlib = struct
  module Array = Array
  module List = List

  module Option = struct
    type 'a t = 'a option =
      | None
      | Some of 'a

    let map f = function
      | Some x ->
          Some (f x)
      | None ->
          None


    let get = function
      | Some x ->
          x
      | None ->
          invalid_arg "Option.get"
  end

  module Result = struct
    type ('a, 'e) t = ('a, 'e) result =
      | Ok of 'a
      | Error of 'e

    let bind r f =
      match r with
      | Ok x ->
          f x
      | Error _ as e ->
          e


    let ( >>= ) = bind
  end

  let ( = ) = ( = )
end

exception Deserialization_error of string * string

let wrap_deserializer f x =
  try f x with Failure msg -> raise (Deserialization_error (msg, x))


let identity x = x

let int_of_string_exn = wrap_deserializer int_of_string

let int32_of_string_exn = wrap_deserializer Int32.of_string

let int64_of_string_exn = wrap_deserializer Int64.of_string
