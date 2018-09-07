(* This module is required to keep references to the OCaml operators in the stdlib *)
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

let identity x = x
