(* This module is required to keep references to the OCaml operators in the stdlib *)
module Stdlib = struct
  module Array = Array

  module Option = struct
    let map f = function
      | Some x -> Some (f x)
      | None -> None

    let get = function
      | Some x -> x
      | None -> invalid_arg "Option.get"
  end

  let ( = ) = ( = )
end

let identity x = x

