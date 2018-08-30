(********************************************************************************)
(** {1 Public functions and values}                                             *)

(********************************************************************************)

(* This module is required to keep references to the OCaml operators in the stdlib *)
module Stdlib = struct
  module Array = Array
  module String = String
  module List = List

  let ( = ) = ( = )
end

let identity x = x

let map_option f = function
  | Some x ->
      Some (f x)
  | None ->
      None


let get_option = function
  | Some x ->
      x
  | None ->
      (* FIXME *)
      assert false
