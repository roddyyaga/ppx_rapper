(********************************************************************************)
(** {1 Public functions and values}                                             *)

(********************************************************************************)

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
