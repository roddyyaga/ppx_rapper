(********************************************************************************)
(* Ppx_mysql_runtime.ml                                                         *)
(********************************************************************************)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let identity x = x

let map_option f = function
    | Some x -> Some (f x)
    | None   -> None

let get_option = function
    | Some x -> x
    | None   -> assert false (* FIXME *)

let select_one = function
    | [x] -> Ok x
    | []  -> Error `Found_none_expected_one
    | _   -> Error `Found_many_expected_one

let select_opt = function
    | [x] -> Ok (Some x)
    | []  -> Ok None
    | _   -> Error `Found_many_expected_maybe_one

let select_all xs =
    Ok xs

let execute = function
    | [] -> Ok ()
    | _  -> Error `Found_nonzero_expected_zero
