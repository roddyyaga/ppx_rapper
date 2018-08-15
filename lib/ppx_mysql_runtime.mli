(********************************************************************************)
(* Ppx_mysql_runtime.mli                                                        *)
(********************************************************************************)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val identity: 'a -> 'a

val map_option: ('a -> 'b) -> 'a option -> 'b option

val get_option: 'a option -> 'a

val select_one: 'a list -> ('a, [> `Found_none_expected_one | `Found_many_expected_one ]) result

val select_opt: 'a list -> ('a option, [> `Found_many_expected_maybe_one ]) result

val select_all: 'a list -> ('a list, unit) result

val execute: 'a list -> (unit, [> `Found_nonzero_expected_zero ]) result
