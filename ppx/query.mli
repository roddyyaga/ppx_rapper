(* From https://github.com/issuu/ppx_mysql
 * Under Apache 2.0 license *)
(** {1 Type definitions} *)

type param = {
  typ : string option * string;
  opt : bool;
  name : string;
}

type list_params = {
  subsql : string;
  string_index : int;
  param_index : int;
  params : param list
}

type parsed_query = {
  sql : string;
  in_params : param list;
  out_params : param list;
  list_params : list_params option
}

type parse_error =
  [ `Bad_identifier of string
  | `Unknown_type_spec of string
  | `Empty_list_params
  | `Multiple_lists_not_supported
  | `Nested_list
  | `Optional_list
  | `Out_params_in_list
  | `Unterminated_list
  | `Unterminated_string
  | `Unterminated_bracket
  | `Escape_at_end ]

type conflict_error = [`Conflicting_spec of string]

type error =
  [ parse_error
  | conflict_error ]

(** {1 Public functions and values} *)

val parse : string -> (parsed_query, [> parse_error]) result

val remove_duplicates : param list -> (param list, [> conflict_error]) result

val explain_error : [< error] -> string
