(** {1 Type definitions} *)

type param =
  { typ : string
  ; opt : bool
  ; name : string
  ; of_string : string * string
  ; to_string : string * string }

type parsed_query =
  { query : string
  ; in_params : param list
  ; out_params : param list }

type parse_error =
  [ `Bad_identifier of string
  | `Unknown_type_spec of string
  | `Unterminated_string
  | `Escape_at_end ]

(** {1 Public functions and values} *)

val parse : string -> (parsed_query, [> parse_error]) result

val explain_parse_error : [< parse_error] -> string
