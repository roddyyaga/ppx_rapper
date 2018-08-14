(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type param =
    {
    typ : string;
    opt : bool;
    name : string;
    of_string : string;
    to_string : string;
    }

type parsed_query =
    {
    query: string;
    in_params: param list;
    out_params: param list;
    }

type parse_error =
    [ `Bad_param of int
    | `Escape_at_end
    | `Unknown_mysql_type of string
    | `Unterminated_string
    ]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val identity: 'a -> 'a

val map_option: ('a -> 'b) -> 'a option -> 'b option

val get_option: 'a option -> 'a

val parse_query: string -> (parsed_query, [> parse_error ]) result

val explain_parse_error: parse_error -> string

val select_one: 'a list -> ('a, [> `Found_none_expected_one | `Found_many_expected_one ]) result

val select_opt: 'a list -> ('a option, [> `Found_many_expected_maybe_one ]) result

val select_all: 'a list -> ('a list, unit) result

val execute: 'a list -> (unit, [> `Found_nonzero_expected_zero ]) result
