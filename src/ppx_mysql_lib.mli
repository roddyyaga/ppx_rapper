type param =
    {
    typ : string;
    opt : bool;
    name : string;
    of_string : string;
    to_string : string;
    }

type parse_error =
    [ `Bad_param
    | `Escape_at_end
    | `Unknown_mysql_type of string
    | `Unterminated_string
    ]

val identity: 'a -> 'a

val map_option: ('a -> 'b) -> 'a option -> 'b option

val get_option: 'a option -> 'a

val parse_query: string -> (string * param list * param list, [> parse_error ]) result
