(********************************************************************************)
(*  Test_ppx_mysql_lib.ml                                                       *)
(********************************************************************************)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type param = Ppx_mysql_lib.param =
    {
    typ: string;
    opt: bool;
    name: string;
    of_string: string;
    to_string: string;
    } [@@deriving eq, show]

type parsed_query = Ppx_mysql_lib.parsed_query =
    {
    query: string;
    in_params: param list;
    out_params: param list;
    } [@@deriving eq, show]

type parse_error =
    [ `Bad_param
    | `Escape_at_end
    | `Unknown_mysql_type of string
    | `Unterminated_string
    ] [@@deriving eq, show]


(********************************************************************************)
(** {1 TESTABLE modules}                                                        *)
(********************************************************************************)

let param_mod = Alcotest.testable pp_param equal_param

let parsed_query_mod = Alcotest.testable pp_parsed_query equal_parsed_query

let parse_error_mod = Alcotest.testable pp_parse_error equal_parse_error


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let query_1 = "SELECT true"
let parsed_query_1 =
    {
    query = "SELECT true";
    in_params = [];
    out_params = [];
    }

let query_2 = "SELECT @INT{id}, @TEXT{name}, @TEXT?{phone} FROM users"
let parsed_query_2 =
    {
    query = "SELECT id, name, phone FROM users";
    in_params = [];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        ];
    }

let query_3 = "INSERT INTO users (id, name, phone) VALUES (%INT{id}, %TEXT{name}, %TEXT?{phone})"
let parsed_query_3 =
    {
    query = "INSERT INTO users (id, name, phone) VALUES (?, ?, ?)";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        ];
    out_params = [];
    }

let query_4 = "SELECT @INT{id}, @TEXT{name}, @TEXT?{phone} FROM users WHERE id = %INT{id} OR name = %TEXT{name} OR PHONE = %TEXT?{phone}"
let parsed_query_4 =
    {
    query = "SELECT id, name, phone FROM users WHERE id = ? OR name = ? OR PHONE = ?";
    in_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        ];
    out_params =
        [
        {typ = "int64"; opt = false; name = "id"; of_string = "Int64.of_string"; to_string = "Int64.to_string"};
        {typ = "string"; opt = false; name = "name"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        {typ = "string"; opt = true; name = "phone"; of_string = "Ppx_mysql_lib.identity"; to_string = "Ppx_mysql_lib.identity"};
        ];
    }

let test_parse_query () =
    let run desc query parsed_query =
        Alcotest.(check (result parsed_query_mod parse_error_mod) desc parsed_query (Ppx_mysql_lib.parse_query query)) in
    run "query_1" query_1 (Ok parsed_query_1);
    run "query_2" query_2 (Ok parsed_query_2);
    run "query_3" query_3 (Ok parsed_query_3);
    run "query_4" query_4 (Ok parsed_query_4)

let testset =
    [
    ("parse_query", `Quick, test_parse_query);
    ]


(********************************************************************************)
(** {1 Main}                                                                    *)
(********************************************************************************)

let () = Alcotest.run "Ppx_mysql_lib module"
    [
    ("Ppx_mysql_lib", testset);
    ]
