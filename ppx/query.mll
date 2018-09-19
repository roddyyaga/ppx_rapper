{
open Ppx_mysql_runtime.Stdlib

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

let build_param spec opt name =
  let open Result in
  begin match spec with
  | "int" -> Ok ("int", ("Ppx_mysql_runtime", "int_of_string"), ("Pervasives", "string_of_int"))
  | "int32" -> Ok ("int32", ("Ppx_mysql_runtime", "int32_of_string"), ("Int32", "to_string"))
  | "int64" -> Ok ("int64", ("Ppx_mysql_runtime", "int64_of_string"), ("Int64", "to_string"))
  | "bool" -> Ok ("bool", ("Ppx_mysql_runtime", "bool_of_string"), ("Pervasives", "string_of_bool"))
  | "string" -> Ok ("string", ("Ppx_mysql_runtime", "identity"), ("Ppx_mysql_runtime", "identity"))
  | spec -> Error (`Unknown_type_spec spec)
  end >>= fun (typ, of_string, to_string) ->
  Ok {typ; opt = (opt = "?"); name; of_string; to_string}
}

let escape = '\\'
let squot = '\''
let dquot = '"'
let quot = squot | dquot
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let underscore = '_'
let ident = (lower | underscore) (lower | upper | underscore | digit)*
let spec = (lower | upper | underscore | digit)+

rule main_parser buf acc_in acc_out = parse
  | quot as delim
    {Buffer.add_char buf delim;
    quotation_parser buf acc_in acc_out delim lexbuf}
  | '%' (spec as spec) ('?'? as opt) '{' ([^ '}' ]+ as name) '}'
    {let open Result in
    check_ident name (Lexing.from_string name) >>= fun () ->
    build_param spec opt name >>= fun in_param ->
    Buffer.add_char buf '?';
    main_parser buf (in_param :: acc_in) acc_out lexbuf}
  | '@' (spec as spec) ('?'? as opt) '{' ([^ '}' ]+ as name) '}'
    {let open Result in
    build_param spec opt name >>= fun out_param ->
    Buffer.add_string buf name;
    main_parser buf acc_in (out_param :: acc_out) lexbuf}
  | escape eof
    {Error `Escape_at_end}
  | escape _ as str
    {Buffer.add_string buf str;
    main_parser buf acc_in acc_out lexbuf}
  | _ as chr
    {Buffer.add_char buf chr;
    main_parser buf acc_in acc_out lexbuf}
  | eof
    {Ok {query = Buffer.contents buf; in_params = List.rev acc_in; out_params = List.rev acc_out}}

and quotation_parser buf acc_in acc_out delim = parse
  | escape eof
    {Error `Escape_at_end}
  | escape _ as str
    {Buffer.add_string buf str;
    quotation_parser buf acc_in acc_out delim lexbuf}
  | squot squot as str
    {Buffer.add_string buf str;
    quotation_parser buf acc_in acc_out delim lexbuf}
  | dquot dquot as str
    {Buffer.add_string buf str;
    quotation_parser buf acc_in acc_out delim lexbuf}
  | quot as chr
    {Buffer.add_char buf chr;
    if delim = chr
    then main_parser buf acc_in acc_out lexbuf
    else quotation_parser buf acc_in acc_out delim lexbuf}
  | _ as chr
    {Buffer.add_char buf chr;
    quotation_parser buf acc_in acc_out delim lexbuf}
  | eof
    {Error `Unterminated_string}

and check_ident name = parse
  | ident {Ok ()}
  | _ | eof {Error (`Bad_identifier name)}

{
let parse query =
  let lexbuf = Lexing.from_string query in
  let buf = Buffer.create (String.length query) in
  main_parser buf [] [] lexbuf

let explain_parse_error = function
  | `Bad_identifier str ->
    Printf.sprintf "'%s' is not a valid OCaml variable identifier" str
  | `Unknown_type_spec spec ->
    Printf.sprintf "Unknown type specification '%s'" spec
  | `Unterminated_string ->
    "The query contains an unterminated string"
  | `Escape_at_end ->
    "The last character of the query cannot be an escape character"
}
