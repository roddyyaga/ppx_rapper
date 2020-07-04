(* A modified version of query.mll from https://github.com/issuu/ppx_mysql *)
{
module Result = struct
    type ('a, 'e) t = ('a, 'e) result =
      | Ok of 'a
      | Error of 'e

    let bind r f =
      match r with
      | Ok x -> f x
      | Error _ as e -> e

    let ( >>= ) = bind
end

module Param_dict = Map.Make (String)

type param =
  { typ : string option * string
  ; opt : bool
  ; name : string
  }

type list_params =
  { subsql : string
  ; string_index : int
  ; param_index : int
  ; params : param list }

type parsed_query =
  { sql : string
  ; in_params : param list
  ; out_params : param list
  ; list_params : list_params option }

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

type conflict_error =
  [ `Conflicting_spec of string ]

type error = [ parse_error | conflict_error ]

type list_status =
  | Absent
  | Ongoing
  | Complete of list_params

(* TODO - factor to remove stuff that isn't needed *)
let build_param spec opt name =
  let open Result in
  begin match spec with
  | "int" ->
      Ok (None, "int")
  | "int32" ->
      Ok (None, "int32")
  | "int64" ->
      Ok (None, "int64")
  | "bool" ->
      Ok (None, "bool")
  | "string" ->
      Ok (None, "string")
  | "octets" ->
      Ok (None, "octets")
  | "pdate" ->
      Ok (None, "pdate")
  | "ptime" ->
      Ok (None, "ptime")
  | "ptime_span" ->
      Ok (None, "ptime_span")
  | "float" ->
      Ok (None, "float")
  | "cdate" ->
      Ok (Some "Caqti_type_calendar", "cdate")
  | "ctime" ->
      Ok (Some "Caqti_type_calendar", "ctime")
  | module_name when String.length module_name > 0 && module_name.[0] >= 'A' && module_name.[0] <= 'Z' ->
      Ok (Some module_name, "t")
  | spec ->
      Error (`Unknown_type_spec spec)
  end >>= fun typ ->
  Ok {typ; opt = (opt = "?"); name}
}

let escape = '\\'
let squot = '\''
let dquot = '"'
let quot = squot | dquot
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let underscore = '_'
let dot = '.'
let ident = (lower | underscore) (lower | upper | underscore | digit)*
let spec' = (lower | upper | underscore | digit)
let spec = (spec' (dot spec')?)+

rule main_parser buf acc_in acc_out list_status = parse
  | quot as delim
      {Buffer.add_char buf delim;
      quotation_parser buf acc_in acc_out list_status delim lexbuf}
  | '%' (spec as spec) ('?'? as opt) '{'
      {match spec, opt with
      | "list", "" ->
          begin match list_status with
          | Complete _ ->
            Error `Multiple_lists_not_supported
          | Ongoing ->
            Error `Nested_list
          | Absent ->
              let open Result in
              let string_index = Buffer.length buf in
              let sub_buf = Buffer.create 64 in
              main_parser sub_buf [] [] Ongoing lexbuf >>= function
              | {sql = _; in_params = []; out_params = []; list_params = _} ->
                  Error `Empty_list_params
              | {sql = subsql; in_params = params; out_params = []; list_params = _} ->
                  let param_index = List.length acc_in in
                  let list_status = Complete {subsql; string_index; param_index; params} in
                  main_parser buf acc_in acc_out list_status lexbuf
              | _ ->
                  Error `Out_params_in_list
          end
      | "list", "?" ->
          Error `Optional_list
      | spec, opt ->
          let open Result in
          ident_parser lexbuf >>= fun name ->
          build_param spec opt name >>= fun in_param ->
          Buffer.add_char buf '?';
          main_parser buf (in_param :: acc_in) acc_out list_status lexbuf}
  | '@' (spec as spec) ('?'? as opt) '{'
      {let open Result in
      out_param_parser lexbuf >>= fun name ->
      build_param spec opt name >>= fun out_param ->
      Buffer.add_string buf name;
      main_parser buf acc_in (out_param :: acc_out) list_status lexbuf}
  | escape eof
      {Error `Escape_at_end}
  | escape _ as str
      {Buffer.add_string buf str;
      main_parser buf acc_in acc_out list_status lexbuf}
  | '}'
      {match list_status with
      | Ongoing ->
          let sql = Buffer.contents buf in
          let in_params = List.rev acc_in in
          let out_params = List.rev acc_out in
          Ok {sql; in_params; out_params; list_params = None}
      | Absent | Complete _ ->
          Buffer.add_char buf '}';
          main_parser buf acc_in acc_out list_status lexbuf}
  | _ as chr
      {Buffer.add_char buf chr;
      main_parser buf acc_in acc_out list_status lexbuf}
  | eof
      {let sql = Buffer.contents buf in
      let in_params = List.rev acc_in in
      let out_params = List.rev acc_out in
      match list_status with
      | Ongoing ->
          Error `Unterminated_list
      | Absent ->
          Ok {sql; in_params; out_params; list_params = None}
      | Complete nested ->
          Ok {sql; in_params; out_params; list_params = Some nested}}

and quotation_parser buf acc_in acc_out list_status delim = parse
  | escape eof
      {Error `Escape_at_end}
  | escape _ as str
      {Buffer.add_string buf str;
      quotation_parser buf acc_in acc_out list_status delim lexbuf}
  | squot squot as str
      {Buffer.add_string buf str;
      quotation_parser buf acc_in acc_out list_status delim lexbuf}
  | dquot dquot as str
      {Buffer.add_string buf str;
      quotation_parser buf acc_in acc_out list_status delim lexbuf}
  | quot as chr
      {Buffer.add_char buf chr;
      if delim = chr
      then main_parser buf acc_in acc_out list_status lexbuf
      else quotation_parser buf acc_in acc_out list_status delim lexbuf}
  | _ as chr
      {Buffer.add_char buf chr;
      quotation_parser buf acc_in acc_out list_status delim lexbuf}
  | eof
      {Error `Unterminated_string}

and ident_parser = parse
  | (ident as ident) '}'
      {Ok ident}
  | ([^ '}' ]+ as etc) '}'
      {Error (`Bad_identifier etc)}
  | _
      {Error `Unterminated_bracket}

and out_param_parser = parse
  | ([^ '}' ]+ as name) '}'
      {Ok name}
  | _
      {Error `Unterminated_bracket}

{
let parse query =
  let lexbuf = Lexing.from_string query in
  let buf = Buffer.create (String.length query) in
  main_parser buf [] [] Absent lexbuf

let remove_duplicates params =
  let rec loop dict accum = function
    | [] ->
        Ok (List.rev accum)
    | {name; typ; opt; _} as hd :: tl ->
        match Param_dict.find_opt name dict with
        | None ->
            let dict = Param_dict.add name hd dict in
            let accum = hd :: accum in
            loop dict accum tl
        | Some el when el.typ = typ && el.opt = opt ->
            loop dict accum tl
        | Some _el ->
            Error (`Conflicting_spec name)
  in
  loop Param_dict.empty [] params

let explain_error = function
  | `Bad_identifier str ->
    Printf.sprintf "'%s' is not a valid OCaml variable identifier" str
  | `Unknown_type_spec spec ->
    Printf.sprintf "Unknown type specification '%s'" spec
  | `Empty_list_params ->
    "Lists must have at least one parameter"
  | `Multiple_lists_not_supported ->
    "The query contains multiple lists. Multiple lists are not supported"
  | `Nested_list ->
    "The query contains a nested list parameter"
  | `Optional_list ->
    "Optional lists are not supported (why would you do this?)"
  | `Out_params_in_list ->
    "Contents of list parameter may not contain an output parameter"
  | `Unterminated_list ->
    "The query contains an unterminated list parameter"
  | `Unterminated_string ->
    "The query contains an unterminated string"
  | `Unterminated_bracket ->
    "The query contains an unterminated bracket"
  | `Escape_at_end ->
    "The last character of the query cannot be an escape character"
  | `Conflicting_spec name ->
    Printf.sprintf "Input parameter '%s' appears multiple times with conflicting specifications" name
}
