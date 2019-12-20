open Core
open Ppxlib
module Buildef = Ast_builder.Default

(** Handle 'record_in' etc. in [%rapper "SELECT * FROM USERS" record_in record_out] *)
let parse_args args =
  let allowed_args = [ "record_in"; "record_out"; "syntax_off" ] in
  match
    List.find ~f:(fun a -> not (List.mem ~equal:String.equal allowed_args a)) args
  with
  | Some unknown ->
      Error (Printf.sprintf "Unknown rapper argument '%s'" unknown)
  | None ->
      let record_in = List.mem args "record_in" ~equal:String.equal in
      let record_out = List.mem args "record_out" ~equal:String.equal in
      let syntax_off = List.mem args "syntax_off" ~equal:String.equal in
      Ok (record_in, record_out, syntax_off)

(** Make some subexpressions to be used in generated code *)
let component_expressions ~loc parsed_query =
  let open Query in
  let inputs_caqti_type =
    Codegen.make_caqti_type_tup ~loc parsed_query.in_params
  in
  let outputs_caqti_type =
    Codegen.make_caqti_type_tup ~loc parsed_query.out_params
  in
  let parsed_sql = Buildef.estring ~loc parsed_query.sql in
  (inputs_caqti_type, outputs_caqti_type, parsed_sql)

(** Make a function [expand_get] to produce the expressions for [get_one], [get_opt] and [get_many], and a similar [expand_exec] for [execute] *)
let make_expand_get_and_exec_expression ~loc parsed_query record_in record_out =
  let inputs_caqti_type, outputs_caqti_type, parsed_sql =
    component_expressions ~loc parsed_query
  in
  let expression_contents =
    Codegen.
      {
        in_params = parsed_query.in_params;
        out_params = parsed_query.out_params;
        record_in;
        record_out;
      }
  in
  let expand_get caqti_request_function_expr make_function =
    try
      Ok
        [%expr
          let query =
            Caqti_request.([%e caqti_request_function_expr])
              Caqti_type.([%e inputs_caqti_type])
              Caqti_type.([%e outputs_caqti_type])
              [%e parsed_sql]
          in
          let wrapped (module Db : Caqti_lwt.CONNECTION) =
            [%e make_function ~loc expression_contents]
          in
          wrapped]
    with Codegen.Error s -> Error s
  in

  let expand_exec caqti_request_function_expr make_function =
    try
      Ok
        [%expr
          let query =
            Caqti_request.([%e caqti_request_function_expr])
              Caqti_type.([%e inputs_caqti_type])
              [%e parsed_sql]
          in
          let wrapped (module Db : Caqti_lwt.CONNECTION) =
            [%e make_function ~loc expression_contents]
          in
          wrapped]
    with Codegen.Error s -> Error s
  in
  (expand_get, expand_exec)

let expand ~loc ~path:_ action query args =
  let expression_result =
    match parse_args args with
    | Error err -> Error err
    | Ok (record_in, record_out, syntax_off) -> (
        match Query.parse query with
        | Error error -> Error (Query.explain_error error)
        | Ok parsed_query -> (
            let syntax_result =
              match syntax_off with
              | false -> (
                  match Pg_query.parse parsed_query.sql with
                  | Ok _ -> Ok ()
                  | Error msg ->
                      Error (Printf.sprintf "Syntax error in SQL: '%s'" msg) )
              | true -> Ok ()
            in
            match syntax_result with
            | Error msg -> Error msg
            | Ok () ->
                Ok
                  (let expand_get, expand_exec =
                     make_expand_get_and_exec_expression ~loc parsed_query
                       record_in record_out
                   in
                   match action with
                   (* execute is special case because there is no output Caqti_type *)
                   | "execute" ->
                       if record_out then
                         Error "record_out is not a valid argument for execute"
                       else expand_exec [%expr exec] Codegen.exec_function
                   | "get_one" -> expand_get [%expr find] Codegen.find_function
                   | "get_opt" ->
                       expand_get [%expr find_opt] Codegen.find_opt_function
                   | "get_many" ->
                       expand_get [%expr collect] Codegen.collect_list_function
                   | _ ->
                       Error
                         "Supported actions are execute, get_one, get_opt and \
                          get_many") ) )
  in
  match expression_result with
  | Ok (Ok expr) -> expr
  | Ok (Error msg) | Error msg ->
      raise
        (Location.Error
           (Location.Error.createf ~loc "Error in ppx_rapper: %s" msg))

(** Captures [[%rapper get_one "SELECT id FROM things WHERE condition"]] *)
let pattern =
  let open Ast_pattern in
  let query_action = pexp_ident (lident __) in
  let query = pair nolabel (estring __) in
  let arg = pair nolabel (pexp_ident (lident __)) in
  (*   let arg_opt = alt_option (arg ^:: nil) nil in *)
  (*   let arg2 = pair nolabel (pexp_ident (lident __)) in *)
  (*   let arg2_opt = alt_option (arg2 ^:: nil) nil in *)
  let arguments = query ^:: many arg in
  pexp_apply query_action arguments

let name = "rapper"

let ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () = Driver.register_transformation name ~extensions:[ ext ]
