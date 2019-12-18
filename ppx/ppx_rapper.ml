open Core
open Ppxlib
module Buildef = Ast_builder.Default

let expand ~loc ~path:_ action query first_arg =
  let record_out =
    match first_arg with
    | Some "record_out" -> true
    | Some _ -> false
    | None -> false
  in
  let expression_result =
    match Query.parse query with
    | Error error -> Error (Query.explain_error error)
    | Ok parsed_query ->
        Ok
          (let inputs_caqti_type =
             Codegen.make_caqti_type_tup ~loc parsed_query.in_params
           in
           let outputs_caqti_type =
             Codegen.make_caqti_type_tup ~loc parsed_query.out_params
           in
           let parsed_sql = Buildef.estring ~loc parsed_query.sql in
           let expression_contents =
             Codegen.
               {
                 in_params = parsed_query.in_params;
                 out_params = parsed_query.out_params;
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
           match action with
           (* execute is special case because there is no output Caqti_type *)
           | "execute" -> (
               if record_out then
                 Error "record_out is not a valid argument for execute"
               else
                 try
                   Ok
                     [%expr
                       let query =
                         Caqti_request.exec
                           Caqti_type.([%e inputs_caqti_type])
                           [%e parsed_sql]
                       in
                       let wrapped (module Db : Caqti_lwt.CONNECTION) =
                         [%e Codegen.exec_function ~loc expression_contents]
                       in
                       wrapped]
                 with Codegen.Error s -> Error s )
           | "get_one" -> expand_get [%expr find] Codegen.find_function
           | "get_opt" -> expand_get [%expr find_opt] Codegen.find_opt_function
           | "get_many" ->
               expand_get [%expr collect] Codegen.collect_list_function
           | _ ->
               Error
                 "Supported actions are execute, get_one, get_opt and get_many")
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
  let record_out = pair nolabel (pexp_ident (lident __)) in
  let with_record_out = record_out ^:: nil in
  let arguments = query ^:: alt_option with_record_out nil in
  pexp_apply query_action arguments

let name = "rapper"

let ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () = Driver.register_transformation name ~extensions:[ ext ]
