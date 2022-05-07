open Base
open Ppxlib
module Buildef = Ast_builder.Default

(** Handle 'record_in' etc. in [%rapper "SELECT * FROM USERS" record_in record_out] *)
let parse_args args =
  let allowed_args =
    [ "record_in"; "record_out"; "function_out"; "syntax_off" ]
  in
  match
    List.find
      ~f:(fun a -> not (List.mem ~equal:String.equal allowed_args a))
      args
  with
  | Some unknown ->
      Error (Printf.sprintf "Unknown rapper argument '%s'" unknown)
  | None ->
      let record_in = List.mem args "record_in" ~equal:String.equal in
      let record_out = List.mem args "record_out" ~equal:String.equal in
      let function_out = List.mem args "function_out" ~equal:String.equal in
      let input_kind = if record_in then `Record else `Labelled_args in
      let output_kind =
        match (record_out, function_out) with
        | false, false -> `Tuple
        | true, false -> `Record
        | false, true -> `Function
        | true, true -> assert false
      in
      let syntax_off = List.mem args "syntax_off" ~equal:String.equal in
      assert (not (function_out && record_out));
      Ok (input_kind, output_kind, syntax_off)

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
let make_expand_get_and_exec_expression ~loc parsed_query input_kind output_kind
    =
  let { Query.sql; in_params; out_params; list_params } = parsed_query in
  match list_params with
  | Some { subsql; string_index; param_index; params } ->
      if not (List.length params = 1) then
        failwith "%list only supports one input parameter currently";
      let subsql_expr = Buildef.estring ~loc subsql in
      let sql_before =
        Buildef.estring ~loc @@ String.sub sql ~pos:0 ~len:string_index
      in
      let sql_after =
        Buildef.estring ~loc
        @@ String.sub sql ~pos:string_index
             ~len:(String.length sql - string_index)
      in
      let params_before, params_after = List.split_n in_params param_index in
      let expression_contents =
        {
          Codegen.in_params = params_before @ params @ params_after;
          out_params;
          input_kind;
          output_kind;
        }
      in
      let caqti_input_type =
        let exprs_before =
          List.map ~f:(Codegen.caqti_type_of_param ~loc) params_before
        in
        let exprs_after =
          List.map ~f:(Codegen.caqti_type_of_param ~loc) params_after
        in
        match (List.is_empty params_before, List.is_empty params_after) with
        | true, true -> [%expr packed_list_type]
        | true, false ->
            let expression =
              Codegen.caqti_type_tup_of_expressions ~loc
                ([%expr packed_list_type] :: exprs_after)
            in
            [%expr Caqti_type.([%e expression])]
        | false, true ->
            let expression =
              Codegen.caqti_type_tup_of_expressions ~loc
                (exprs_before @ [ [%expr packed_list_type] ])
            in
            [%expr Caqti_type.([%e expression])]
        | false, false ->
            let expression =
              Codegen.caqti_type_tup_of_expressions ~loc
                (exprs_before @ [ [%expr packed_list_type] ] @ exprs_after)
            in
            [%expr Caqti_type.([%e expression])]
      in
      let outputs_caqti_type = Codegen.make_caqti_type_tup ~loc out_params in
      let list_param = List.hd_exn params in
      let make_generic make_function query_expr =
        let body_fn body =
          let base =
            [%expr
              match
                [%e
                  Buildef.pexp_ident ~loc
                    (Codegen.lident_of_param ~loc list_param)]
              with
              | [] ->
                  Rapper_helper.fail
                    Caqti_error.(
                      encode_rejected ~uri:Uri.empty ~typ:Caqti_type.unit
                        (Msg "Empty list"))
              | elems ->
                  let subsqls =
                    Stdlib.List.map (fun _ -> [%e subsql_expr]) elems
                  in
                  let patch = Stdlib.String.concat ", " subsqls in
                  let sql = [%e sql_before] ^ patch ^ [%e sql_after] in
                  let open Rapper.Internal in
                  let (Dynparam.Pack
                        ( packed_list_type,
                          [%p Codegen.ppat_of_param ~loc list_param] )) =
                    Stdlib.List.fold_left
                      (fun pack item ->
                        Dynparam.add
                          (Caqti_type.(
                             [%e
                               Codegen.make_caqti_type_tup ~loc [ list_param ]])
                          [@ocaml.warning "-33"])
                          item pack)
                      Dynparam.empty elems
                  in
                  let query = [%e query_expr] in
                  [%e body]]
          in
          match output_kind with
          | `Function -> [%expr fun loaders -> [%e base]]
          | _ -> base
        in
        match output_kind with
        | `Function ->
            [%expr
              let wrapped =
                [%e make_function ~body_fn ~loc expression_contents]
              in
              wrapped loaders]
        | _ ->
            [%expr
              let wrapped =
                [%e make_function ~body_fn ~loc expression_contents]
              in
              wrapped]
      in
      let expand_get caqti_request_function_expr make_function =
        try
          Ok
            (make_generic make_function
               [%expr
                 [%e caqti_request_function_expr]
                   ~oneshot:true ([%e caqti_input_type] [@ocaml.warning "-33"])
                   (Caqti_type.([%e outputs_caqti_type]) [@ocaml.warning "-33"])
                   sql])
        with Codegen.Error s -> Error s
      in

      let expand_exec caqti_request_function_expr make_function =
        try
          Ok
            (make_generic make_function
               [%expr
                 [%e caqti_request_function_expr]
                   [%e caqti_input_type] (Caqti_type.unit) sql])
        with Codegen.Error s -> Error s
      in
      (expand_get, expand_exec)
  | None ->
      let inputs_caqti_type, outputs_caqti_type, parsed_sql =
        component_expressions ~loc parsed_query
      in
      let expression_contents =
        Codegen.
          {
            in_params = parsed_query.in_params;
            out_params = parsed_query.out_params;
            input_kind;
            output_kind;
          }
      in
      let make_generic make_function query_expr =
        match output_kind with
        | `Function ->
            [%expr
              fun loaders ->
                let query = [%e query_expr] in
                let wrapped =
                  [%e
                    make_function ~body_fn:(fun x -> x) ~loc expression_contents]
                in
                wrapped loaders]
        | _ ->
            [%expr
              let query = [%e query_expr] in
              let wrapped =
                [%e
                  make_function ~body_fn:(fun x -> x) ~loc expression_contents]
              in
              wrapped]
      in
      let expand_get caqti_request_function_expr make_function =
        try
          Ok
            (make_generic make_function
               [%expr
                 [%e caqti_request_function_expr]
                   (Caqti_type.([%e inputs_caqti_type]) [@ocaml.warning "-33"])
                   (Caqti_type.([%e outputs_caqti_type]) [@ocaml.warning "-33"])
                   [%e parsed_sql]])
        with Codegen.Error s -> Error s
      in

      let expand_exec caqti_request_function_expr make_function =
        try
          Ok
            (make_generic make_function
               [%expr
                 [%e caqti_request_function_expr]
                   (Caqti_type.([%e inputs_caqti_type]) [@ocaml.warning "-33"])
                   (Caqti_type.unit) [%e parsed_sql]])
        with Codegen.Error s -> Error s
      in
      (expand_get, expand_exec)

let expand_apply ~loc ~path:_ action query args =
  let expression_result =
    match parse_args args with
    | Error err -> Error err
    | Ok (input_kind, output_kind, syntax_off) -> (
        match Query.parse query with
        | Error error -> Error (Query.explain_error error)
        | Ok parsed_query -> (
            let syntax_result =
              match syntax_off with
              | false -> (
                  let query_sql =
                    match parsed_query.list_params with
                    | Some { subsql; string_index; _ } ->
                        let sql = parsed_query.sql in
                        let sql_before =
                          String.sub sql ~pos:0 ~len:string_index
                        in
                        let sql_after =
                          String.sub sql ~pos:string_index
                            ~len:(String.length sql - string_index)
                        in
                        sql_before ^ subsql ^ sql_after
                    | None -> parsed_query.sql
                  in
                  match Pg_query.parse query_sql with
                  | Ok _ -> Ok ()
                  | Error msg ->
                      Error (Printf.sprintf "Syntax error in SQL: '%s'" msg))
              | true -> Ok ()
            in
            match syntax_result with
            | Error msg -> Error msg
            | Ok () ->
                Ok
                  (let expand_get, expand_exec =
                     make_expand_get_and_exec_expression ~loc parsed_query
                       input_kind output_kind
                   in
                   match action with
                   (* execute is special case because there is no output Caqti_type *)
                   | "execute" -> (
                       match output_kind with
                       | `Record ->
                           Error
                             "record_out is not a valid argument for execute"
                       (* TODO - could implement this *)
                       | `Function ->
                           Error
                             "function_out is not a valid argument for execute"
                       | `Tuple ->
                           expand_exec [%expr Caqti_request.Infix.(->.)] Codegen.exec_function)
                   | "get_one" -> expand_get [%expr Caqti_request.Infix.(->!)] Codegen.find_function
                   | "get_opt" ->
                       expand_get [%expr Caqti_request.Infix.(->?)] Codegen.find_opt_function
                   | "get_many" ->
                       expand_get [%expr Caqti_request.Infix.(->*)] Codegen.collect_list_function
                   | _ ->
                       Error
                         "Supported actions are execute, get_one, get_opt and \
                          get_many")))
  in
  match expression_result with
  | Ok (Ok expr) -> expr
  | Ok (Error msg) | Error msg ->
      raise
        (Location.Error
           (Location.Error.createf ~loc "Error in ppx_rapper: %s" msg))

let expand_let ~loc ~path vb_var action query args =
  let module Ast = Ast_builder.Default in
  let vb =
    Ast.value_binding ~loc
      ~pat:(Ast.ppat_var ~loc (Ast.Located.mk ~loc vb_var))
      ~expr:(expand_apply ~loc ~path action query args)
  in
  Ast.pstr_value ~loc Nonrecursive [ vb ]

(** Captures [\[%rapper get_one "SELECT id FROM things WHERE condition"\]] *)
let apply_pattern () =
  let open Ast_pattern in
  let query_action = pexp_ident (lident __) in
  let query = pair nolabel (estring __) in
  let arg = pair nolabel (pexp_ident (lident __)) in
  (*   let arg_opt = alt_option (arg ^:: nil) nil in *)
  (*   let arg2 = pair nolabel (pexp_ident (lident __)) in *)
  (*   let arg2_opt = alt_option (arg2 ^:: nil) nil in *)
  let arguments = query ^:: many arg in
  pexp_apply query_action arguments

(** Captures [\[let%rapper get_thing = get_one "SELECT id FROM things WHERE condition"\]] *)
let let_pattern () =
  let open Ast_pattern in
  pstr
    (pstr_value nonrecursive
       (value_binding ~pat:(ppat_var __) ~expr:(apply_pattern ()) ^:: nil)
    ^:: nil)

let name = "rapper"

let apply_ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload (apply_pattern ()))
    expand_apply

let let_ext =
  Extension.declare name Extension.Context.structure_item (let_pattern ())
    expand_let

let () = Driver.register_transformation name ~extensions:[ let_ext; apply_ext ]
