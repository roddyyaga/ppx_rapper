open Ppxlib
open Ppx_mysql_runtime.Stdlib

(* So the unit tests have access to the Query module *)
module Query = Query
module Buildef = Ast_builder.Default

let rec build_fun_chain ~loc expr = function
  | [] ->
      expr
  | Query.({typ; opt; name; _}) :: tl ->
      let open Buildef in
      let tl' = build_fun_chain ~loc expr tl in
      let var = ppat_var ~loc (Loc.make ~loc name) in
      let basetyp = ptyp_constr ~loc (Loc.make ~loc (Lident typ)) [] in
      let fulltyp =
        match opt with
        | true ->
            ptyp_constr ~loc (Loc.make ~loc (Lident "option")) [basetyp]
        | false ->
            basetyp
      in
      let pat = ppat_constraint ~loc var fulltyp in
      pexp_fun ~loc (Labelled name) None pat tl'

let build_in_param ~loc param =
  let to_string_mod, to_string_fun = Query.(param.to_string) in
  let to_string =
    Buildef.pexp_ident ~loc (Loc.make ~loc (Ldot (Lident to_string_mod, to_string_fun)))
  in
  let arg = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.name)) in
  match param.opt with
  | true ->
      [%expr (Ppx_mysql_runtime.Stdlib.Option.map [%e to_string]) [%e arg]]
  | false ->
      [%expr Ppx_mysql_runtime.Stdlib.Option.Some ([%e to_string] [%e arg])]

let build_out_param_processor ~loc out_params =
  let make_elem i param =
    let of_string_mod, of_string_fun = Query.(param.of_string) in
    let of_string =
      Buildef.pexp_ident
        ~loc
        (Loc.make ~loc (Ldot (Lident of_string_mod, of_string_fun)))
    in
    let param_name = Buildef.estring ~loc Query.(param.name) in
    let of_string_desc =
      Buildef.estring ~loc @@ Printf.sprintf "%s.%s" of_string_mod of_string_fun
    in
    let arg = [%expr Ppx_mysql_runtime.Stdlib.Array.get row [%e Buildef.eint ~loc i]] in
    let appl =
      [%expr
        let deserialize value =
          try [%e of_string] value with Failure _ ->
            raise (Deserialization_error ([%e param_name], [%e of_string_desc], value))
        in
        Ppx_mysql_runtime.Stdlib.Option.map deserialize [%e arg]]
    in
    match param.opt with
    | true ->
        appl
    | false ->
        [%expr
          try Ppx_mysql_runtime.Stdlib.Option.get [%e appl] with Invalid_argument _ ->
            raise (Expected_non_null_column [%e param_name])]
  in
  let ret_expr =
    match out_params with
    | [] ->
        [%expr ()]
    | [x] ->
        make_elem 0 x
    | _ :: _ :: _ ->
        Buildef.pexp_tuple ~loc @@ List.mapi make_elem out_params
  in
  let len_expected = Buildef.eint ~loc (List.length out_params) in
  [%expr
    fun row ->
      (let exception Deserialization_error of string * string * string in
      (let exception Expected_non_null_column of string in
      let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
      let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
      if len_row = [%e len_expected]
      then
        try Ppx_mysql_runtime.Stdlib.Result.Ok [%e ret_expr] with
        | Deserialization_error (col, f, v) ->
            Ppx_mysql_runtime.Stdlib.Result.Error
              (`Column_errors [col, `Deserialization_error (f, v)])
        | Expected_non_null_column col ->
            Ppx_mysql_runtime.Stdlib.Result.Error
              (`Column_errors [col, `Expected_non_null_value])
      else
        Ppx_mysql_runtime.Stdlib.Result.Error
          (`Unexpected_number_of_columns (len_row, [%e len_expected]))) [@warning "-38"]) 
      [@warning "-38"]]

let build_process_rows ~loc = function
  | "select_one" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result
              >>= fun maybe_row ->
              match acc, maybe_row with
              | [], Ppx_mysql_runtime.Stdlib.Option.Some row -> (
                match process_out_params row with
                | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                    loop [row']
                | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                    IO.return err )
              | [], Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
              | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
              | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
            in
            loop []]
  | "select_opt" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result
              >>= fun maybe_row ->
              match acc, maybe_row with
              | [], Ppx_mysql_runtime.Stdlib.Option.Some row -> (
                match process_out_params row with
                | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                    loop [row']
                | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                    IO.return err )
              | [], Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Ok
                       Ppx_mysql_runtime.Stdlib.Option.None)
              | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_maybe_one_found_many)
              | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Ok
                       (Ppx_mysql_runtime.Stdlib.Option.Some hd))
            in
            loop []]
  | "select_all" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result
              >>= function
              | Ppx_mysql_runtime.Stdlib.Option.Some row -> (
                match process_out_params row with
                | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                    loop (row' :: acc)
                | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                    IO.return err )
              | Ppx_mysql_runtime.Stdlib.Option.None ->
                  IO.return
                    (Ppx_mysql_runtime.Stdlib.Result.Ok
                       (Ppx_mysql_runtime.Stdlib.List.rev acc))
            in
            loop []]
  | "execute" ->
      Ok
        [%expr
          fun () ->
            Prepared.fetch stmt_result
            >>= function
            | Ppx_mysql_runtime.Stdlib.Option.Some _ ->
                IO.return
                  (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_none_found_one)
            | Ppx_mysql_runtime.Stdlib.Option.None ->
                IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ())]
  | etc ->
      Error (`Unknown_query_variant etc)

let actually_expand ~loc sql_variant query =
  let open Result in
  build_process_rows ~loc sql_variant
  >>= fun process_rows ->
  Query.parse query
  >>= fun {query; in_params; out_params} ->
  Query.remove_duplicates in_params
  >>= fun unique_in_params ->
  (* Note that in the expr fragment below we disable warning 26 (about unused variables)
     for the 'process_out_params' function, since it may indeed be unused if there are
     no output parameters. *)
  let expr =
    [%expr
      let open IO_result in
      let query = [%e Buildef.estring ~loc query] in
      let params =
        [%e Buildef.(pexp_array ~loc @@ List.map (build_in_param ~loc) in_params)]
      in
      let[@warning "-26"] process_out_params =
        [%e build_out_param_processor ~loc out_params]
      in
      Prepared.with_stmt dbh query
      @@ fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result -> [%e process_rows] ()]
  in
  let dbh_pat = Buildef.ppat_var ~loc (Loc.make ~loc "dbh") in
  let chain = build_fun_chain ~loc expr unique_in_params in
  Ok (Buildef.pexp_fun ~loc Nolabel None dbh_pat chain)

let expand ~loc ~path:_ sql_variant query =
  match actually_expand ~loc sql_variant query with
  | Ok expr ->
      expr
  | Error err ->
      let msg =
        match err with
        | #Query.error as err ->
            Query.explain_error err
        | `Unknown_query_variant variant ->
            Printf.sprintf "I don't understand query variant '%s'" variant
      in
      raise
        (Location.Error
           (Location.Error.createf ~loc "Error in 'mysql' extension: %s" msg))

let pattern =
  Ast_pattern.(pexp_apply (pexp_ident (lident __)) (pair nolabel (estring __) ^:: nil))

let name = "mysql"

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () = Driver.register_transformation name ~extensions:[ext]
