open Ppxlib
open Ppx_mysql_runtime.Stdlib

(* So the unit tests have access to the Query module *)
module Query = Query
module Buildef = Ast_builder.Default

(* [split_n] has the same signature and semantics as its homonym in Base.
 * [split_n xs n] is [(take xs n, drop xs n)].
 *)
let split_n elems index =
  let rec loop accum leftovers index =
    match leftovers, index with
    | _, x when x <= 0 -> List.rev accum, leftovers
    | [], _ -> List.rev accum, leftovers
    | hd :: tl, i -> loop (hd :: accum) tl (i - 1)
  in
  loop [] elems index

let create_unique_var ~loc params base =
  let already_exists name =
    List.exists (fun param -> Query.(param.name) = name) params
  in
  let rec add_suffix counter =
    let candidate = Printf.sprintf "%s_%d" base counter in
    match already_exists candidate with
    | true -> add_suffix (counter + 1)
    | false -> candidate
  in
  let name =
    match already_exists base with
    | true -> add_suffix 0
    | false -> base
  in
  let pat = Buildef.ppat_var ~loc (Loc.make ~loc name) in
  let ident = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident name)) in
  pat, ident

let rec build_fun_chain ~loc expr = function
  | [] -> expr
  | Query.{typ; opt; name; _} :: tl ->
      let open Buildef in
      let tl' = build_fun_chain ~loc expr tl in
      let var = ppat_var ~loc (Loc.make ~loc name) in
      let basetyp =
        match typ with
        | None, typ -> ptyp_constr ~loc (Loc.make ~loc (Lident typ)) []
        | Some module_name, typ ->
            ptyp_constr ~loc (Loc.make ~loc (Ldot (Lident module_name, typ))) []
      in
      let fulltyp =
        match opt with
        | true -> ptyp_constr ~loc (Loc.make ~loc (Lident "option")) [basetyp]
        | false -> basetyp
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
  | true -> [%expr (Option.map [%e to_string]) [%e arg]]
  | false -> [%expr Option.Some ([%e to_string] [%e arg])]

let make_column_expr ~loc i param =
  let of_string_mod, of_string_fun = Query.(param.of_string) in
  let of_string =
    Buildef.pexp_ident ~loc (Loc.make ~loc (Ldot (Lident of_string_mod, of_string_fun)))
  in
  let param_name = Buildef.estring ~loc Query.(param.name) in
  let of_string_desc =
    Buildef.estring ~loc @@ Printf.sprintf "%s.%s" of_string_mod of_string_fun
  in
  let idx = Buildef.eint ~loc i in
  let arg = [%expr row.([%e idx])] in
  let processor =
    match param.opt with
    | true -> [%expr Ppx_mysql_runtime.deserialize_nullable_column]
    | false -> [%expr Ppx_mysql_runtime.deserialize_non_nullable_column]
  in
  [%expr
    [%e processor]
      [%e idx]
      [%e param_name]
      [%e of_string]
      [%e of_string_desc]
      err_accum
      [%e arg]]

let build_out_param_processor ~loc out_params =
  let len_out_params = List.length out_params in
  let ret_expr =
    match out_params with
    | [] -> [%expr Result.Ok ()]
    | [x] ->
        [%expr
          let err_accum = [] in
          match [%e make_column_expr ~loc 0 x] with
          | Option.Some res, _ -> Result.Ok res
          | Option.None, err -> Result.Error (`Column_errors err)]
    | _ ->
        let make_ident_str name i = String.append name @@ string_of_int i in
        let make_ident_expr name i =
          Buildef.pexp_ident ~loc (Loc.make ~loc (Lident (make_ident_str name i)))
        in
        let make_ident_pat name i =
          Buildef.ppat_var ~loc (Loc.make ~loc @@ make_ident_str name i)
        in
        let match_expr =
          let test_expr =
            Buildef.pexp_tuple ~loc @@ List.init len_out_params (make_ident_expr "col")
          in
          let ok_case =
            let lhs =
              Buildef.ppat_tuple ~loc
              @@ List.init len_out_params (fun i ->
                     [%pat? Option.Some [%p make_ident_pat "v" i]] )
            in
            let rhs =
              let tuple =
                Buildef.pexp_tuple ~loc @@ List.init len_out_params (make_ident_expr "v")
              in
              [%expr Result.Ok [%e tuple]]
            in
            Buildef.case ~lhs ~guard:None ~rhs
          in
          let error_case =
            let lhs = Buildef.ppat_any ~loc in
            let rhs = [%expr Result.Error (`Column_errors err_accum)] in
            Buildef.case ~lhs ~guard:None ~rhs
          in
          Buildef.pexp_match ~loc test_expr [ok_case; error_case]
        in
        let call_chain, _ =
          let err_accum_pat = Buildef.ppat_var ~loc (Loc.make ~loc "err_accum") in
          let make_call out_param (accum, i) =
            let pat = Buildef.ppat_tuple ~loc [make_ident_pat "col" i; err_accum_pat] in
            let expr = make_column_expr ~loc i out_param in
            let binding = Buildef.value_binding ~loc ~pat ~expr in
            Buildef.pexp_let ~loc Nonrecursive [binding] accum, i - 1
          in
          List.fold_right make_call out_params (match_expr, len_out_params - 1)
        in
        [%expr
          let err_accum = [] in
          [%e call_chain]]
  in
  let len_expected = Buildef.eint ~loc len_out_params in
  [%expr
    fun row ->
      let len_row = Array.length row in
      if Ppx_mysql_runtime.Stdlib.( = ) len_row [%e len_expected]
      then [%e ret_expr]
      else Result.Error (`Unexpected_number_of_columns (len_row, [%e len_expected]))]

let build_process_rows ~loc = function
  | "select_one" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result >>= fun maybe_row ->
              match acc, maybe_row with
              | [], Option.Some row -> (
                match process_out_params row with
                | Result.Ok row' -> loop [row']
                | Result.Error _ as err -> IO.return err )
              | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
              | _ :: _, Option.Some _ ->
                  IO.return (Result.Error `Expected_one_found_many)
              | hd :: _, Option.None -> IO.return (Result.Ok hd)
            in
            loop []]
  | "select_opt" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result >>= fun maybe_row ->
              match acc, maybe_row with
              | [], Option.Some row -> (
                match process_out_params row with
                | Result.Ok row' -> loop [row']
                | Result.Error _ as err -> IO.return err )
              | [], Option.None -> IO.return (Result.Ok Option.None)
              | _ :: _, Option.Some _ ->
                  IO.return (Result.Error `Expected_maybe_one_found_many)
              | hd :: _, Option.None -> IO.return (Result.Ok (Option.Some hd))
            in
            loop []]
  | "select_all" ->
      Ok
        [%expr
          fun () ->
            let rec loop acc =
              Prepared.fetch stmt_result >>= function
              | Option.Some row -> (
                match process_out_params row with
                | Result.Ok row' -> loop (row' :: acc)
                | Result.Error _ as err -> IO.return err )
              | Option.None -> IO.return (Result.Ok (List.rev acc))
            in
            loop []]
  | "execute" ->
      Ok
        [%expr
          fun () ->
            Prepared.fetch stmt_result >>= function
            | Option.Some _ -> IO.return (Result.Error `Expected_none_found_one)
            | Option.None -> IO.return (Result.Ok ())]
  | etc -> Error (`Unknown_query_action etc)

let actually_expand ~loc query_action cached query =
  let open Result in
  (match cached with
    | None | Some "true" -> Ok [%expr Prepared.with_stmt_cached]
    | Some "false" -> Ok [%expr Prepared.with_stmt_uncached]
    | Some etc -> Error (`Invalid_cached_parameter etc)) >>= fun with_stmt ->
  build_process_rows ~loc query_action >>= fun process_rows ->
  Query.parse query >>= fun {sql; in_params; out_params; list_params} ->
  Query.remove_duplicates in_params >>= fun unique_in_params ->
  let dbh_pat, dbh_ident = create_unique_var ~loc unique_in_params "dbh" in
  let elems_pat, elems_ident = create_unique_var ~loc unique_in_params "elems" in
  ( match list_params with
  | None ->
      let sql_expr = Buildef.estring ~loc sql in
      let param_expr =
        Buildef.pexp_array ~loc @@ List.map (build_in_param ~loc) in_params
      in
      Ok [%expr IO.return (Result.Ok ([%e sql_expr], [%e param_expr]))]
  | Some {subsql; string_index; param_index; params} ->
      Query.remove_duplicates params >>= fun unique_params ->
      let subsql_expr = Buildef.estring ~loc subsql in
      let sql_before = Buildef.estring ~loc @@ String.sub sql 0 string_index in
      let sql_after =
        Buildef.estring ~loc
        @@ String.sub sql string_index (String.length sql - string_index)
      in
      let params_before, params_after = split_n in_params param_index in
      let params_before =
        Buildef.pexp_array ~loc @@ List.map (build_in_param ~loc) params_before
      in
      let params_after =
        Buildef.pexp_array ~loc @@ List.map (build_in_param ~loc) params_after
      in
      let list_params_decl =
        let make_elem param = Buildef.ppat_var ~loc (Loc.make ~loc Query.(param.name)) in
        Buildef.ppat_tuple ~loc @@ List.map make_elem unique_params
      in
      let list_params_conv =
        Buildef.elist ~loc @@ List.map (build_in_param ~loc) params
      in
      Ok
        [%expr
          match [%e elems_ident] with
          | [] -> IO.return (Result.Error `Empty_input_list)
          | elems ->
              let subsqls = List.map (fun _ -> [%e subsql_expr]) elems in
              let patch = String.concat ", " subsqls in
              let sql =
                String.append [%e sql_before] (String.append patch [%e sql_after])
              in
              let params_between =
                Array.of_list
                  (List.concat
                     (List.map (fun [%p list_params_decl] -> [%e list_params_conv]) elems))
              in
              let params =
                Array.concat [[%e params_before]; params_between; [%e params_after]]
              in
              IO.return (Result.Ok (sql, params))] )
  >>= fun setup_expr ->
  (* Note that in the expr fragment below we disable warning 26 (about unused variables)
     for the 'process_out_params' function, since it may indeed be unused if there are
     no output parameters. *)
  let expr =
    [%expr
      let open IO_result in
      let module Array = Ppx_mysql_runtime.Stdlib.Array in
      let module List = Ppx_mysql_runtime.Stdlib.List in
      let module Option = Ppx_mysql_runtime.Stdlib.Option in
      let module String = Ppx_mysql_runtime.Stdlib.String in
      let module Result = Ppx_mysql_runtime.Stdlib.Result in
      [%e setup_expr] >>= fun (sql, params) ->
      let[@warning "-26"] process_out_params =
        [%e build_out_param_processor ~loc out_params]
      in
      [%e with_stmt] [%e dbh_ident] sql (fun stmt ->
          Prepared.execute_null stmt params >>= fun stmt_result -> [%e process_rows] ()
      )]
  in
  let chain = build_fun_chain ~loc expr unique_in_params in
  let chain =
    match list_params with
    | None -> chain
    | Some _ -> Buildef.pexp_fun ~loc Nolabel None elems_pat chain
  in
  Ok (Buildef.pexp_fun ~loc Nolabel None dbh_pat chain)

let expand ~loc ~path:_ query_action cached query =
  match actually_expand ~loc query_action cached query with
  | Ok expr -> expr
  | Error err ->
      let msg =
        match err with
        | #Query.error as err -> Query.explain_error err
        | `Unknown_query_action action ->
            Printf.sprintf "I don't understand query action '%s'" action
        | `Invalid_cached_parameter param ->
            Printf.sprintf "Only values 'true' or 'false' are accepted, but got '%s' instead" param
      in
      raise
        (Location.Error
           (Location.Error.createf ~loc "Error in 'mysql' extension: %s" msg))

let pattern =
  let open Ast_pattern in
  let query_action = pexp_ident (lident __) in
  let query = pair nolabel (estring __) in
  let cached = pair (labelled @@ string "cached") (pexp_construct (lident __) none) in
  let without_cached = query ^:: nil in
  let with_cached = cached ^:: without_cached in
  Ast_pattern.(pexp_apply query_action @@ Ast_pattern.alt_option with_cached without_cached)

let name = "mysql"

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () = Driver.register_transformation name ~extensions:[ext]
