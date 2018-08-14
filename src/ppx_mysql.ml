open Ppxlib
open Ppx_mysql_lib

module Buildef = Ast_builder.Default

type sql_variant =
    | Select_one
    | Select_opt
    | Select_all
    | Execute

let sql_variant_of_string = function
    | "Select_one" -> Select_one
    | "Select_opt" -> Select_opt
    | "Select_all" -> Select_all
    | "Execute"    -> Execute
    | x            -> invalid_arg ("sql_variant_of_string: " ^ x)

let name = "mysql"

let rec build_fun_chain ~loc expr = function
    | [] ->
        expr
    | {typ; opt; name; _} :: tl ->
        let open Buildef in
        let tl' = build_fun_chain ~loc expr tl in
        let var = ppat_var ~loc (Loc.make ~loc name) in
        let basetyp = ptyp_constr ~loc (Loc.make ~loc (Lident typ)) [] in
        let fulltyp =
            if opt
            then ptyp_constr ~loc (Loc.make ~loc (Lident "option")) [basetyp]
            else basetyp in
        let pat = ppat_constraint ~loc var fulltyp in
        pexp_fun ~loc (Labelled name) None pat tl'

let build_in_param ~loc param =
    let f = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.to_string)) in
    let arg = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.name)) in
    if param.opt
    then [%expr (Ppx_mysql_lib.map_option [%e f]) [%e arg]]
    else [%expr Some ([%e f] [%e arg])]

let build_out_param_processor ~loc out_params =
    let make_elem i param =
        let f = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.of_string)) in
        let arg = [%expr row.([%e Buildef.eint ~loc i])] in
        let appl = [%expr (Ppx_mysql_lib.map_option [%e f]) [%e arg]] in
        if param.opt
        then appl
        else [%expr (Ppx_mysql_lib.get_option [%e appl])] in
    let ret_expr = match out_params with
        | []     -> [%expr ()]
        | [x]    -> make_elem 0 x
        | _ :: _ -> Buildef.pexp_tuple ~loc (List.mapi make_elem out_params) in
    [%expr fun row ->
        if Array.length row = [%e Buildef.eint ~loc (List.length out_params)]
        then [%e ret_expr]
        else assert false (* FIXME *)
        ]

let expand ~loc ~path:_ (sql_variant: string) (query: string) =
    let postproc = match sql_variant with
        | "Select_one" -> "select_one"
        | "Select_opt" -> "select_opt"
        | "Select_all" -> "select_all"
        | "Execute"    -> "execute"
        | _            -> assert false in (* FIX ME *)
    let fq_postproc = Buildef.pexp_ident ~loc (Loc.make ~loc (Lident ("Ppx_mysql_lib." ^ postproc))) in
    match Ppx_mysql_lib.parse_query query with
        | Ok {query; in_params; out_params} ->
            let expr =
                [%expr
                let open Ppx_mysql_aux.IO in
                let query = [%e Buildef.estring ~loc query] in
                let params = [%e Buildef.(pexp_array ~loc @@ List.map (build_in_param ~loc) in_params) ] in
                let process_out_params = [%e build_out_param_processor ~loc out_params] in
                Ppx_mysql_aux.Prepared.with' dbh query @@ fun stmt ->
                    Ppx_mysql_aux.Prepared.execute stmt params >>= fun stmt_result ->
                    Ppx_mysql_aux.Prepared.map process_out_params stmt_result >>= fun xs ->
                    IO.return ([%e fq_postproc] xs)
                ] in
            build_fun_chain ~loc expr in_params
        | Error _ ->
            raise (Location.Error (Location.Error.createf ~loc "Error in mysql extension"))

let pattern =
    Ast_pattern.(pexp_construct (lident __) (some (estring __)))

let ext = Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () =
    Driver.register_transformation name ~extensions:[ext]
