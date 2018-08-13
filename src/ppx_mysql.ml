open Ppxlib
open Ppx_mysql_lib

module Astdef = Ast_builder.Default

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
        let open Astdef in
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
    let f = Astdef.pexp_ident ~loc (Loc.make ~loc (Lident param.to_string)) in
    let arg = Astdef.pexp_ident ~loc (Loc.make ~loc (Lident param.name)) in
    if param.opt
    then [%expr (Ppx_mysql_lib.map_option [%e f]) [%e arg]]
    else [%expr [%e f] [%e arg]]

let build_out_param_processor ~loc out_params =
    let make_tuple_elem i param =
        let f = Astdef.pexp_ident ~loc (Loc.make ~loc (Lident param.of_string)) in
        let arg = [%expr row.([%e Astdef.eint ~loc i])] in
        let appl = [%expr (Ppx_mysql_lib.map_option [%e f]) [%e arg]] in
        if param.opt
        then appl
        else [%expr (Ppx_mysql_lib.get_option [%e appl])] in
    [%expr fun f row ->
        if Array.length row = [%e Astdef.eint ~loc (List.length out_params)]
        then
            [%e Astdef.pexp_tuple ~loc (List.mapi make_tuple_elem out_params)]
        else
            assert false (* FIXME *)
        ]

let expand ~loc ~path:_ (sql_variant: string) (query: string) =
    let sql_variant = sql_variant_of_string sql_variant in
    match Ppx_mysql_lib.parse_query query with
        | Ok (query, in_params, out_params) ->
            let expr =
                [%expr
                let open IO in
                let query = [%e Astdef.estring ~loc query] in
                let params = [%e Astdef.(pexp_array ~loc @@ List.map (build_in_param ~loc) in_params) ] in
                let process_out_params = [%e build_out_param_processor ~loc out_params] in
                Ppx_mysql_lib.Prepared.with' dbh query @@ fun stmt ->
                    Ppx_mysql_lib.Prepared.execute stmt params >>= fun stmt_result ->
                    Ppx_mysql_lib.Prepared.map f stmt_result >>= fun fetch_result ->
                    fetch_result
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
