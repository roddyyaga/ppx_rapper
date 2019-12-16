open Core
open Ppxlib
module Buildef = Ast_builder.Default

let up_to_last xs = List.take xs (List.length xs - 1)

let caqti_type_of_param ~loc Query.{ typ = _, base_type; opt; _ } =
  let base_expr =
    match base_type with
    | "string" -> [%expr string]
    | "int" -> [%expr int]
    | "bool" -> [%expr bool]
    | _ -> failwith (Printf.sprintf "Unsupported base type %s" base_type)
  in
  match opt with
  | true -> Buildef.(pexp_apply ~loc [%expr option] [ (Nolabel, base_expr) ])
  | false -> base_expr

let make_caqti_type_tup ~loc in_params =
  let type_exprs = List.map ~f:(caqti_type_of_param ~loc) in_params in
  let f elem_type_expr apply_expr =
    [%expr tup2 [%e elem_type_expr] [%e apply_expr]]
  in
  List.fold_right ~f ~init:(List.last_exn type_exprs) (up_to_last type_exprs)

let make_nested_tuples ~loc in_params =
  match List.length in_params with
  (* With current design, 0-tuple case should not occur. *)
  | 0 -> [%expr ()]
  | _ ->
      let idents =
        List.map
          ~f:(fun param ->
            Buildef.pexp_ident ~loc (Loc.make ~loc (Lident param.Query.name)))
          in_params
      in
      let f ident accum = Buildef.pexp_tuple ~loc [ ident; accum ] in
      List.fold_right ~f ~init:(List.last_exn idents) (up_to_last idents)

(** Generates code like [fun ~x ~y ~z -> Db.exec query (x, (y, z))]. *)
let make_exec_function ~loc in_params =
  if List.is_empty in_params then [%expr fun () -> Db.exec query ()]
  else
    let tuples = make_nested_tuples ~loc in_params in
    let body = [%expr Db.exec query [%e tuples]] in
    let f in_param body_so_far =
      let name = in_param.Query.name in
      let pattern = Buildef.ppat_var ~loc (Loc.make ~loc name) in
      Buildef.pexp_fun ~loc (Labelled name) None pattern body_so_far
    in
    List.fold_right ~f ~init:body in_params

let expand ~loc ~path:_ action query =
  let parsed_query =
    match Query.parse query with
    | Ok parsed_query -> parsed_query
    | Error _ -> failwith "Couldn't parse query!"
  in
  let inputs_caqti_type =
    match List.length parsed_query.in_params with
    | 0 -> [%expr unit]
    (*| 1 -> caqti_type_of_param ~loc (List.hd_exn parsed_query.in_params)*)
    | _ -> make_caqti_type_tup ~loc parsed_query.in_params
  in
  let parsed_sql = Buildef.estring ~loc parsed_query.sql in
  match action with
  | "execute" ->
      [%expr
        let query =
          Caqti_request.exec Caqti_type.([%e inputs_caqti_type]) [%e parsed_sql]
        in
        let wrapped (module Db : Caqti_lwt.CONNECTION) =
          [%e make_exec_function ~loc parsed_query.in_params]
        in
        wrapped]
  | _ -> failwith "Action not implemented"

(** Captures [[%rapper get_one "SELECT id FROM things WHERE condition"]] *)
let pattern =
  let open Ast_pattern in
  let query_action = pexp_ident (lident __) in
  let query = pair nolabel (estring __) in
  Ast_pattern.(pexp_apply query_action (query ^:: nil))

let name = "rapper"

let ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload pattern)
    expand

let () = Driver.register_transformation name ~extensions:[ ext ]
