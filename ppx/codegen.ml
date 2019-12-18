open Core
open Ppxlib
module Buildef = Ast_builder.Default

type extension_contents = {
  in_params: Query.param list;
  out_params: Query.param list;
  record_out: bool;
}

exception Error of string

(** [up_to_last (xs @ [x])] returns [xs] *)
let up_to_last xs = List.take xs (List.length xs - 1)

(** Produces individual Caqti types from parsed parameters *)
let caqti_type_of_param ~loc Query.{ typ = _, base_type; opt; _ } =
  let base_expr =
    match base_type with
    | "string" -> [%expr string]
    | "int" -> [%expr int]
    | "bool" -> [%expr bool]
    (* TODO - support float *)
    (* TODO - support custom types *)
    | other ->
        raise (Error (Printf.sprintf "Base type '%s' not supported" other))
  in
  match opt with
  | true -> Buildef.(pexp_apply ~loc [%expr option] [ (Nolabel, base_expr) ])
  | false -> base_expr

(** Makes Caqti type specifications like [string & option int & bool] *)
let make_caqti_type_tup ~loc params =
  match List.length params with
  | 0 -> [%expr unit]
  | _ ->
      let type_exprs = List.map ~f:(caqti_type_of_param ~loc) params in
      let f elem_type_expr apply_expr =
        [%expr tup2 [%e elem_type_expr] [%e apply_expr]]
      in
      List.fold_right ~f ~init:(List.last_exn type_exprs)
        (up_to_last type_exprs)

let lident_of_param ~loc param = Loc.make ~loc (Lident param.Query.name)

(** Maps parsed parameters to ident expressions of their names *)
let pexp_idents_of_params ~loc params =
  List.map
    ~f:(fun param -> Buildef.pexp_ident ~loc (lident_of_param ~loc param))
    params

(** Maps parsed parameters to var patterns of their names *)
let ppat_var_of_params ~loc params =
  List.map
    ~f:(fun param -> Buildef.ppat_var ~loc (Loc.make ~loc param.Query.name))
    params

(** General function for producing ASTs for [(a, (b, (c, (d, e))))] as either expressions or patterns *)
let nested_tuple_thing zero_case mapper tuple_maker ~loc params =
  match List.length params with
  (* With current design, 0-tuple case should not occur. *)
  | 0 -> zero_case
  | _ ->
      let idents = mapper ~loc params in
      let f ident accum = tuple_maker ~loc [ ident; accum ] in
      List.fold_right ~f ~init:(List.last_exn idents) (up_to_last idents)

(** Makes [(a, (b, (c, (d, e))))] expression ASTs from parsed parameters *)
let nested_tuple_expression ~loc =
  nested_tuple_thing [%expr ()] pexp_idents_of_params Buildef.pexp_tuple ~loc

(** Makes [(a, (b, (c, (d, e))))] pattern ASTs from parsed parameters *)
let nested_tuple_pattern ~loc =
  nested_tuple_thing
    (Buildef.ppat_tuple ~loc [])
    ppat_var_of_params Buildef.ppat_tuple ~loc

(** Makes [(a, b, c, d, e)] expression ASTs from parsed parameters *)
let flat_tuple ~loc params =
  Buildef.pexp_tuple ~loc (pexp_idents_of_params ~loc params)

(** Makes [{a; b; c; d; e}] expression ASTs from parsed parameters *)
let record_expression ~loc params =
  let f param =
    let lident = lident_of_param ~loc param in
    (lident, Buildef.pexp_ident ~loc lident)
  in
  let pair_list = List.map params ~f in
  Buildef.pexp_record ~loc pair_list None

(** Generates the function body for an [exec] function ([execute] statement) *)
let function_body_exec ~loc connection_function_expr
    { in_params; record_out; _ } =
  assert (not record_out);
  let input_nested_tuples = nested_tuple_expression ~loc in_params in
  [%expr [%e connection_function_expr] query [%e input_nested_tuples]]

let function_body_general ~loc factory connection_function_expr
    { in_params; out_params; record_out } =
  let input_nested_tuple_expression = nested_tuple_expression ~loc in_params in
  match (List.length out_params, record_out) with
  | 0, true ->
      raise
        (Error
           "'record_out' should not be set when there are no output parameters")
  | 0, false | 1, false ->
      [%expr
        [%e connection_function_expr] query [%e input_nested_tuple_expression]]
  | 1, true | _ ->
      let input_nested_tuple_pattern = nested_tuple_pattern ~loc out_params in
      let output_expression =
        if record_out then record_expression ~loc out_params
        else flat_tuple ~loc out_params
      in
      factory ~loc input_nested_tuple_pattern output_expression
        connection_function_expr input_nested_tuple_expression

let find_body_factory ~loc input_nested_tuple_pattern output_expression
    connection_function_expr input_nested_tuple_expression =
  [%expr
    let f result =
      Result.map
        ~f:(fun [%p input_nested_tuple_pattern] -> [%e output_expression])
        result
    in
    Lwt.map f
      ([%e connection_function_expr] query [%e input_nested_tuple_expression])]

let find_map_factory ~loc map_expr input_nested_tuple_pattern output_expression
    connection_function_expr input_nested_tuple_expression =
  [%expr
    let f result =
      let g [%p input_nested_tuple_pattern] = [%e output_expression] in
      Result.map ~f:([%e map_expr] ~f:g) result
    in
    Lwt.map f
      ([%e connection_function_expr] query [%e input_nested_tuple_expression])]

(** Generates the function body for a [find] function ([get_one] statement)*)
let function_body_find ~loc = function_body_general ~loc find_body_factory

(** Generates the function body for cases where it has involves a map

 * These are [find_opt] and [collect_list] (for [get_opt] and [get_many] statements). *)
let function_body_map map_expr ~loc =
  function_body_general ~loc (find_map_factory map_expr)

(** Generates the function body for a [find_opt] function ([get_opt] statement) *)
let function_body_find_opt ~loc = function_body_map [%expr Option.map] ~loc

(** Generates the function body for a [collect_list] function ([get_many] statement) *)
let function_body_collect ~loc = function_body_map [%expr List.map] ~loc

(** Generates code like [fun ~x ~y ~z -> Db.some_function query (x, (y, z))]. *)
let query_function ~loc function_body_factory connection_function_expr
    expression_contents =
  (* Tuples should have duplicates if they exist. *)
  let body =
    function_body_factory ~loc connection_function_expr expression_contents
  in
  let in_params = expression_contents.in_params in
  if List.is_empty in_params then [%expr fun () -> [%e body]]
  else
    let deduped_in_params =
      match Query.remove_duplicates in_params with
      | Ok deduplicated -> deduplicated
      | Error _ ->
          raise (Error "Duplicated input parameters with conflicting specs")
    in
    let f in_param body_so_far =
      let name = in_param.Query.name in
      let pattern = Buildef.ppat_var ~loc (Loc.make ~loc name) in
      Buildef.pexp_fun ~loc (Labelled name) None pattern body_so_far
    in
    List.fold_right ~f ~init:body deduped_in_params

let exec_function ~loc = query_function ~loc function_body_exec [%expr Db.exec]

let find_function ~loc = query_function ~loc function_body_find [%expr Db.find]

let find_opt_function ~loc =
  query_function ~loc function_body_find_opt [%expr Db.find_opt]

let collect_list_function ~loc =
  query_function ~loc function_body_collect [%expr Db.collect_list]
