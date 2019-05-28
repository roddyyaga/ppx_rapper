let test_no_params dbh =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return (Result.Ok ("SELECT TRUE", [||])) >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 0
    then Result.Ok ()
    else Result.Error (`Unexpected_number_of_columns (len_row, 0))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_single_output_params dbh =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return (Result.Ok ("SELECT name FROM users WHERE id = 1", [||]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 1
    then
      let err_accum = [] in
      match
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(0)
      with
      | Option.Some res, _ -> Result.Ok res
      | Option.None, err -> Result.Error (`Column_errors err)
    else Result.Error (`Unexpected_number_of_columns (len_row, 1))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_pair_output_params dbh =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return (Result.Ok ("SELECT id, name FROM users WHERE id = 1", [||]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_one_input_params dbh ~(id : int) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT name FROM users WHERE id = ?",
         [|Option.Some (Pervasives.string_of_int id)|] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 1
    then
      let err_accum = [] in
      match
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(0)
      with
      | Option.Some res, _ -> Result.Ok res
      | Option.None, err -> Result.Error (`Column_errors err)
    else Result.Error (`Unexpected_number_of_columns (len_row, 1))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_two_input_pair_output_params dbh ~(id : int) ~(name : string) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT id, name FROM users WHERE id = ? OR name = ?",
         [| Option.Some (Pervasives.string_of_int id);
            Option.Some (Ppx_mysql_runtime.identity name) |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_select_all dbh =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return (Result.Ok ("SELECT id, name FROM users", [||])) >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= function
          | Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop (row' :: acc)
            | Result.Error _ as err -> IO.return err )
          | Option.None -> IO.return (Result.Ok (List.rev acc))
        in
        loop [] )
        () )

let test_repeated_input_params dbh ~(id : int) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT id, name FROM users WHERE id <> ? AND id <> ?",
         [| Option.Some (Pervasives.string_of_int id);
            Option.Some (Pervasives.string_of_int id) |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= function
          | Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop (row' :: acc)
            | Result.Error _ as err -> IO.return err )
          | Option.None -> IO.return (Result.Ok (List.rev acc))
        in
        loop [] )
        () )

let test_select_opt dbh ~(id : int) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT id, name FROM users WHERE id = ?",
         [|Option.Some (Pervasives.string_of_int id)|] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
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
        loop [] )
        () )

let test_execute dbh ~(id : int) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ("DELETE FROM users WHERE id = ?", [|Option.Some (Pervasives.string_of_int id)|]))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 0
    then Result.Ok ()
    else Result.Error (`Unexpected_number_of_columns (len_row, 0))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        Prepared.fetch stmt_result >>= function
        | Option.Some _ -> IO.return (Result.Error `Expected_none_found_one)
        | Option.None -> IO.return (Result.Ok ()) )
        () )

let test_int dbh ~(a : int) ~(b : int option) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?",
         [| Option.Some (Pervasives.string_of_int a);
            (Option.map Pervasives.string_of_int) b |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_int32 dbh ~(a : int32) ~(b : int32 option) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?",
         [|Option.Some (Int32.to_string a); (Option.map Int32.to_string) b|] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.int32_of_string
          "Ppx_mysql_runtime.int32_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.int32_of_string
          "Ppx_mysql_runtime.int32_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_int64 dbh ~(a : int64) ~(b : int64 option) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?",
         [|Option.Some (Int64.to_string a); (Option.map Int64.to_string) b|] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.int64_of_string
          "Ppx_mysql_runtime.int64_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.int64_of_string
          "Ppx_mysql_runtime.int64_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_bool dbh ~(a : bool) ~(b : bool option) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?",
         [| Option.Some (Pervasives.string_of_bool a);
            (Option.map Pervasives.string_of_bool) b |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.bool_of_string
          "Ppx_mysql_runtime.bool_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.bool_of_string
          "Ppx_mysql_runtime.bool_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_string dbh ~(a : string) ~(b : string option) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?",
         [| Option.Some (Ppx_mysql_runtime.identity a);
            (Option.map Ppx_mysql_runtime.identity) b |] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_custom_type dbh ~(a : Id.t) ~(b : Phone.t option) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  IO.return
    (Result.Ok
       ( "SELECT a, b FROM users where a = ? OR b = ?",
         [|Option.Some (Id.to_mysql a); (Option.map Phone.to_mysql) b|] ))
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "a"
          Id.of_mysql
          "Id.of_mysql"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_nullable_column
          1
          "b"
          Phone.of_mysql
          "Phone.of_mysql"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= fun maybe_row ->
          match acc, maybe_row with
          | [], Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop [row']
            | Result.Error _ as err -> IO.return err )
          | [], Option.None -> IO.return (Result.Error `Expected_one_found_none)
          | _ :: _, Option.Some _ -> IO.return (Result.Error `Expected_one_found_many)
          | hd :: _, Option.None -> IO.return (Result.Ok hd)
        in
        loop [] )
        () )

let test_list0 dbh elems =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  ( match elems with
  | [] -> IO.return (Result.Error `Empty_input_list)
  | elems ->
      let subsqls = List.map (fun _ -> "?") elems in
      let patch = String.concat ", " subsqls in
      let sql =
        String.append
          "SELECT id, name FROM users WHERE id IN ("
          (String.append patch ")")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map (fun id -> [Option.Some (Pervasives.string_of_int id)]) elems))
      in
      let params = Array.concat [[||]; params_between; [||]] in
      IO.return (Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= function
          | Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop (row' :: acc)
            | Result.Error _ as err -> IO.return err )
          | Option.None -> IO.return (Result.Ok (List.rev acc))
        in
        loop [] )
        () )

let test_list1 dbh elems =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  ( match elems with
  | [] -> IO.return (Result.Error `Empty_input_list)
  | elems ->
      let subsqls = List.map (fun _ -> "(?, ?, NULL)") elems in
      let patch = String.concat ", " subsqls in
      let sql =
        String.append
          "INSERT INTO users (id, name, phone) VALUES "
          (String.append patch "")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map
                (fun (id, name) ->
                  [ Option.Some (Pervasives.string_of_int id);
                    Option.Some (Ppx_mysql_runtime.identity name) ] )
                elems))
      in
      let params = Array.concat [[||]; params_between; [||]] in
      IO.return (Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 0
    then Result.Ok ()
    else Result.Error (`Unexpected_number_of_columns (len_row, 0))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        Prepared.fetch stmt_result >>= function
        | Option.Some _ -> IO.return (Result.Error `Expected_none_found_one)
        | Option.None -> IO.return (Result.Ok ()) )
        () )

let test_list2 dbh elems ~(name : string) ~(age : int) =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  ( match elems with
  | [] -> IO.return (Result.Error `Empty_input_list)
  | elems ->
      let subsqls = List.map (fun _ -> "?") elems in
      let patch = String.concat ", " subsqls in
      let sql =
        String.append
          "SELECT id, name FROM users WHERE name = ? OR id IN ("
          (String.append patch ") OR age > ?")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map (fun id -> [Option.Some (Pervasives.string_of_int id)]) elems))
      in
      let params =
        Array.concat
          [ [|Option.Some (Ppx_mysql_runtime.identity name)|];
            params_between;
            [|Option.Some (Pervasives.string_of_int age)|] ]
      in
      IO.return (Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= function
          | Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop (row' :: acc)
            | Result.Error _ as err -> IO.return err )
          | Option.None -> IO.return (Result.Ok (List.rev acc))
        in
        loop [] )
        () )

let test_list3 dbh elems =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  ( match elems with
  | [] -> IO.return (Result.Error `Empty_input_list)
  | elems ->
      let subsqls = List.map (fun _ -> "(?, ?, ?, ?)") elems in
      let patch = String.concat ", " subsqls in
      let sql =
        String.append
          "INSERT INTO users (id, name, real_name, age) VALUES "
          (String.append patch "")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map
                (fun (id, name, age) ->
                  [ Option.Some (Pervasives.string_of_int id);
                    Option.Some (Ppx_mysql_runtime.identity name);
                    Option.Some (Ppx_mysql_runtime.identity name);
                    Option.Some (Pervasives.string_of_int age) ] )
                elems))
      in
      let params = Array.concat [[||]; params_between; [||]] in
      IO.return (Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 0
    then Result.Ok ()
    else Result.Error (`Unexpected_number_of_columns (len_row, 0))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        Prepared.fetch stmt_result >>= function
        | Option.Some _ -> IO.return (Result.Error `Expected_none_found_one)
        | Option.None -> IO.return (Result.Ok ()) )
        () )

let test_cached0 dbh elems =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  ( match elems with
  | [] -> IO.return (Result.Error `Empty_input_list)
  | elems ->
      let subsqls = List.map (fun _ -> "?") elems in
      let patch = String.concat ", " subsqls in
      let sql =
        String.append
          "SELECT id, name FROM users WHERE id IN ("
          (String.append patch ")")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map (fun id -> [Option.Some (Pervasives.string_of_int id)]) elems))
      in
      let params = Array.concat [[||]; params_between; [||]] in
      IO.return (Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_cached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= function
          | Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop (row' :: acc)
            | Result.Error _ as err -> IO.return err )
          | Option.None -> IO.return (Result.Ok (List.rev acc))
        in
        loop [] )
        () )

let test_cached1 dbh elems =
  let open IO_result in
  let module Array = Ppx_mysql_runtime.Stdlib.Array in
  let module List = Ppx_mysql_runtime.Stdlib.List in
  let module Option = Ppx_mysql_runtime.Stdlib.Option in
  let module String = Ppx_mysql_runtime.Stdlib.String in
  let module Result = Ppx_mysql_runtime.Stdlib.Result in
  ( match elems with
  | [] -> IO.return (Result.Error `Empty_input_list)
  | elems ->
      let subsqls = List.map (fun _ -> "?") elems in
      let patch = String.concat ", " subsqls in
      let sql =
        String.append
          "SELECT id, name FROM users WHERE id IN ("
          (String.append patch ")")
      in
      let params_between =
        Array.of_list
          (List.concat
             (List.map (fun id -> [Option.Some (Pervasives.string_of_int id)]) elems))
      in
      let params = Array.concat [[||]; params_between; [||]] in
      IO.return (Result.Ok (sql, params)) )
  >>= fun (sql, params) ->
  let process_out_params row =
    let len_row = Array.length row in
    if Ppx_mysql_runtime.Stdlib.( = ) len_row 2
    then
      let err_accum = [] in
      let col0, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          0
          "id"
          Ppx_mysql_runtime.int_of_string
          "Ppx_mysql_runtime.int_of_string"
          err_accum
          row.(0)
      in
      let col1, err_accum =
        Ppx_mysql_runtime.deserialize_non_nullable_column
          1
          "name"
          Ppx_mysql_runtime.string_of_string
          "Ppx_mysql_runtime.string_of_string"
          err_accum
          row.(1)
      in
      match col0, col1 with
      | Option.Some v0, Option.Some v1 -> Result.Ok (v0, v1)
      | _ -> Result.Error (`Column_errors err_accum)
    else Result.Error (`Unexpected_number_of_columns (len_row, 2))
    [@@warning "-26"]
  in
  Prepared.with_stmt_uncached dbh sql (fun stmt ->
      Prepared.execute_null stmt params >>= fun stmt_result ->
      (fun () ->
        let rec loop acc =
          Prepared.fetch stmt_result >>= function
          | Option.Some row -> (
            match process_out_params row with
            | Result.Ok row' -> loop (row' :: acc)
            | Result.Error _ as err -> IO.return err )
          | Option.None -> IO.return (Result.Ok (List.rev acc))
        in
        loop [] )
        () )
