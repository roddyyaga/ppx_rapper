let test_no_params dbh =
  let open IO_result in
  let query = "SELECT TRUE" in
  let params = [||] in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok () with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 0))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_single_output_params dbh =
  let open IO_result in
  let query = "SELECT name FROM users WHERE id = 1" in
  let params = [||] in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 1
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          (Ppx_mysql_runtime.Stdlib.Option.get
             ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                (Ppx_mysql_runtime.Stdlib.Array.get row 0)))
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 1))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_pair_output_params dbh =
  let open IO_result in
  let query = "SELECT id, name FROM users WHERE id = 1" in
  let params = [||] in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 1)) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_one_input_params dbh ~(id : int) =
  let open IO_result in
  let query = "SELECT name FROM users WHERE id = ?" in
  let params = [|Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)|] in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 1
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          (Ppx_mysql_runtime.Stdlib.Option.get
             ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                (Ppx_mysql_runtime.Stdlib.Array.get row 0)))
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 1))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_two_input_pair_output_params dbh ~(id : int) ~(name : string) =
  let open IO_result in
  let query = "SELECT id, name FROM users WHERE id = ? OR name = ?" in
  let params =
    [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)
     ; Ppx_mysql_runtime.Stdlib.Option.Some (Ppx_mysql_runtime.identity name) |]
  in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 1)) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_select_all dbh =
  let open IO_result in
  let query = "SELECT id, name FROM users" in
  let params = [||] in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 1)) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
            (Ppx_mysql_runtime.Stdlib.Result.Ok (Ppx_mysql_runtime.Stdlib.List.rev acc))
    in
    loop [] )
    ()


let test_repeated_input_params dbh ~(id : int) =
  let open IO_result in
  let query = "SELECT id, name FROM users WHERE id <> ? AND id <> ?" in
  let params =
    [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)
     ; Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id) |]
  in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 1)) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
            (Ppx_mysql_runtime.Stdlib.Result.Ok (Ppx_mysql_runtime.Stdlib.List.rev acc))
    in
    loop [] )
    ()


let test_select_opt dbh ~(id : int) =
  let open IO_result in
  let query = "SELECT id, name FROM users WHERE id = ?" in
  let params = [|Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)|] in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 1)) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
            (Ppx_mysql_runtime.Stdlib.Result.Ok Ppx_mysql_runtime.Stdlib.Option.None)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return
            (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_maybe_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return
            (Ppx_mysql_runtime.Stdlib.Result.Ok (Ppx_mysql_runtime.Stdlib.Option.Some hd))
    in
    loop [] )
    ()


let test_execute dbh ~(id : int) =
  let open IO_result in
  let query = "DELETE FROM users WHERE id = ?" in
  let params = [|Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id)|] in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok () with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 0))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
    Prepared.fetch stmt_result
    >>= function
    | Ppx_mysql_runtime.Stdlib.Option.Some _ ->
        IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_none_found_one)
    | Ppx_mysql_runtime.Stdlib.Option.None ->
        IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ()) )
    ()


let test_int dbh ~(a : int) ~(b : int option) =
  let open IO_result in
  let query = "SELECT a, b FROM users where a = ? OR b = ?" in
  let params =
    [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int a)
     ; (Ppx_mysql_runtime.Stdlib.Option.map Pervasives.string_of_int) b |]
  in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , (Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int_of_string_exn)
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_int32 dbh ~(a : int32) ~(b : int32 option) =
  let open IO_result in
  let query = "SELECT a, b FROM users where a = ? OR b = ?" in
  let params =
    [| Ppx_mysql_runtime.Stdlib.Option.Some (Int32.to_string a)
     ; (Ppx_mysql_runtime.Stdlib.Option.map Int32.to_string) b |]
  in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int32_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , (Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int32_of_string_exn)
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_int64 dbh ~(a : int64) ~(b : int64 option) =
  let open IO_result in
  let query = "SELECT a, b FROM users where a = ? OR b = ?" in
  let params =
    [| Ppx_mysql_runtime.Stdlib.Option.Some (Int64.to_string a)
     ; (Ppx_mysql_runtime.Stdlib.Option.map Int64.to_string) b |]
  in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int64_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , (Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.int64_of_string_exn)
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_bool dbh ~(a : bool) ~(b : bool option) =
  let open IO_result in
  let query = "SELECT a, b FROM users where a = ? OR b = ?" in
  let params =
    [| Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_bool a)
     ; (Ppx_mysql_runtime.Stdlib.Option.map Pervasives.string_of_bool) b |]
  in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.bool_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , (Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.bool_of_string_exn)
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()


let test_string dbh ~(a : string) ~(b : string option) =
  let open IO_result in
  let query = "SELECT a, b FROM users where a = ? OR b = ?" in
  let params =
    [| Ppx_mysql_runtime.Stdlib.Option.Some (Ppx_mysql_runtime.identity a)
     ; (Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity) b |]
  in
  let[@warning "-26"] process_out_params row =
    let ( = ) = Ppx_mysql_runtime.Stdlib.( = ) in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ( Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))
          , (Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
              (Ppx_mysql_runtime.Stdlib.Array.get row 1) )
      with
      | Ppx_mysql_runtime.Deserialization_error (f, v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else Ppx_mysql_runtime.Stdlib.Result.Error (`Unexpected_number_of_rows (len_row, 2))
  in
  Prepared.with_stmt dbh query
  @@ fun stmt ->
  Prepared.execute_null stmt params
  >>= fun stmt_result ->
  (fun () ->
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
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_none)
      | _ :: _, Ppx_mysql_runtime.Stdlib.Option.Some _ ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Error `Expected_one_found_many)
      | hd :: _, Ppx_mysql_runtime.Stdlib.Option.None ->
          IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd)
    in
    loop [] )
    ()
