let test_no_params dbh =
  let (>>=) = IO.bind  in
  let query = "SELECT TRUE"  in
  let params = [||]  in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok ()
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 0))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (fun maybe_row  ->
                       match (acc, maybe_row) with
                       | ([],Ppx_mysql_runtime.Stdlib.Option.Some row) ->
                           (match process_out_params row with
                            | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                                loop [row']
                            | Ppx_mysql_runtime.Stdlib.Result.Error _ as err
                                -> IO.return err)
                       | ([],Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_none)
                       | (_::_,Ppx_mysql_runtime.Stdlib.Option.Some _) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_many)
                       | (hd::_,Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_single_output_params dbh =
  let (>>=) = IO.bind  in
  let query = "SELECT name FROM users WHERE id = 1"  in
  let params = [||]  in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 1
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          (Ppx_mysql_runtime.Stdlib.Option.get
             ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                (Ppx_mysql_runtime.Stdlib.Array.get row 0)))
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 1))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (fun maybe_row  ->
                       match (acc, maybe_row) with
                       | ([],Ppx_mysql_runtime.Stdlib.Option.Some row) ->
                           (match process_out_params row with
                            | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                                loop [row']
                            | Ppx_mysql_runtime.Stdlib.Result.Error _ as err
                                -> IO.return err)
                       | ([],Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_none)
                       | (_::_,Ppx_mysql_runtime.Stdlib.Option.Some _) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_many)
                       | (hd::_,Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_pair_output_params dbh =
  let (>>=) = IO.bind  in
  let query = "SELECT id, name FROM users WHERE id = 1"  in
  let params = [||]  in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ((Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map
                  Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))),
            (Ppx_mysql_runtime.Stdlib.Option.get
               ((Ppx_mysql_runtime.Stdlib.Option.map
                   Ppx_mysql_runtime.identity)
                  (Ppx_mysql_runtime.Stdlib.Array.get row 1))))
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 2))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (fun maybe_row  ->
                       match (acc, maybe_row) with
                       | ([],Ppx_mysql_runtime.Stdlib.Option.Some row) ->
                           (match process_out_params row with
                            | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                                loop [row']
                            | Ppx_mysql_runtime.Stdlib.Result.Error _ as err
                                -> IO.return err)
                       | ([],Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_none)
                       | (_::_,Ppx_mysql_runtime.Stdlib.Option.Some _) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_many)
                       | (hd::_,Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_one_input_params dbh ~id:(id : int)  =
  let (>>=) = IO.bind  in
  let query = "SELECT name FROM users WHERE id = ?"  in
  let params =
    [|(Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id))|]
     in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 1
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          (Ppx_mysql_runtime.Stdlib.Option.get
             ((Ppx_mysql_runtime.Stdlib.Option.map Ppx_mysql_runtime.identity)
                (Ppx_mysql_runtime.Stdlib.Array.get row 0)))
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 1))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (fun maybe_row  ->
                       match (acc, maybe_row) with
                       | ([],Ppx_mysql_runtime.Stdlib.Option.Some row) ->
                           (match process_out_params row with
                            | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                                loop [row']
                            | Ppx_mysql_runtime.Stdlib.Result.Error _ as err
                                -> IO.return err)
                       | ([],Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_none)
                       | (_::_,Ppx_mysql_runtime.Stdlib.Option.Some _) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_many)
                       | (hd::_,Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_two_input_pair_output_params dbh ~id:(id : int) 
  ~name:(name : string)  =
  let (>>=) = IO.bind  in
  let query = "SELECT id, name FROM users WHERE id = ? OR name = ?"  in
  let params =
    [|(Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id));(
      Ppx_mysql_runtime.Stdlib.Option.Some (Ppx_mysql_runtime.identity name))|]
     in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ((Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map
                  Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))),
            (Ppx_mysql_runtime.Stdlib.Option.get
               ((Ppx_mysql_runtime.Stdlib.Option.map
                   Ppx_mysql_runtime.identity)
                  (Ppx_mysql_runtime.Stdlib.Array.get row 1))))
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 2))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (fun maybe_row  ->
                       match (acc, maybe_row) with
                       | ([],Ppx_mysql_runtime.Stdlib.Option.Some row) ->
                           (match process_out_params row with
                            | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                                loop [row']
                            | Ppx_mysql_runtime.Stdlib.Result.Error _ as err
                                -> IO.return err)
                       | ([],Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_none)
                       | (_::_,Ppx_mysql_runtime.Stdlib.Option.Some _) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_one_found_many)
                       | (hd::_,Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok hd))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_select_all dbh =
  let (>>=) = IO.bind  in
  let query = "SELECT id, name FROM users"  in
  let params = [||]  in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ((Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map
                  Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))),
            (Ppx_mysql_runtime.Stdlib.Option.get
               ((Ppx_mysql_runtime.Stdlib.Option.map
                   Ppx_mysql_runtime.identity)
                  (Ppx_mysql_runtime.Stdlib.Array.get row 1))))
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 2))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (function
                     | Ppx_mysql_runtime.Stdlib.Option.Some row ->
                         (match process_out_params row with
                          | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                              loop (row' :: acc)
                          | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                              IO.return err)
                     | Ppx_mysql_runtime.Stdlib.Option.None  ->
                         IO.return
                           (Ppx_mysql_runtime.Stdlib.Result.Ok
                              (Ppx_mysql_runtime.Stdlib.List.rev acc)))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_repeated_input_params dbh ~id:(id : int)  =
  let (>>=) = IO.bind  in
  let query = "SELECT id, name FROM users WHERE id <> ? AND id <> ?"  in
  let params =
    [|(Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id));(
      Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id))|]
     in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ((Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map
                  Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))),
            (Ppx_mysql_runtime.Stdlib.Option.get
               ((Ppx_mysql_runtime.Stdlib.Option.map
                   Ppx_mysql_runtime.identity)
                  (Ppx_mysql_runtime.Stdlib.Array.get row 1))))
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 2))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (function
                     | Ppx_mysql_runtime.Stdlib.Option.Some row ->
                         (match process_out_params row with
                          | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                              loop (row' :: acc)
                          | Ppx_mysql_runtime.Stdlib.Result.Error _ as err ->
                              IO.return err)
                     | Ppx_mysql_runtime.Stdlib.Option.None  ->
                         IO.return
                           (Ppx_mysql_runtime.Stdlib.Result.Ok
                              (Ppx_mysql_runtime.Stdlib.List.rev acc)))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_select_opt dbh ~id:(id : int)  =
  let (>>=) = IO.bind  in
  let query = "SELECT id, name FROM users WHERE id = ?"  in
  let params =
    [|(Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id))|]
     in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 2
    then
      try
        Ppx_mysql_runtime.Stdlib.Result.Ok
          ((Ppx_mysql_runtime.Stdlib.Option.get
              ((Ppx_mysql_runtime.Stdlib.Option.map
                  Ppx_mysql_runtime.int_of_string_exn)
                 (Ppx_mysql_runtime.Stdlib.Array.get row 0))),
            (Ppx_mysql_runtime.Stdlib.Option.get
               ((Ppx_mysql_runtime.Stdlib.Option.map
                   Ppx_mysql_runtime.identity)
                  (Ppx_mysql_runtime.Stdlib.Array.get row 1))))
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 2))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                let rec loop acc =
                  (Prepared.fetch stmt_result) >>=
                    (fun maybe_row  ->
                       match (acc, maybe_row) with
                       | ([],Ppx_mysql_runtime.Stdlib.Option.Some row) ->
                           (match process_out_params row with
                            | Ppx_mysql_runtime.Stdlib.Result.Ok row' ->
                                loop [row']
                            | Ppx_mysql_runtime.Stdlib.Result.Error _ as err
                                -> IO.return err)
                       | ([],Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Ok
                                Ppx_mysql_runtime.Stdlib.Option.None)
                       | (_::_,Ppx_mysql_runtime.Stdlib.Option.Some _) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Error
                                `Expected_maybe_one_found_many)
                       | (hd::_,Ppx_mysql_runtime.Stdlib.Option.None ) ->
                           IO.return
                             (Ppx_mysql_runtime.Stdlib.Result.Ok
                                (Ppx_mysql_runtime.Stdlib.Option.Some hd)))
                   in
                loop []) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
let test_execute dbh ~id:(id : int)  =
  let (>>=) = IO.bind  in
  let query = "DELETE FROM users WHERE id = ?"  in
  let params =
    [|(Ppx_mysql_runtime.Stdlib.Option.Some (Pervasives.string_of_int id))|]
     in
  let process_out_params row =
    let (=) = Ppx_mysql_runtime.Stdlib.(=)  in
    let len_row = Ppx_mysql_runtime.Stdlib.Array.length row  in
    if len_row = 0
    then
      try Ppx_mysql_runtime.Stdlib.Result.Ok ()
      with
      | Ppx_mysql_runtime.Deserialization_error (f,v) ->
          Ppx_mysql_runtime.Stdlib.Result.Error
            (`Deserialization_error (f, v))
      | Invalid_argument _ ->
          Ppx_mysql_runtime.Stdlib.Result.Error `Expected_non_null_column
    else
      Ppx_mysql_runtime.Stdlib.Result.Error
        (`Unexpected_number_of_rows (len_row, 0))
    [@@warning "-26"] in
  (Prepared.create dbh query) >>=
    (fun stmt  ->
       (Prepared.execute_null stmt params) >>=
         (fun stmt_result  ->
            ((fun ()  ->
                (Prepared.fetch stmt_result) >>=
                  (function
                   | Ppx_mysql_runtime.Stdlib.Option.Some _ ->
                       IO.return
                         (Ppx_mysql_runtime.Stdlib.Result.Error
                            `Expected_none_found_one)
                   | Ppx_mysql_runtime.Stdlib.Option.None  ->
                       IO.return (Ppx_mysql_runtime.Stdlib.Result.Ok ()))) ())
              >>=
              (fun result  ->
                 (Prepared.close stmt) >>= (fun ()  -> IO.return result))))
  
