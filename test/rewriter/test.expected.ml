open Core
type a = {
  username: string }
type b = {
  id: int ;
  username: string }
type c = {
  id: int ;
  username: string ;
  email: string }
let many_arg_execute =
  let query =
    (let open Caqti_request in exec)
      (let open Caqti_type in
         tup2 string (tup2 string (tup2 (option string) int)))
      "\n      UPDATE users\n      SET (username, email, bio) = (?, ?, ?)\n      WHERE id = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username 
    ~email  ~bio  ~id  = Db.exec query (username, (email, (bio, id))) in
  wrapped
let single_arg_execute =
  let query =
    (let open Caqti_request in exec) (let open Caqti_type in string)
      "\n      UPDATE users\n      SET username = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username  =
    Db.exec query username in
  wrapped
let no_arg_execute =
  let query =
    (let open Caqti_request in exec) (let open Caqti_type in unit)
      "\n      UPDATE users\n      SET username = 'Hello!'\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    Db.exec query () in
  wrapped
let many_arg_get_one =
  let query =
    (let open Caqti_request in find) (let open Caqti_type in tup2 string int)
      (let open Caqti_type in
         tup2 int (tup2 string (tup2 (option string) bool)))
      "\n      SELECT id, username, bio, is_married\n      FROM users\n      WHERE username = ? AND id > ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username 
    ~min_id  =
    let f result =
      match result with
      | Ok (id, (username, (bio, is_married))) ->
          (id, username, bio, is_married)
      | Error e -> e in
    Lwt.map f (Db.find query (username, min_id)) in
  wrapped
let single_arg_get_one =
  let query =
    (let open Caqti_request in find) (let open Caqti_type in string)
      (let open Caqti_type in tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username  =
    let f result =
      match result with
      | Ok (id, username) -> { id; username }
      | Error e -> e in
    Lwt.map f (Db.find query username) in
  wrapped
let no_arg_get_one =
  let query =
    (let open Caqti_request in find) (let open Caqti_type in unit)
      (let open Caqti_type in tup2 int (tup2 string string))
      "\n      SELECT id, username, email\n      FROM users\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    let f result =
      match result with
      | Ok (id, (username, email)) -> { id; username; email }
      | Error e -> e in
    Lwt.map f (Db.find query ()) in
  wrapped
let many_arg_get_one_repeated_arg =
  let query =
    (let open Caqti_request in find)
      (let open Caqti_type in tup2 int (tup2 string int))
      (let open Caqti_type in string)
      "\n      SELECT username\n      FROM users\n      WHERE id = ? OR username = ? OR id <> ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~id  ~username 
    =
    let f result =
      match result with | Ok username -> { username } | Error e -> e in
    Lwt.map f (Db.find query (id, (username, id))) in
  wrapped
let many_arg_get_opt =
  let query =
    (let open Caqti_request in find_opt)
      (let open Caqti_type in tup2 string int)
      (let open Caqti_type in tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ? AND id > ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username 
    ~min_id  =
    let f result =
      let g (id, username) = (id, username) in
      let f =
        (fun f -> fun x -> match x with | Some x -> Some (f x) | None -> None)
          g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.find_opt query (username, min_id)) in
  wrapped
let single_arg_get_opt =
  let query =
    (let open Caqti_request in find_opt) (let open Caqti_type in string)
      (let open Caqti_type in tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username  =
    let f result =
      let g (id, username) = { id; username } in
      let f =
        (fun f -> fun x -> match x with | Some x -> Some (f x) | None -> None)
          g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.find_opt query username) in
  wrapped
let no_arg_get_opt =
  let query =
    (let open Caqti_request in find_opt) (let open Caqti_type in unit)
      (let open Caqti_type in tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    let f result =
      let g (id, username) = (id, username) in
      let f =
        (fun f -> fun x -> match x with | Some x -> Some (f x) | None -> None)
          g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.find_opt query ()) in
  wrapped
let many_arg_get_many =
  let query =
    (let open Caqti_request in collect)
      (let open Caqti_type in tup2 string int)
      (let open Caqti_type in tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ? AND id > ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username 
    ~min_id  =
    let f result =
      let g (id, username) = { id; username } in
      let f = List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query (username, min_id)) in
  wrapped
let single_arg_get_many =
  let query =
    (let open Caqti_request in collect) (let open Caqti_type in string)
      (let open Caqti_type in tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username  =
    let f result =
      let g (id, username) = (id, username) in
      let f = List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query username) in
  wrapped
let no_arg_get_many =
  let query =
    (let open Caqti_request in collect) (let open Caqti_type in unit)
      (let open Caqti_type in tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    let f result =
      let g (id, username) = { id; username } in
      let f = List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query ()) in
  wrapped
let my_query =
  let query =
    (let open Caqti_request in find_opt)
      (let open Caqti_type in tup2 string int)
      (let open Caqti_type in
         tup2 int (tup2 string (tup2 bool (option string))))
      "\n      SELECT id, username, following, bio\n      FROM users\n      WHERE username <> ? AND id > ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~wrong_user 
    ~min_id  =
    let f result =
      let g (id, (username, (following, bio))) =
        (id, username, following, bio) in
      let f =
        (fun f -> fun x -> match x with | Some x -> Some (f x) | None -> None)
          g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.find_opt query (wrong_user, min_id)) in
  wrapped
let list =
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~following  ~ids
     =
    match ids with
    | [] ->
        Lwt_result.fail
          (let open Caqti_error in
             encode_rejected ~uri:Uri.empty ~typ:Caqti_type.unit
               (Msg "Empty list"))
    | elems ->
        let subsqls = List.map (fun _ -> "?") elems in
        let patch = String.concat ", " subsqls in
        let sql =
          "\n      SELECT id, username, following, bio\n      FROM users\n      WHERE following = ? and username IN ("
            ^ (patch ^ ")\n      ") in
        let open Ppx_rapper_runtime in
          let Dynparam.Pack (packed_list_type, ids) =
            List.fold_left
              (fun pack ->
                 fun item ->
                   Dynparam.add (let open Caqti_type in int) item pack)
              Dynparam.empty elems in
          let query =
            (let open Caqti_request in find_opt) ~oneshot:true
              (let open Caqti_type in tup2 bool packed_list_type)
              (let open Caqti_type in
                 tup2 int (tup2 string (tup2 bool (option string)))) sql in
          let f result =
            let g (id, (username, (following, bio))) =
              (id, username, following, bio) in
            let f =
              (fun f ->
                 fun x -> match x with | Some x -> Some (f x) | None -> None)
                g in
            match result with | Ok x -> Ok (f x) | Error e -> Error e in
          Lwt.map f (Db.find_opt query (following, ids)) in
  wrapped
let collect_list =
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~versions  =
    match versions with
    | [] ->
        Lwt_result.fail
          (let open Caqti_error in
             encode_rejected ~uri:Uri.empty ~typ:Caqti_type.unit
               (Msg "Empty list"))
    | elems ->
        let subsqls = List.map (fun _ -> "?") elems in
        let patch = String.concat ", " subsqls in
        let sql =
          " SELECT id from schema_migrations where version in (" ^
            (patch ^ ")") in
        let open Ppx_rapper_runtime in
          let Dynparam.Pack (packed_list_type, versions) =
            List.fold_left
              (fun pack ->
                 fun item ->
                   Dynparam.add (let open Caqti_type in int) item pack)
              Dynparam.empty elems in
          let query =
            (let open Caqti_request in collect) ~oneshot:true
              packed_list_type (let open Caqti_type in string) sql in
          Db.collect_list query versions in
  wrapped
