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
      ((let open Caqti_type in
          tup2 string (tup2 string (tup2 (option string) int)))
      [@ocaml.warning "-33"])
      "\n      UPDATE users\n      SET (username, email, bio) = (?, ?, ?)\n      WHERE id = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username 
    ~email  ~bio  ~id  = Db.exec query (username, (email, (bio, id))) in
  wrapped
let single_arg_execute =
  let query =
    (let open Caqti_request in exec) ((let open Caqti_type in string)
      [@ocaml.warning "-33"])
      "\n      UPDATE users\n      SET username = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username  =
    Db.exec query username in
  wrapped
let no_arg_execute =
  let query =
    (let open Caqti_request in exec) ((let open Caqti_type in unit)
      [@ocaml.warning "-33"])
      "\n      UPDATE users\n      SET username = 'Hello!'\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    Db.exec query () in
  wrapped
let many_arg_get_one =
  let query =
    (let open Caqti_request in find)
      ((let open Caqti_type in tup2 string int)[@ocaml.warning "-33"])
      ((let open Caqti_type in
          tup2 int (tup2 string (tup2 (option string) bool)))
      [@ocaml.warning "-33"])
      "\n      SELECT id, username, bio, is_married\n      FROM users\n      WHERE username = ? AND id > ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username 
    ~min_id  =
    let f result =
      match result with
      | Ok (id, (username, (bio, is_married))) ->
          Ok (id, username, bio, is_married)
      | Error e -> Error e in
    Lwt.map f (Db.find query (username, min_id)) in
  wrapped
let single_arg_get_one =
  let query =
    (let open Caqti_request in find) ((let open Caqti_type in string)
      [@ocaml.warning "-33"]) ((let open Caqti_type in tup2 int string)
      [@ocaml.warning "-33"])
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username  =
    let f result =
      match result with
      | Ok (id, username) -> Ok { id; username }
      | Error e -> Error e in
    Lwt.map f (Db.find query username) in
  wrapped
let no_arg_get_one =
  let query =
    (let open Caqti_request in find) ((let open Caqti_type in unit)
      [@ocaml.warning "-33"])
      ((let open Caqti_type in tup2 int (tup2 string string))
      [@ocaml.warning "-33"])
      "\n      SELECT id, username, email\n      FROM users\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    let f result =
      match result with
      | Ok (id, (username, email)) -> Ok { id; username; email }
      | Error e -> Error e in
    Lwt.map f (Db.find query ()) in
  wrapped
let many_arg_get_one_repeated_arg =
  let query =
    (let open Caqti_request in find)
      ((let open Caqti_type in tup2 int (tup2 string int))
      [@ocaml.warning "-33"]) ((let open Caqti_type in string)
      [@ocaml.warning "-33"])
      "\n      SELECT username\n      FROM users\n      WHERE id = ? OR username = ? OR id <> ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~id  ~username 
    =
    let f result =
      match result with | Ok username -> Ok { username } | Error e -> Error e in
    Lwt.map f (Db.find query (id, (username, id))) in
  wrapped
let many_arg_get_opt =
  let query =
    (let open Caqti_request in find_opt)
      ((let open Caqti_type in tup2 string int)[@ocaml.warning "-33"])
      ((let open Caqti_type in tup2 int string)[@ocaml.warning "-33"])
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
    (let open Caqti_request in find_opt) ((let open Caqti_type in string)
      [@ocaml.warning "-33"]) ((let open Caqti_type in tup2 int string)
      [@ocaml.warning "-33"])
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
    (let open Caqti_request in find_opt) ((let open Caqti_type in unit)
      [@ocaml.warning "-33"]) ((let open Caqti_type in tup2 int string)
      [@ocaml.warning "-33"])
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
      ((let open Caqti_type in tup2 string int)[@ocaml.warning "-33"])
      ((let open Caqti_type in tup2 int string)[@ocaml.warning "-33"])
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ? AND id > ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username 
    ~min_id  =
    let f result =
      let g (id, username) = { id; username } in
      let f = Stdlib.List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query (username, min_id)) in
  wrapped
let single_arg_get_many =
  let query =
    (let open Caqti_request in collect) ((let open Caqti_type in string)
      [@ocaml.warning "-33"]) ((let open Caqti_type in tup2 int string)
      [@ocaml.warning "-33"])
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~username  =
    let f result =
      let g (id, username) = (id, username) in
      let f = Stdlib.List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query username) in
  wrapped
let no_arg_get_many =
  let query =
    (let open Caqti_request in collect) ((let open Caqti_type in unit)
      [@ocaml.warning "-33"]) ((let open Caqti_type in tup2 int string)
      [@ocaml.warning "-33"])
      "\n      SELECT id, username\n      FROM users\n      " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    let f result =
      let g (id, username) = { id; username } in
      let f = Stdlib.List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query ()) in
  wrapped
let my_query =
  let query =
    (let open Caqti_request in find_opt)
      ((let open Caqti_type in tup2 string int)[@ocaml.warning "-33"])
      ((let open Caqti_type in
          tup2 int (tup2 string (tup2 bool (option string))))
      [@ocaml.warning "-33"])
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
        let subsqls = Stdlib.List.map (fun _ -> "?") elems in
        let patch = Stdlib.String.concat ", " subsqls in
        let sql =
          "\n      SELECT id, username, following, bio\n      FROM users\n      WHERE following = ? and username IN ("
            ^ (patch ^ ")\n      ") in
        let open Ppx_rapper_runtime in
          let Dynparam.Pack (packed_list_type, ids) =
            Stdlib.List.fold_left
              (fun pack ->
                 fun item ->
                   Dynparam.add ((let open Caqti_type in int)
                     [@ocaml.warning "-33"]) item pack) Dynparam.empty elems in
          let query =
            (let open Caqti_request in find_opt) ~oneshot:true
              (let open Caqti_type in tup2 bool packed_list_type)
              ((let open Caqti_type in
                  tup2 int (tup2 string (tup2 bool (option string))))
              [@ocaml.warning "-33"]) sql in
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
        let subsqls = Stdlib.List.map (fun _ -> "?") elems in
        let patch = Stdlib.String.concat ", " subsqls in
        let sql =
          " SELECT id from schema_migrations where version in (" ^
            (patch ^ ")") in
        let open Ppx_rapper_runtime in
          let Dynparam.Pack (packed_list_type, versions) =
            Stdlib.List.fold_left
              (fun pack ->
                 fun item ->
                   Dynparam.add ((let open Caqti_type in int)
                     [@ocaml.warning "-33"]) item pack) Dynparam.empty elems in
          let query =
            (let open Caqti_request in collect) ~oneshot:true
              packed_list_type ((let open Caqti_type in string)
              [@ocaml.warning "-33"]) sql in
          Db.collect_list query versions in
  wrapped
module Suit : Ppx_rapper_runtime.CUSTOM =
  struct
    type t =
      | Clubs 
      | Diamonds 
      | Hearts 
      | Spades 
    let t =
      let encode =
        function
        | Clubs -> Ok "c"
        | Diamonds -> Ok "d"
        | Hearts -> Ok "h"
        | Spades -> Ok "s" in
      let decode =
        function
        | "c" -> Ok Clubs
        | "d" -> Ok Diamonds
        | "h" -> Ok Hearts
        | "s" -> Ok Spades
        | _ -> Error "invalid suit" in
      let open Caqti_type in custom ~encode ~decode string
  end 
let get_cards =
  let query =
    (let open Caqti_request in collect) ((let open Caqti_type in Suit.t)
      [@ocaml.warning "-33"]) ((let open Caqti_type in tup2 int Suit.t)
      [@ocaml.warning "-33"]) " SELECT id, suit FROM cards WHERE suit <> ? " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~suit  =
    let f result =
      let g (id, suit) = (id, suit) in
      let f = Stdlib.List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query suit) in
  wrapped
let all_types =
  let query =
    (let open Caqti_request in collect) ((let open Caqti_type in unit)
      [@ocaml.warning "-33"])
      ((let open Caqti_type in
          tup2 string
            (tup2 octets
               (tup2 int
                  (tup2 int32
                     (tup2 int64
                        (tup2 bool
                           (tup2 float (tup2 pdate (tup2 ptime ptime_span)))))))))
      [@ocaml.warning "-33"])
      " SELECT id, payload, version,\n                some_int32, some_int64, added,\n                fl, date, time, span\n         FROM some_table " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) () =
    let f result =
      let g
        (id,
         (payload,
          (version,
           (some_int32, (some_int64, (added, (fl, (date, (time, span)))))))))
        =
        (id, payload, version, some_int32, some_int64, added, fl, date, time,
          span) in
      let f = Stdlib.List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query ()) in
  wrapped
module Nested = struct module Suit = Suit end
let get_cards =
  let query =
    (let open Caqti_request in collect) ((let open Caqti_type in Suit.t)
      [@ocaml.warning "-33"])
      ((let open Caqti_type in tup2 int Nested.Suit.t)[@ocaml.warning "-33"])
      " SELECT id, suit FROM cards WHERE suit <> ? " in
  let wrapped ((module Db)  : (module Caqti_lwt.CONNECTION)) ~suit  =
    let f result =
      let g (id, suit) = (id, suit) in
      let f = Stdlib.List.map g in
      match result with | Ok x -> Ok (f x) | Error e -> Error e in
    Lwt.map f (Db.collect_list query suit) in
  wrapped
