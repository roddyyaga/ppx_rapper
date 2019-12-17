let many_arg_execute =
  let query =
    Caqti_request.exec
      (let open Caqti_type in
      tup2 string (tup2 string (tup2 (option string) int)))
      "\n\
      \      UPDATE users\n\
      \      SET (username, email, bio) = (?, ?, ?)\n\
      \      WHERE id = ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username ~email ~bio ~id =
    Db.exec query (username, (email, (bio, id)))
  in
  wrapped

let single_arg_execute =
  let query =
    Caqti_request.exec
      (let open Caqti_type in
      string)
      "\n      UPDATE users\n      SET username = ?\n      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username =
    Db.exec query username
  in
  wrapped

let no_arg_execute =
  let query =
    Caqti_request.exec
      (let open Caqti_type in
      unit)
      "\n      UPDATE users\n      SET username = 'Hello!'\n      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) () = Db.exec query () in
  wrapped

let many_arg_get_one =
  let query =
    (let open Caqti_request in
    find)
      (let open Caqti_type in
      tup2 string int)
      (let open Caqti_type in
      tup2 int string)
      "\n\
      \      SELECT id, username\n\
      \      FROM users\n\
      \      WHERE username = ? AND id > ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username ~min_id =
    Db.find query (username, min_id)
  in
  wrapped

let single_arg_get_one =
  let query =
    (let open Caqti_request in
    find)
      (let open Caqti_type in
      string)
      (let open Caqti_type in
      tup2 int string)
      "\n\
      \      SELECT id, username\n\
      \      FROM users\n\
      \      WHERE username = ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username =
    Db.find query username
  in
  wrapped

let no_arg_get_one =
  let query =
    (let open Caqti_request in
    find)
      (let open Caqti_type in
      unit)
      (let open Caqti_type in
      tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) () = Db.find query () in
  wrapped

let many_arg_get_one_repeated_arg =
  let query =
    (let open Caqti_request in
    find)
      (let open Caqti_type in
      tup2 int (tup2 string int))
      (let open Caqti_type in
      tup2 int string)
      "\n\
      \      SELECT id, username\n\
      \      FROM users\n\
      \      WHERE id = ? OR username = ? OR id <> ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~id ~username =
    Db.find query (id, (username, id))
  in
  wrapped

let many_arg_get_opt =
  let query =
    (let open Caqti_request in
    find_opt)
      (let open Caqti_type in
      tup2 string int)
      (let open Caqti_type in
      tup2 int string)
      "\n\
      \      SELECT id, username\n\
      \      FROM users\n\
      \      WHERE username = ? AND id > ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username ~min_id =
    Db.find_opt query (username, min_id)
  in
  wrapped

let single_arg_get_opt =
  let query =
    (let open Caqti_request in
    find_opt)
      (let open Caqti_type in
      string)
      (let open Caqti_type in
      tup2 int string)
      "\n\
      \      SELECT id, username\n\
      \      FROM users\n\
      \      WHERE username = ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username =
    Db.find_opt query username
  in
  wrapped

let no_arg_get_opt =
  let query =
    (let open Caqti_request in
    find_opt)
      (let open Caqti_type in
      unit)
      (let open Caqti_type in
      tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) () = Db.find_opt query () in
  wrapped

let many_arg_get_many =
  let query =
    (let open Caqti_request in
    collect)
      (let open Caqti_type in
      tup2 string int)
      (let open Caqti_type in
      tup2 int string)
      "\n\
      \      SELECT id, username\n\
      \      FROM users\n\
      \      WHERE username = ? AND id > ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username ~min_id =
    Db.collect_list query (username, min_id)
  in
  wrapped

let single_arg_get_many =
  let query =
    (let open Caqti_request in
    collect)
      (let open Caqti_type in
      string)
      (let open Caqti_type in
      tup2 int string)
      "\n\
      \      SELECT id, username\n\
      \      FROM users\n\
      \      WHERE username = ?\n\
      \      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~username =
    Db.collect_list query username
  in
  wrapped

let no_arg_get_many =
  let query =
    (let open Caqti_request in
    collect)
      (let open Caqti_type in
      unit)
      (let open Caqti_type in
      tup2 int string)
      "\n      SELECT id, username\n      FROM users\n      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) () =
    Db.collect_list query ()
  in
  wrapped
