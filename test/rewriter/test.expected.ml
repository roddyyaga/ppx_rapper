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
