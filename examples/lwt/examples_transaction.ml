open Lwt.Syntax
open Lwt.Infix

let pool =
  match Caqti_lwt.connect_pool ~max_size:5 (Uri.of_string "DATABASE_URL") with
  | Ok pool -> pool
  | Error e -> failwith @@ Caqti_error.show e

let dispatch query =
  Caqti_lwt.Pool.use query pool

(** Dispatch a list of queries from left to right, in a transaction. *)
let dispatch_transaction queries =
  let aux (module Connection : Caqti_lwt.CONNECTION) =
    match%lwt Connection.start () with
    | Ok () ->
       let query_result =
         List.fold_left
           (fun a b -> a >>= fun _ -> (b (module Connection : Caqti_lwt.CONNECTION)))
           (Lwt.return (Ok ()))
           queries
       in
       begin
         match%lwt query_result with
         | Ok _ -> Connection.commit ()
         | Error _ as e -> Connection.rollback () >>= fun _ -> Lwt.return e
       end
    | Error _ as e -> Lwt.return e
  in
  dispatch aux

let update_usernames =
  [%rapper
      execute
      {sql|UPDATE users SET name = %string{name} WHERE id = %int{id}|sql}
  ]

let drop_users_table =
  [%rapper
      execute
      {sql|DROP TABLE users|sql}
  ]

let _ =
  let+ result = dispatch_transaction [update_usernames ~name:"bob" ~id:123; drop_users_table ()] in
  match result with
  | Ok () -> print_endline "Yipee!"
  | Error e -> print_endline @@ Caqti_error.show e
