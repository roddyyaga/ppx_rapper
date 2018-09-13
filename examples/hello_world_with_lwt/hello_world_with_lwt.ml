(* This example assumes that a Mysql database 'test' exists for user 'root'.
 * Moreover, a table 'users' defined as follows is also present in the DB:
 *
 * CREATE TABLE users
 *     (
 *     id    INT NOT NULL,
 *     name  TEXT NOT NULL,
 *     phone TEXT NULL,
 *     PRIMARY KEY (id)
 *     );
 *)

open Mysql_with_lwt

(** {1 Database queries using the Ppx_mysql syntax extension} *)

let get_users =
  [%mysql Select_all "SELECT @int32{id}, @string{name}, @string?{phone} FROM users"]


let get_user =
  [%mysql
    Select_one
      "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id = %int32{id}"]


let insert_user =
  [%mysql
    Execute
      "INSERT INTO users (id, name, phone) VALUES (%int32{id}, %string{name}, \
       %string?{phone})"]


let update_user =
  [%mysql
    Execute
      "UPDATE users SET name = %string{name}, phone = %string?{phone} WHERE id = \
       %int32{id}"]


let delete_user = [%mysql Execute "DELETE FROM users WHERE id = %int32{id}"]

(** {1 Main functions and values} *)

let print_user (id, name, phone) =
  Lwt_io.printf
    "%ld -> %s (phone: %s)\n"
    id
    name
    ( match phone with
    | Some p ->
        p
    | None ->
        "--" )


let result dbh =
  let open IO_result in
  insert_user dbh ~id:1l ~name:"John Doe" ~phone:(Some "123456") >>= fun () ->
  insert_user dbh ~id:2l ~name:"Jane Doe" ~phone:None >>= fun () ->
  insert_user dbh ~id:3l ~name:"Claire" ~phone:None >>= fun () ->
  delete_user dbh ~id:3l >>= fun () ->
  get_users dbh >>= fun users ->
  let%lwt () = Lwt_list.iter_s print_user users in
  update_user dbh ~id:2l ~name:"Mary" ~phone:(Some "654321") >>= fun () ->
  get_user dbh ~id:2l >>= fun user ->
  let%lwt () = print_user user in
  Lwt.return_ok ()

let main dbh =
  let open Lwt.Infix in
  result dbh >>= function
  | Ok () -> Lwt_io.printlf "All went well!"
  | Error _ -> Lwt_io.printlf "An error occurred!"

let () =
  let dbh = Mysql.quick_connect ~database:"test" ~user:"root" () in
  Lwt_main.run @@ main dbh;
  Mysql.disconnect dbh
