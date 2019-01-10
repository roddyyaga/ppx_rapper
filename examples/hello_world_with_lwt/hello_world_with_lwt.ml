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

(** Database queries using the Ppx_mysql syntax extension. *)

let get_all_users =
  [%mysql select_all "SELECT @int32{id}, @string{name}, @string?{phone} FROM users"]

let get_some_users =
  [%mysql
    select_all
      "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id IN \
       (%list{%int32{id}})"]

let get_user =
  [%mysql
    select_one
      "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id = %int32{id}"]

let insert_user =
  [%mysql
    execute
      "INSERT INTO users (id, name, phone) VALUES (%int32{id}, %string{name}, \
       %string?{phone})"]

let insert_users =
  [%mysql
    execute
      "INSERT INTO users (id, name, phone) VALUES %list{(%int32{id}, %string{name}, \
       %string?{phone})}"]

let update_user =
  [%mysql
    execute
      "UPDATE users SET name = %string{name}, phone = %string?{phone} WHERE id = \
       %int32{id}"]

let delete_user = [%mysql execute "DELETE FROM users WHERE id = %int32{id}"]

(** Main functions and values. *)

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

let test dbh =
  let open IO_result in
  insert_user dbh ~id:1l ~name:"John" ~phone:(Some "123456")
  >>= fun () ->
  insert_user dbh ~id:2l ~name:"Jane" ~phone:None
  >>= fun () ->
  insert_user dbh ~id:3l ~name:"Claire" ~phone:None
  >>= fun () ->
  insert_users dbh [4l, "Mark", None; 5l, "Alice", Some "234567"]
  >>= fun () ->
  get_all_users dbh
  >>= fun users ->
  Lwt_result.ok @@ Lwt_io.printf "All users:\n"
  >>= fun () ->
  Lwt_result.ok @@ Lwt_list.iter_s print_user users
  >>= fun () ->
  get_some_users dbh [1l; 2l; 3l]
  >>= fun users ->
  Lwt_result.ok @@ Lwt_io.printf "Users with ID in {1, 2, 3}:\n"
  >>= fun () ->
  Lwt_result.ok @@ Lwt_list.iter_s print_user users
  >>= fun () ->
  update_user dbh ~id:2l ~name:"Mary" ~phone:(Some "654321")
  >>= fun () ->
  get_user dbh ~id:2l
  >>= fun user ->
  Lwt_result.ok @@ Lwt_io.printf "User with ID = 2 after update:\n"
  >>= fun () ->
  Lwt_result.ok @@ print_user user
  >>= fun () ->
  delete_user dbh ~id:3l
  >>= fun () ->
  get_all_users dbh
  >>= fun users ->
  Lwt_result.ok @@ Lwt_io.printf "All users after deleting one with ID = 3:\n"
  >>= fun () ->
  Lwt_result.ok @@ Lwt_list.iter_s print_user users >>= fun () -> Lwt_result.return ()

let main dbh =
  let open Lwt.Infix in
  test dbh
  >>= function
  | Ok () ->
      Lwt_io.printf "All went well!\n"
  | Error _ ->
      Lwt_io.printf "An error occurred!\n"

let () =
  let dbh = Mysql.quick_connect ~database:"test" ~user:"root" () in
  Lwt_main.run @@ main dbh;
  Mysql.disconnect dbh
