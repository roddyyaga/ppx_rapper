(********************************************************************************)
(* Mysql_with_identity_monad.ml                                                 *)
(********************************************************************************)

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


(********************************************************************************)
(** {1 Required modules for the Ppx_syntax extension}                           *)
(********************************************************************************)

(* The Ppx_mysql syntax extension expects the existence of a module named 'IO'
 * where the IO monad is defined.  For this example we use the identity monad,
 * but normally you'd want to use Lwt or Async.
 *)
module IO =
struct
    type 'a t = 'a
    let return x = x
    let (>>=) x f = f x
end

(* The Ppx_mysql syntax extension also expects the existence of a module named
 * 'Prepared' where functions relating to prepared functions are defined. Mysql's
 * 'Prepared' module satisfies the expected interface and can be used directly.
 *)
module Prepared = Mysql.Prepared


(********************************************************************************)
(** {1 Database queries using the Ppx_mysql syntax extension}                   *)
(********************************************************************************)

let get_users = [%mysql Select_all "SELECT @l{id}, @s{name}, @s?{phone} FROM users"]

let get_user = [%mysql Select_one "SELECT @l{id}, @s{name}, @s?{phone} FROM users WHERE id = %l{id}"]

let insert_user = [%mysql Execute "INSERT INTO users (id, name, phone) VALUES (%l{id}, %s{name}, %s?{phone})"]

let update_user = [%mysql Execute "UPDATE users SET name = %s{name}, phone = %s?{phone} WHERE id = %l{id}"]

let delete_user = [%mysql Execute "DELETE FROM users WHERE id = %l{id}"]


(********************************************************************************)
(** {1 Main functions and values}                                               *)
(********************************************************************************)

let print_user (id, name, phone) =
    Printf.printf "%ld -> %s (phone: %s)\n" id name (match phone with Some p -> p | None -> "--")

let () =
    let result =
        let open Rresult.R in (* For the result monad's (>>=) operator *)
        let dbh = Mysql.quick_connect ~database:"test" ~user:"root" () in
        insert_user dbh ~id:1l ~name:"John Doe" ~phone:(Some "123456") >>= fun () ->
        insert_user dbh ~id:2l ~name:"Jane Doe" ~phone:None >>= fun () ->
        insert_user dbh ~id:3l ~name:"Claire" ~phone:None >>= fun () ->
        delete_user dbh ~id:3l >>= fun () ->
        get_users dbh >>= fun users ->
        List.iter print_user users;
        update_user dbh ~id:2l ~name:"Mary" ~phone:(Some "654321") >>= fun () ->
        get_user dbh ~id:2l >>= fun user ->
        print_user user;
        Mysql.disconnect dbh;
        ok ()
    in match result with
        | Ok ()   -> print_endline "All went well!"
        | Error _ -> print_endline "An error occurred!"
