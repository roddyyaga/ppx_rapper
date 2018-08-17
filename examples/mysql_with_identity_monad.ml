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

let get_users = [%mysql Select_all "SELECT @INT{id}, @TEXT{name}, @TEXT?{phone} FROM users"]

let get_user = [%mysql Select_one "SELECT @INT{id}, @TEXT{name}, @TEXT?{phone} FROM users WHERE id = %INT{id}"]

let insert_user = [%mysql Execute "INSERT INTO users (id, name, phone) VALUES (%INT{id}, %TEXT{name}, %TEXT?{phone})"]

let update_user = [%mysql Execute "UPDATE users SET name = %TEXT{name}, phone = %TEXT?{phone} WHERE id = %INT{id}"]

let delete_user = [%mysql Execute "DELETE FROM users WHERE id = %INT{id}"]


(********************************************************************************)
(** {1 Main functions and values}                                               *)
(********************************************************************************)

let print_user (id, name, phone) =
    Printf.printf "%Ld -> %s (phone: %s)\n" id name (match phone with Some p -> p | None -> "--")

let () =
    let result =
        let open Rresult.R in (* For the result monad's (>>=) operator *)
        let dbh = Mysql.quick_connect ~database:"test" ~user:"root" () in
        insert_user dbh ~id:1L ~name:"John Doe" ~phone:(Some "123456") >>= fun () ->
        insert_user dbh ~id:2L ~name:"Jane Doe" ~phone:None >>= fun () ->
        insert_user dbh ~id:3L ~name:"Claire" ~phone:None >>= fun () ->
        delete_user dbh ~id:3L >>= fun () ->
        get_users dbh >>= fun users ->
        let () = List.iter print_user users in
        update_user dbh ~id:2L ~name:"Mary" ~phone:(Some "654321") >>= fun () ->
        get_user dbh ~id:2L >>= fun user ->
        let () = print_user user in
        let () = Mysql.disconnect dbh in
        ok ()
    in match result with
        | Ok ()   -> print_endline "All went well!"
        | Error _ -> print_endline "An error occurred!"
