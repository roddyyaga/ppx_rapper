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

open Core
open Async
open Mysql_with_async

let stdout = Lazy.force Writer.stdout

(** Module implementing custom (de)serialization to/from MySQL. *)

module Phone : Ppx_mysql_runtime.SERIALIZABLE with type t = string = struct
  type t = string

  let of_mysql str =
    if String.length str <= 9
    then Ok str
    else Error (`Deserialization_error "string too long")

  let to_mysql str = str
end

(** The user type used throughout this example. *)

type user =
  { id : int32
  ; name : string
  ; phone : Phone.t option }

let user_of_tuple (id, name, phone) = {id; name; phone}

let print_user {id; name; phone} =
  Writer.writef
    stdout
    "\t%ld -> %s (phone: %s)\n"
    id
    name
    ( match phone with
    | Some p ->
        p
    | None ->
        "--" )

(** Database queries using the Ppx_mysql syntax extension. *)

let get_all_users dbh =
  let open Deferred.Result in
  [%mysql select_all "SELECT @int32{id}, @string{name}, @string?{phone} FROM users"] dbh
  >>| List.map ~f:user_of_tuple

let get_some_users dbh ids =
  let open Deferred.Result in
  [%mysql
    select_all
      "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id IN \
       (%list{%int32{id}})"]
    dbh
    ids
  >>| List.map ~f:user_of_tuple

let get_user dbh ~id =
  let open Deferred.Result in
  [%mysql
    select_one
      "SELECT @int32{id}, @string{name}, @string?{phone} FROM users WHERE id = %int32{id}"]
    dbh
    ~id
  >>| user_of_tuple

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

let test dbh =
  let open Deferred.Result in
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
  Writer.writef stdout "All users:\n";
  List.iter ~f:print_user users;
  get_some_users dbh [1l; 2l; 3l]
  >>= fun users ->
  Writer.writef stdout "Users with ID in {1, 2, 3}:\n";
  List.iter ~f:print_user users;
  update_user dbh ~id:2l ~name:"Mary" ~phone:(Some "654321")
  >>= fun () ->
  get_user dbh ~id:2l
  >>= fun user ->
  Writer.writef stdout "User with ID = 2 after update:\n";
  print_user user;
  delete_user dbh ~id:3l
  >>= fun () ->
  get_all_users dbh
  >>= fun users ->
  Writer.writef stdout "All users after deleting one with ID = 3:\n";
  List.iter ~f:print_user users;
  return ()

let main () =
  let open Deferred.Infix in
  let dbh = Mysql.quick_connect ~database:"test" ~user:"root" () in
  test dbh
  >>= fun res ->
  Mysql.disconnect dbh;
  match res with
  | Ok () ->
      Writer.writef stdout "All went well!\n";
      return ()
  | Error _ ->
      Writer.writef stdout "An error occurred!\n";
      return ()

let () = Command.(run @@ async ~summary:"Run Async example" @@ Param.return main)
