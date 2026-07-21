(* Simple queries *)
type a = { username: string }
type b = { id: int; username: string }

let many_arg_execute =
  [%rapper
    execute
      {sql|
      UPDATE users
      SET (username, email, bio) = (%string{username}, %string{email}, %string?{bio})
      WHERE id = %int{id}
      |sql}]

let many_arg_get_one_repeated_arg =
  [%rapper
    get_one
      {sql|
      SELECT @string{username}
      FROM users
      WHERE id = %int{id} OR username = %string{username} OR id <> %int{id}
      |sql}
      record_out]

let many_arg_get_opt =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      WHERE username = %string{username} AND id > %int{min_id}
      |sql}]

(* Using list parameters *)
type list_in = { versions: int list }

let collect_list =
  [%rapper
    get_many
      {sql| SELECT @string{id} from schema_migrations where version in (%list{%int{versions}})|sql}
      record_in]

(* Using custom types *)
module Suit : Rapper.CUSTOM = struct
  type t = Clubs | Diamonds | Hearts | Spades

  let t =
    let encode = function
      | Clubs -> Ok "c"
      | Diamonds -> Ok "d"
      | Hearts -> Ok "h"
      | Spades -> Ok "s"
    in
    let decode = function
      | "c" -> Ok Clubs
      | "d" -> Ok Diamonds
      | "h" -> Ok Hearts
      | "s" -> Ok Spades
      | _ -> Error "invalid suit"
    in
    Caqti_type.(custom ~encode ~decode string)
end

let get_cards =
  [%rapper
    get_many
      {sql| SELECT @int{id}, @Suit{suit} FROM cards WHERE suit <> %Suit{suit} |sql}]

(* Example showing the correspondence between rapper/Caqti types and OCaml types *)
type all_types_output = {
  id: string;
  payload: string;
  version: int;
  some_int32: int32;
  some_int64: int64;
  added: bool;
  fl: float;
  date: Ptime.t;
  time: Ptime.t;
  span: Ptime.span;
}

let all_types =
  [%rapper
    get_many
      {sql| SELECT @string{id}, @octets{payload}, @int{version},
                @int32{some_int32}, @int64{some_int64}, @bool{added},
                @float{fl}, @pdate{date}, @ptime{time}, @ptime_span{span}
         FROM some_table |sql}
      record_out]

(* Example of using [function_out] and [Rapper.load_many] *)
module Twoot = struct
  type t = { id: int; content: string; likes: int }

  let make ~id ~content ~likes = { id; content; likes }
end

module User = struct
  type t = { id: int; name: string; twoots: Twoot.t list }

  let make ~id ~name = { id; name; twoots = [] }
end

let get_multiple_function_out () dbh =
  let open Lwt_result.Infix in
  [%rapper
    get_many
      {sql|
      SELECT @int{users.id}, @string{users.name},
             @int{twoots.id}, @string{twoots.content}, @int{twoots.likes}
      FROM users
      JOIN twoots ON twoots.id = users.id
      ORDER BY users.id
      |sql}
      function_out]
    (User.make, Twoot.make) () dbh
  >|= Rapper.load_many
        (fst, fun { User.id; _ } -> id)
        [ (snd, fun user twoots -> { user with twoots }) ]

type regclass_response = { to_regclass: string option; lec: int }
type foo = { exists: bool; some_other_param: bool }

let get_something =
  [%rapper
    get_one
      {sql| SELECT @bool{EXISTS} (
          /* @bool{some_other_param} */
  SELECT 1
  FROM pg_tables
  WHERE schemaname = 'schema_name' AND tablename = 'table_name'
); |sql}]

(* Examples of creating caqti connections and using them for queries *)
let main_pooled () =
  let pool =
    Caqti_lwt_unix.connect_pool (Uri.of_string "postgresql://example.com")
    |> Result.get_ok
  in
  Caqti_lwt_unix.Pool.use (get_something ()) pool

let main_not_pooled () =
  Caqti_lwt_unix.with_connection
    (Uri.of_string "postgresql://example.com")
    (get_something ())
