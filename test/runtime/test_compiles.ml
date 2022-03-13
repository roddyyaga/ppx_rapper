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
      {sql|
      SELECT @string{id}
      FROM schema_migrations
      WHERE version in (%list{%int{versions}})
      |sql}
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
  cd: CalendarLib.Date.t;
  ct: CalendarLib.Calendar.t;
}

let all_types =
  [%rapper
    get_many
      {sql|
      SELECT @string{id}, @octets{payload}, @int{version},
                @int32{some_int32}, @int64{some_int64}, @bool{added},
                @float{fl}, @pdate{date}, @ptime{time}, @ptime_span{span},
                @cdate{cd}, @ctime{ct}
      FROM some_table
      |sql}
      record_out]

let e =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}
      FROM users
      WHERE id <> %int{id}
        AND blah IN (%list{%Suit{blahs}})
      |sql}]

let f =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}
      FROM users
      WHERE id <> %int{id}
        AND %int{seriousness} = 5
        AND blah IN (%list{%int{blahs}})
        AND %string{x} = 'x'
      |sql}]

let h =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}
      FROM users
      WHERE blah in (%list{%int{blahs}})
      |sql}]

let i =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}
      FROM users
      WHERE blah in (%list{%int{blahs}})
        AND %int{x} = x
        AND %int{y} = y
      |sql}]

module Double_nested = struct
  module Nested = struct
    module Suit = Suit
  end
end

let nested_modules =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}, @Double_nested.Nested.Suit{suit}
      FROM cards
      WHERE suit <> %Double_nested.Nested.Suit{suit}
      AND username IN (%list{%Double_nested.Nested.Suit{suits}})
      |sql}]

type a' = { a1: int; a2: string }
type b' = { b1: bool }
type c' = { c1: int }

let get_multiple_record_out =
  [%rapper
    get_many
      {sql|
      SELECT @int{c.c1}, @int{a.a1}, @string{a.a2}, @bool{b.b1}
      FROM some_table
      |sql}
      record_out]

let get_cards_function =
  [%rapper
    get_many {sql| SELECT @int{id}, @Suit{suit} FROM cards |sql} function_out]
    (fun ~id ~suit -> (id, suit))

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
