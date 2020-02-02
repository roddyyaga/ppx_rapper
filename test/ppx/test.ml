type a = { username: string }

type b = { id: int; username: string }

type c = { id: int; username: string; email: string }

let many_arg_execute =
  [%rapper
    execute
      {sql|
      UPDATE users
      SET (username, email, bio) = (%string{username}, %string{email}, %string?{bio})
      WHERE id = %int{id}
      |sql}]

let single_arg_execute =
  [%rapper
    execute
      {sql|
      UPDATE users
      SET username = %string{username}
      |sql}]

let no_arg_execute =
  [%rapper
    execute {sql|
      UPDATE users
      SET username = 'Hello!'
      |sql}]

let many_arg_get_one =
  [%rapper
    get_one
      {sql|
      SELECT @int{id}, @string{username}, @string?{bio}, @bool{is_married}
      FROM users
      WHERE username = %string{username} AND id > %int{min_id}
      |sql}]

let single_arg_get_one =
  [%rapper
    get_one
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      WHERE username = %string{username}
      |sql}
      record_out]

let no_arg_get_one =
  [%rapper
    get_one
      {sql|
      SELECT @int{id}, @string{username}, @string{email}
      FROM users
      |sql}
      record_out]

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

let single_arg_get_opt =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      WHERE username = %string{username}
      |sql}
      record_out]

let no_arg_get_opt =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      |sql}]

let many_arg_get_many =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      WHERE username = %string{username} AND id > %int{min_id}
      |sql}
      record_out]

let single_arg_get_many =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      WHERE username = %string{username}
      |sql}]

let no_arg_get_many =
  [%rapper
    get_many
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      |sql}
      record_out]

let my_query =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}, @bool{following}, @string?{bio}
      FROM users
      WHERE username <> %string{wrong_user} AND id > %int{min_id}
      |sql}]

let list =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}, @bool{following}, @string?{bio}
      FROM users
      WHERE following = %bool{following} and username IN (%list{%int{ids}})
      |sql}]

let collect_list =
  [%rapper
    get_many
      {sql| SELECT @string{id} from schema_migrations where version in (%list{%int{versions}})|sql}]

module Suit : Ppx_rapper_runtime.CUSTOM = struct
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

let all_types =
  [%rapper
    get_many
      {sql| SELECT @string{id}, @octets{payload}, @int{version},
                @int32{some_int32}, @int64{some_int64}, @bool{added},
                @float{fl}, @pdate{date}, @ptime{time}, @ptime_span{span}
         FROM some_table |sql}]
