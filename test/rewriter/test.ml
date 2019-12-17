open Core

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
      |sql}]

let no_arg_get_one =
  [%rapper
    get_one
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      |sql}]

let many_arg_get_one_repeated_arg =
  [%rapper
    get_one
      {sql|
      SELECT @int{id}, @string{username}
      FROM users
      WHERE id = %int{id} OR username = %string{username} OR id <> %int{id}
      |sql}]

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
      |sql}]

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
      |sql}]

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
      |sql}]
