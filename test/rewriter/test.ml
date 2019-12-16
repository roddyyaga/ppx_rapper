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
