# ppx_rapper

Like ppx\_mysql (uses the same query syntax) but with Caqti/PostgreSQL/LWT as the backend.

## Use
```ocaml
let my_query =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}, @bool{following}, @string?{bio}
      FROM users
      WHERE username <> %string{wrong_user} AND id > %int{min_id}
      |sql}]
```
turns into

```ocaml
let my_query =
  let request =
    Caqti_request.find_opt
      Caqti_type.(tup2 string int)
      Caqti_type.(tup2 int (tup2 string (tup2 bool (option string))))
      {sql|
      SELECT id, username, following, bio
      FROM users
      WHERE username <> ? AND id > ?
      |sql}
  in
  let query (module Db : Caqti_lwt.CONNECTION) ~wrong_user ~min_id =
    Db.find_opt query (wrong_user, min_id)
  in
  query
```

Query functions are
- `execute` for queries that return 0 rows, represented as `()`
- `get_one` for queries that return 1 rows, represented as a tuple
- `get_opt` for queries that return 0 or 1 rows, represented as a tuple option
- `get_many` for queries that many return any number of rows, represented as a list of tuples

These correspond to `exec`, `find`, `find_opt` and `collect` in `Caqti_request`.
