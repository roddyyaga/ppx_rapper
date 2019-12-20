# ppx_rapper

An extension that allows named parameters in SQL with types inferred, and syntax checking of SQL as a preprocessing
step. Like [ppx_mysql](https://github.com/issuu/ppx_mysql) but using Caqti/PostgreSQL/LWT. The name comes from being
[Dapper](https://github.com/StackExchange/Dapper) but with Records.

## Usage
### Example
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
  let query =
    (let open Caqti_request in
    find_opt)
      (let open Caqti_type in
      tup2 string int)
      (let open Caqti_type in
      tup2 int (tup2 string (tup2 bool (option string))))
      "\n\
      \\      SELECT id, username, following, bio\n\
      \\      FROM users\n\
      \\      WHERE username <> ? AND id > ?\n\
      \\      "
  in
  let wrapped (module Db : Caqti_lwt.CONNECTION) ~wrong_user ~min_id =
    let f result =
      let g (id, (username, (following, bio))) =
        (id, username, following, bio)
      in
      Result.map ~f:(Option.map ~f:g) result
    in
    Lwt.map f (Db.find_opt query (wrong_user, min_id))
  in
  wrapped
```

### Query functions
Query functions are
- `execute` for queries that return 0 rows, represented as `()`
- `get_one` for queries that return 1 rows, represented as a tuple/record
- `get_opt` for queries that return 0 or 1 rows, represented as a tuple/record option
- `get_many` for queries that many return any number of rows, represented as a list of tuples/records

These correspond to `exec`, `find`, `find_opt` and `collect` in `Caqti_request`.

### Parameters
Syntax for input/output parameters is the same as ppx\_mysql: `%type{name}` for
inputs and `@type{name}` for outputs. Currently supported base types are `int`, `string` and `bool`, plus options of
those specified with `type?`. Lists and custom types from ppx\_mysql are not implemented yet.

### Options
If `record_in` or `record_out` are given as options like so:
```ocaml
let my_query =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}, @bool{following}, @string?{bio}
      FROM users
      WHERE username <> %string{wrong_user} AND id > %int{min_id}
      |sql}
      record_in record_out]
```
then the input and/or output of the query will be records. For the example above, they would have type `{id: int; wrong_user: string}` and `{id: int; username: string; following: bool; bio: string option}` respectively. The default non-record methods are labelled arguments and tuples respectively.

By default, queries are syntax checked using [pg_query-ocaml](https://github.com/roddyyaga/pg_query-ocaml) and the
extension will error if syntax checking fails. If this gives a false positive error for a query it can be stopped using the `syntax_off` option.


## Requirements
The runtime requirements of Rapper are core, lwt, caqti, caqti-lwt and caqti-driver-postgresql.
