![Build](https://github.com/roddyyaga/ppx_rapper/workflows/Build%20and%20test/badge.svg)

# ppx_rapper
An extension that allows named parameters in SQL with types inferred, and syntax checking of SQL as a preprocessing
step. Like [ppx_mysql](https://github.com/issuu/ppx_mysql) but using Caqti. The name comes from the idea of
[Dapper](https://github.com/StackExchange/Dapper) but with Records.

The syntax checking feature only works for PostgreSQL, but other features should work with other Caqti backends such as MariaDB and SQLite. If you are using a non-Postgres dialect you should use the `syntax_off` option to avoid spurious errors.

## Installation
You can install `ppx_rapper` with opam:
```
$ opam install ppx_rapper ppx_rapper_lwt
```
(or `ppx_rapper_async` if you are using async instead).

To use in a project built with dune, add these lines to the relevant stanzas:
```
(libraries ppx_rapper_lwt)
(preprocess (pps ppx_rapper))
```
or similar for async.

## Example usage
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
      ((let open Caqti_type in
       t2 string int) [@ocaml.warning "-33"])
      ((let open Caqti_type in
       t2 int (t2 string (t2 bool (option string))))
      [@ocaml.warning "-33"])
      "\n\
      \      SELECT id, username, following, bio\n\
      \      FROM users\n\
      \      WHERE username <> ? AND id > ?\n\
      \      "
  in
  let wrapped ~wrong_user ~min_id (module Db : Rapper_helper.CONNECTION) =
    let f result =
      let g (id, (username, (following, bio))) =
        (id, username, following, bio)
      in
      let f =
        (fun f x -> match x with Some x -> Some (f x) | None -> None) g
      in
      match result with Ok x -> Ok (f x) | Error e -> Error e
    in
    Rapper_helper.map f (Db.find_opt query (wrong_user, min_id))
  in
  wrapped
```

For further examples, see the `examples` directory.

## Query functions
Query functions are
- `execute` for queries that return 0 rows, represented as `()`
- `get_one` for queries that return 1 rows, represented as a tuple/record
- `get_opt` for queries that return 0 or 1 rows, represented as a tuple/record option
- `get_many` for queries that may return any number of rows, represented as a list of tuples/records

These correspond to `exec`, `find`, `find_opt` and `collect` in `Caqti_request`.

Since 1-tuples don't exist, single values are used instead for that case.

## Parameters

Syntax for input/output parameters is the same as ppx\_mysql: `%type{name}` for
inputs and `@type{name}` for outputs. The set of currently supported base types
overlaps with `Caqti`'s: `int`,`int32`,`int64`, `string`, `octets`, `float`,
`bool`, `pdate`, `ptime` and `ptime_span` are supported, in addition to `cdate`
and `ctime`, provided by
[caqti-type-calendar](https://paurkedal.github.io/ocaml-caqti/caqti-type-calendar/Caqti_type_calendar/index.html).
Option types can be specified by appending a `?` to the type specification,
e.g.`int?{id}`.

### Custom types

In the style of `ppx_mysql`, `ppx_rapper` also provides (limited) support for
custom types via user-provided encoding and decoding functions. Consider the
following example, adapted from the `mysql_ppx`
[section](ppx_mysql_custom_types) for the same feature:

[ppx_mysql_custom_types]: https://github.com/issuu/ppx_mysql/blob/master/README.md#custom-types-and-deserialization-functions

```ocaml
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
      | _   -> Error "invalid suit"
    in
    Caqti_type.(custom ~encode ~decode string)
end

let get_cards =
  [%rapper get_many
   {sql| SELECT @int{id}, @Suit{suit} FROM cards WHERE suit <> %Suit{suit} |sql}]
```

The syntax extension will recognize type specifications that start with an
uppercase letter  -- `Suit` in our example -- and assume they refer to a module
(available in the scope where the extension is evaluated) that implements the
`Rapper.CUSTOM` signature, as listed below:

```ocaml
module type CUSTOM = sig
  type t

  val t : t Caqti_type.t
end
```

_Note_: custom type support in this syntax extension is fairly limited and not
meant to be used for e.g. composite types in the output. If you intend to get
the return values for your query in a record, there's support for that with
the `record_out` option (described [below](#options)).

### List support for input parameters

`ppx_rapper` has limited support for queries that take a list of values as
input, through the special `%list{}` construct. An example is shown below:

```ocaml
let users =
  [%rapper
    get_opt
      {sql|
      SELECT @int{id}, @string{username}, @bool{following}, @string?{bio}
      FROM users
      WHERE following = %bool{following} and username IN (%list{%int{ids}})
      |sql}]
```

Current limitations for `list` include:

- Only one `list` input parameter is supported at this time;
- Generated Caqti queries are dynamically generated, and thus `oneshot` as per
  the [documentation](https://paurkedal.github.io/ocaml-caqti/caqti/Caqti_request/index.html#how-to-dynamically-assemble-queries-and-parameters). Turning this off is not currently
  supported, but please let us know if you have a use case for it.

## Extension options
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

Instead of `record_out` you can give `function_out`, in which case the first argument to the generated function should
be a function with labelled arguments of the types of the output parameters, like so:

```ocaml
let show_user_names =
  [%rapper
    get_many {sql| SELECT @int{id}, @string{name} FROM users |sql} function_out]
    (fun ~name ~id -> Printf.sprintf "User %d is called %s" id name)
```

By default, queries are syntax checked using [pg_query-ocaml](https://github.com/roddyyaga/pg_query-ocaml) and the
extension will error if syntax checking fails. If you are using a non-Postgres SQL dialect or this gives a false positive error for a query it can be suppressed using the `syntax_off` option.

## Multiple outputs
With the `record_out` or `function_out` option, an output parameter `@type{param_name}` will usually map to a record field name
or labelled argument `param_name`. However, different behaviour occurs if there are output parameters containing dots.
In this case, multiple outputs will be produced. For example:

```ocaml
let get_user_hat =
  [%rapper
    get_one
      {sql|
      SELECT @int{users.user_id}, @string{users.name},
             @int{hats.hat_id}, @string{hats.colour}
      FROM users
      JOIN hats ON hats.hat_id = users.hat_id
      WHERE users.id = 7
      |sql}
      record_out]
```

will produce output with type `{ user_id: int; name: string} * { hat_id: int; colour: string}`. Similarly, with
`function_out` the generated function will take a tuple of loading functions. Ordering of elements of these tuples is given by the order of their first output parameters in the query.

Note that multiple outputs that share field names (for instance `@int{users.id}` and `@int{hats.id}` in the same query)
will not work with `record_out`, but will work fine with `function_out`.

## Loading data with one-to-many relationships
The multiple outputs feature can be used with the runtime function `Rapper.load_many` to conveniently load entities with one-to-many relationships, as in the following example:

```ocaml
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
      JOIN twoots ON twoots.user_id = users.id
      ORDER BY users.id
      |sql}
      function_out]
    (User.make, Twoot.make) () dbh
  >|= Rapper.load_many
        (fst, fun { User.id; _ } -> id)
        [ (snd, fun user twoots -> { user with twoots }) ]
```

Here, the query itself produces a list of tuples, where the first element is a user and the second element is one of
that user's "twoots". The query is sorted by user id, so all twoots belonging to one user are adjacent. Using
`Rapper.load_many` produces a list of the unique users with the `twoots` field filled correctly.

## Contributions
Contributions are very welcome!
