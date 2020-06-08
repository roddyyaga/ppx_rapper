# ppx_rapper
An extension that allows named parameters in SQL with types inferred, and syntax checking of SQL as a preprocessing
step. Like [ppx_mysql](https://github.com/issuu/ppx_mysql) but using Caqti/PostgreSQL/Lwt. The name comes from the idea of
[Dapper](https://github.com/StackExchange/Dapper) but with Records.

## Installation
You can install `ppx_rapper` with opam:
```
$ opam install ppx_rapper
```
To use in a project built with dune, add these lines to the relevant stanzas:
```
(libraries ppx_rapper.runtime)
(preprocess (pps ppx_rapper))
```

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
      (let open Caqti_type in
      tup2 string int)
      (let open Caqti_type in
      tup2 int (tup2 string (tup2 bool (option string))))
      "\n\
      \      SELECT id, username, following, bio\n\
      \      FROM users\n\
      \      WHERE username <> ? AND id > ?\n\
      \      "
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

For further examples, see the `examples` directory.

## Query functions
Query functions are
- `execute` for queries that return 0 rows, represented as `()`
- `get_one` for queries that return 1 rows, represented as a tuple/record
- `get_opt` for queries that return 0 or 1 rows, represented as a tuple/record option
- `get_many` for queries that many return any number of rows, represented as a list of tuples/records

These correspond to `exec`, `find`, `find_opt` and `collect` in `Caqti_request`.

Since 1-tuples don't exist, single values are used instead for that case.

## Parameters

Syntax for input/output parameters is the same as ppx\_mysql: `%type{name}` for
inputs and `@type{name}` for outputs. The set of currently supported base types
overlaps with `Caqti`'s: `int`,`int32`,`int64`, `string`, `octets`, `float`,
`bool`, `pdate`, `ptime` and `ptime_span` are supported. Option types can be
specified by appending a `?` to the type specification, e.g.`int?{id}`.

### Custom types

In the style of `ppx_mysql`, `ppx_rapper` also provides (limited) support for
custom types via user-provided encoding and decoding functions. Consider the
following example, adapted from the `mysql_ppx`
[section](ppx_mysql_custom_types) for the same feature:

[ppx_mysql_custom_types]: https://github.com/issuu/ppx_mysql/blob/master/README.md#custom-types-and-deserialization-functions

```ocaml
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
`Ppx_rapper_runtime.CUSTOM` signature, as listed below:

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
  the [documentation](caqti-oneshot-docs). Turning this off is not currently
  supported, but please let us know if you have a use case for it.

[caqti-oneshot-docs]: https://paurkedal.github.io/ocaml-caqti/caqti/Caqti_request/index.html#how-to-dynamically-assemble-queries-and-parameters

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

By default, queries are syntax checked using [pg_query-ocaml](https://github.com/roddyyaga/pg_query-ocaml) and the
extension will error if syntax checking fails. If this gives a false positive error for a query it can be suppressed using the `syntax_off` option.

## Contributions
Contributions are very welcome!
