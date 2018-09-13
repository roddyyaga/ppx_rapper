Ppx_mysql
=========

[![Build Status](https://travis-ci.org/issuu/ppx_mysql.svg?branch=master)](https://travis-ci.org/issuu/ppx_mysql)

This syntax extension aims to reduce the pain and boilerplate associated with using
MySQL bindings from OCaml.


Preliminaries
-------------

Throughout this document we reference a SQL table named `employees`, whose MySQL
definition is as follows:

```sql
CREATE TABLE employees
    (
    id INT NOT NULL,
    supervisor_id INT NULL,
    name TEXT NOT NULL,
    phone TEXT NULL,
    PRIMARY KEY (id),
    CONSTRAINT 'fk_supervisor_id' FOREIGN KEY (supervisor_id) REFERENCES employees(id)
    );
```

We also define an OCaml record named `employee` that matches the structure
of the SQL table `employees`:

```ocaml
type employee =
    {
    id: int32;
    supervisor_id: int32 option;
    name: string;
    phone: string option;
    }
```

Assume also the existence of functions for converting to and from a tupled representation
of the `employee` record:

```ocaml
type employee_tuple = int32 * int32 option * string * string option

employee_of_tuple: employee_tuple -> employee
tuple_of_employee: employee -> employee_tuple
```


Setting up the environment
--------------------------

To minimise the amount of boilerplate, this syntax extension generates functions which expect
the existence of the following signature in the current context:

```ocaml
sig
  module IO : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module IO_result : sig
    type ('a, 'e) t = ('a, 'e) result IO.t
    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  module Prepared: sig
    type dbh
    type stmt
    type stmt_result
    type error = [ `Mysql_exception of exn ]

    val create : dbh -> string -> (stmt, [> error ]) result IO.t
    val execute_null : stmt -> string option array -> (stmt_result, [> error ]) result IO.t
    val fetch : stmt_result -> (string option array option, [> error ]) result IO.t
    val close : stmt -> (unit, [> error ]) result IO.t
    val with_stmt : dbh -> string -> (stmt -> ('a, [> error ] as 'e) result IO.t) -> ('a, 'e) result IO.t
  end
end
```

Note that you should **not** manually write the code that satisfies this
signature. Instead, you should use the `Make_context` functor defined in the
`Ppx_mysql_runtime` module, which will produce a module satisfying the above
signature using as argument a module with a much simpler signature. (Please
see the API documentation for details.)

Note also that in most cases you don't have to even worry about calling the
functor yourself.  For your convenience, besides the main `ppx_mysql` package,
you can also find in OPAM the packages `ppx_mysql_identity` / `ppx_mysql_async`
/ `ppx_mysql_lwt`, which define modules `Mysql_with_identity` / `Mysql_with_async`
/ `Mysql_with_lwt` (respectively) for using Mysql with various IO monads, and
which take of all the nitty-gritty of defining a base module and passing it
to the `Make_context` functor.

As an example, to compile the samples in this document using Mysql and the identity
monad for IO, just add package `ppx_mysql_identity` to your project dependencies and
`open Mysql_with_identity` either globally or locally.


Basic usage: selecting a single row
-----------------------------------

Writing a function to fetch one row from the DB is as simple as this:

```ocaml
let get_employee dbh employee_id =
    [%mysql Select_one
    "SELECT @int32{id}, @int32?{supervisor_id}, @string{name}, @string?{phone}
    FROM employees
    WHERE id = %int32{employee_id}"] dbh ~employee_id >>| employee_of_tuple
```

The `%mysql` extension makes all the "magic" happen: it creates a function
that takes as parameter a database handle plus all the input parameters
present in the SQL statement, and returns a tuple with all the output
parameters present in the SQL statement, properly wrapped in a `result`
and `IO` monad.

The "magic" is easier to understand if we explicitly declare the type
of the function created by this extension. We will do so for the rest
of this document.  Note, however, that this explicit declaration is
neither necessary nor recommended for actual code.  Here's the same
`get_employee` function with type annotations:

```ocaml
let get_employee dbh employee_id =
    let q :
        Prepared.dbh ->
        employee_id:int32 ->
        ((int32 * int32 option * string * string option), error) result IO.t =
        [%mysql Select_one
        "SELECT @int32{id}, @int32?{supervisor_id}, @string{name}, @string?{phone}
        FROM employees
        WHERE id = %int32{employee_id}"]  in
    q dbh ~employee_id >>| employee_of_tuple
```

Things to note:

 - Type `Prepared.dbh` is the type of database handles.

 - We denote input parameters using the syntax `%TYPE{name}`, where `TYPE`
   is a type specification (see next section), and `name` is the OCaml named
   parameter that will be part of the generated function's signature.

 - We denote output parameters using the syntax `@TYPE{name}`, where `TYPE`
   is a type specification (see next section), and `name` is the MySQL
   column we are selecting.

 - Both input and output parameters may be `NULL`, which is handled
   by suffixing the type specification with the character `?`
   (Cf. the `supervisor_id` and `phone` columns in this example).

 - The `Select_one` variant immediately after `%mysql` tells the
   extension that the function should return a single value.
   In this case, the value is of type `int32 * int32 option * string * string option`,
   which is wrapped inside a `result IO.t` because errors may occur.


Type specifications
-------------------

Serialization of input parameters and deserialization of output paramters
is done according to provided type specifications. The following list
shows the mapping between the currently implemented type specifications
and their OCaml counterparts. Note that you may always use the name of
the OCaml type.

 - `string`, `TEXT`, `VARCHAR`\*: `string`
 - `int`, `INT`: `int`
 - `int32`: `int32`
 - `int64`: `int64`


Other select queries
--------------------

The query below is a variation on the one above, illustrating a case
getting zero results is perfectly normal and should not be an error.
Note the use of the `Select_opt` variant, which makes the function
return an `option` (wrapped inside a `result IO.t`, because other
errors may still occur).

```ocaml
let get_supervisor dbh employee_id =
    let q :
        Prepared.dbh ->
        employee_id:int32 ->
        ((int32 * int32 option * string * string option) option, error) result IO.t =
        [%mysql Select_opt
        "SELECT @int32{id}, @int32?{supervisor_id}, @string{name}, @string?{phone}
        FROM employees
        WHERE supervisor_id = %int32{employee_id}"] in
    q dbh ~employee_id >>| maybe employee_of_tuple   (* val maybe: ('a -> 'b) -> 'a option -> 'b option *)
```

For queries where multiple (or zero) rows are expected, use the `Select_all`
variant.  The sample below illustrates its use.  Note that the function now
returns a `list` (again wrapped inside a `result IO.t`, because other errors
may occur).

```ocaml
let get_underlings dbh supervisor_id =
    let q :
        Prepared.dbh ->
        supervisor_id:int32 ->
        ((int32 * int32 option * string * string option) list, error) result IO.t =
        [%mysql Select_all
        "SELECT @int32{id}, @int32?{supervisor_id}, @string{name}, @string?{phone}
        FROM employees
        WHERE supervisor_id = %int32{supervisor_id}"] in
    q dbh ~supervisor_id >>| List.map employee_of_tuple
```

Insertions, updates, deletions
------------------------------

We don't really expect a value returned from queries that modify the DB,
such as those that use SQL's `INSERT`, `UPDATE`, and `DELETE` statements.
We use the `Execute` variant for these cases, as the example below illustrates.
Note the use of multiple input parameters, which show up in the function
signature as named parameters in the same order they appear within
the SQL statement (though these being named parameters, one does not
usually need to worry about the order).

```ocaml
let insert_employee {id; supervisor_id; name; phone} =
    let q :
        Prepared.dbh ->
        id:int32 ->
        supervisor_id:int32 option ->
        name:string ->
        phone:string option ->
        (unit, error) result IO.t =
        [%mysql Execute
        "INSERT LO employees (id, supervisor_id, name, phone)
        VALUES (%int32{id}, %int32?{supervisor_id}, %string{name}, %string?{phone}"] in
    q dbh ~id ~supervisor_id ~name ~phone
```

Special cases
-------------

Should there be no input parameters, the function generated by the syntax
extension will take only the database handle as parameter:

```ocaml
let get_unsupervised dbh =
    let q :
        Prepared.dbh ->
        ((int32 * int32 option * string * string option) list, error) result IO.t =
        [%mysql Select_all
        "SELECT @int32{id}, @int32?{supervisor_id}, @string{name}, @string?{phone}
        FROM employees
        WHERE supervisor_id IS NULL"] in
    q dbh >>| List.map employee_of_tuple
```

Should an input parameter with the same name appear multiple times in the
SQL statement, the generated function will take it only once:

```ocaml
let is_related dbh id =
    let q :
        Prepared.dbh ->
        id:int32 ->
        ((int32 * int32 option * string * string option) list, error) result IO.t =
        [%mysql Select_all
        "SELECT @int32{id}, @int32?{supervisor_id}, @string{name}, @string?{phone}
        FROM employees
        WHERE (id = %int32{id} OR supervisor_id = %int32{id}"] in
    q dbh ~id >>| List.map employee_of_tuple
```


Limitations
-----------

All output columns must be specified explicitly, and queries such as
`SELECT * FROM employees` are not supported.  However, since these
queries are brittle and should not be used anyway, this limitation
is unlikely to ever be a problem.  Moreover, note that queries such
as `SELECT @d{count(*)} FROM employees` are supported just fine.


Summary of the query variants
-----------------------------

Below is a summary of all available query variants.

 - `Select_one`: For queries that expect a single row to be returned,
   and where anything else (zero or multiple rows) is an error.

 - `Select_opt`: For queries that may return a single row or none at all.
   Getting multiple rows from the DB is an error.

 - `Select_all`: For queries that expect any number of rows from the DB,
   including zero.

 - `Execute`: For queries that insert, update, or delete data from the DB,
   and where no return value is expected.
