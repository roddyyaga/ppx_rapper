open Ppx_mysql

(** {1 Type definitions} *)

type param = Query.param =
  { typ : string option * string
  ; opt : bool
  ; name : string
  ; of_string : string * string
  ; to_string : string * string }
[@@deriving eq, show]

type list_params = Query.list_params =
  { subsql : string
  ; string_index : int
  ; param_index : int
  ; params : param list }
[@@deriving eq, show]

type parsed_query = Query.parsed_query =
  { sql : string
  ; in_params : param list
  ; out_params : param list
  ; list_params : list_params option }
[@@deriving eq, show]

type parse_error =
  [ `Bad_identifier of string
  | `Unknown_type_spec of string
  | `Empty_list_params
  | `Multiple_lists_not_supported
  | `Nested_list
  | `Optional_list
  | `Out_params_in_list
  | `Unterminated_list
  | `Unterminated_string
  | `Unterminated_bracket
  | `Escape_at_end ]
[@@deriving eq, show]

type conflicting_spec = [`Conflicting_spec of string] [@@deriving eq, show]

(** {1 TESTABLE modules}  *)

let param_mod = Alcotest.testable pp_param equal_param

let parsed_query_mod = Alcotest.testable pp_parsed_query equal_parsed_query

let parse_error_mod = Alcotest.testable pp_parse_error equal_parse_error

let conflicting_spec_mod = Alcotest.testable pp_conflicting_spec equal_conflicting_spec

(** {1 Functions and values for {!test_parse_query}} *)

let query_0 = "SELECT true"

let parsed_query_0 =
  {sql = "SELECT true"; in_params = []; out_params = []; list_params = None}

let query_out1 = "SELECT @int64{id} FROM users"

let parsed_query_out1 =
  { sql = "SELECT id FROM users"
  ; in_params = []
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" } ]
  ; list_params = None }

let query_out2 = "SELECT @int64{id}, @string{name} FROM users"

let parsed_query_out2 =
  { sql = "SELECT id, name FROM users"
  ; in_params = []
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; list_params = None }

let query_out3 = "SELECT @int64{id}, @string{name}, @string?{phone} FROM users"

let parsed_query_out3 =
  { sql = "SELECT id, name, phone FROM users"
  ; in_params = []
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" }
      ; { typ = None, "string"
        ; opt = true
        ; name = "phone"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; list_params = None }

let query_out4 = "SELECT @Id{id}, @Name{name}, @Phone?{phone} FROM users"

let parsed_query_out4 =
  { sql = "SELECT id, name, phone FROM users"
  ; in_params = []
  ; out_params =
      [ { typ = Some "Id", "t"
        ; opt = false
        ; name = "id"
        ; of_string = "Id", "of_mysql"
        ; to_string = "Id", "to_mysql" }
      ; { typ = Some "Name", "t"
        ; opt = false
        ; name = "name"
        ; of_string = "Name", "of_mysql"
        ; to_string = "Name", "to_mysql" }
      ; { typ = Some "Phone", "t"
        ; opt = true
        ; name = "phone"
        ; of_string = "Phone", "of_mysql"
        ; to_string = "Phone", "to_mysql" } ]
  ; list_params = None }

let query_in1 = "INSERT INTO users (id) VALUES (%int64{id})"

let parsed_query_in1 =
  { sql = "INSERT INTO users (id) VALUES (?)"
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" } ]
  ; out_params = []
  ; list_params = None }

let query_in2 = "INSERT INTO users (id, name) VALUES (%int64{id}, %string{name})"

let parsed_query_in2 =
  { sql = "INSERT INTO users (id, name) VALUES (?, ?)"
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; out_params = []
  ; list_params = None }

let query_in3 =
  "INSERT INTO users (id, name, phone) VALUES (%int64{id}, %string{name}, \
   %string?{phone})"

let parsed_query_in3 =
  { sql = "INSERT INTO users (id, name, phone) VALUES (?, ?, ?)"
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" }
      ; { typ = None, "string"
        ; opt = true
        ; name = "phone"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; out_params = []
  ; list_params = None }

let query_in4 =
  "INSERT INTO users (id, name, phone) VALUES (%Id{id}, %Name{name}, %Phone?{phone})"

let parsed_query_in4 =
  { sql = "INSERT INTO users (id, name, phone) VALUES (?, ?, ?)"
  ; in_params =
      [ { typ = Some "Id", "t"
        ; opt = false
        ; name = "id"
        ; of_string = "Id", "of_mysql"
        ; to_string = "Id", "to_mysql" }
      ; { typ = Some "Name", "t"
        ; opt = false
        ; name = "name"
        ; of_string = "Name", "of_mysql"
        ; to_string = "Name", "to_mysql" }
      ; { typ = Some "Phone", "t"
        ; opt = true
        ; name = "phone"
        ; of_string = "Phone", "of_mysql"
        ; to_string = "Phone", "to_mysql" } ]
  ; out_params = []
  ; list_params = None }

let query_inout =
  "SELECT @int64{id}, @string{name}, @string?{phone} FROM users WHERE id = %int64{id} \
   OR name = %string{name} OR PHONE = %string?{phone}"

let parsed_query_inout =
  { sql = "SELECT id, name, phone FROM users WHERE id = ? OR name = ? OR PHONE = ?"
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" }
      ; { typ = None, "string"
        ; opt = true
        ; name = "phone"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" }
      ; { typ = None, "string"
        ; opt = true
        ; name = "phone"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; list_params = None }

let query_quoted0 =
  "SELECT @int64{id}, @string{name} FROM users WHERE id = %int64{id} OR NAME = 'Hello \
   @int64{name} world'"

let parsed_query_quoted0 =
  { sql = "SELECT id, name FROM users WHERE id = ? OR NAME = 'Hello @int64{name} world'"
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" } ]
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; list_params = None }

let query_quoted1 =
  "SELECT @int64{id}, @string{name} FROM users WHERE id = %int64{id} OR NAME = \"Hello \
   @int64{name} world\""

let parsed_query_quoted1 =
  { sql = "SELECT id, name FROM users WHERE id = ? OR NAME = \"Hello @int64{name} world\""
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" } ]
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; list_params = None }

let query_quoted2 =
  "SELECT @int64{id}, @string{name} FROM users WHERE id = %int64{id} OR NAME = 'Hello \
   ''@int64{name}'' world'"

let parsed_query_quoted2 =
  { sql =
      "SELECT id, name FROM users WHERE id = ? OR NAME = 'Hello ''@int64{name}'' world'"
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" } ]
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; list_params = None }

let query_quoted3 =
  "SELECT @int64{id}, @string{name} FROM users WHERE id = %int64{id} OR NAME = \"Hello \
   '@int64{name}' world\""

let parsed_query_quoted3 =
  { sql =
      "SELECT id, name FROM users WHERE id = ? OR NAME = \"Hello '@int64{name}' world\""
  ; in_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" } ]
  ; out_params =
      [ { typ = None, "int64"
        ; opt = false
        ; name = "id"
        ; of_string = "Ppx_mysql_runtime", "int64_of_string"
        ; to_string = "Int64", "to_string" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; list_params = None }

let query_list0 =
  "SELECT @int{COUNT(*)} FROM users WHERE age > %int{age} AND id IN (%list{%int64{id}})"

let parsed_query_list0 =
  { sql = "SELECT COUNT(*) FROM users WHERE age > ? AND id IN ()"
  ; in_params =
      [ { typ = None, "int"
        ; opt = false
        ; name = "age"
        ; of_string = "Ppx_mysql_runtime", "int_of_string"
        ; to_string = "Pervasives", "string_of_int" } ]
  ; out_params =
      [ { typ = None, "int"
        ; opt = false
        ; name = "COUNT(*)"
        ; of_string = "Ppx_mysql_runtime", "int_of_string"
        ; to_string = "Pervasives", "string_of_int" } ]
  ; list_params =
      Some
        { subsql = "?"
        ; string_index = 52
        ; param_index = 1
        ; params =
            [ { typ = None, "int64"
              ; opt = false
              ; name = "id"
              ; of_string = "Ppx_mysql_runtime", "int64_of_string"
              ; to_string = "Int64", "to_string" } ] } }

let query_list1 =
  "INSERT INTO users (id, name, phone) VALUES %list{(%int{id}, %string{name}, \
   %string?{phone})}"

let parsed_query_list1 =
  { sql = "INSERT INTO users (id, name, phone) VALUES "
  ; in_params = []
  ; out_params = []
  ; list_params =
      Some
        { subsql = "(?, ?, ?)"
        ; string_index = 43
        ; param_index = 0
        ; params =
            [ { typ = None, "int"
              ; opt = false
              ; name = "id"
              ; of_string = "Ppx_mysql_runtime", "int_of_string"
              ; to_string = "Pervasives", "string_of_int" }
            ; { typ = None, "string"
              ; opt = false
              ; name = "name"
              ; of_string = "Ppx_mysql_runtime", "string_of_string"
              ; to_string = "Ppx_mysql_runtime", "identity" }
            ; { typ = None, "string"
              ; opt = true
              ; name = "phone"
              ; of_string = "Ppx_mysql_runtime", "string_of_string"
              ; to_string = "Ppx_mysql_runtime", "identity" } ] } }

let query_list2 =
  "INSERT INTO users (id, name, phone) VALUES %list{(%int{id}, %string{name}, NULL)}"

let parsed_query_list2 =
  { sql = "INSERT INTO users (id, name, phone) VALUES "
  ; in_params = []
  ; out_params = []
  ; list_params =
      Some
        { subsql = "(?, ?, NULL)"
        ; string_index = 43
        ; param_index = 0
        ; params =
            [ { typ = None, "int"
              ; opt = false
              ; name = "id"
              ; of_string = "Ppx_mysql_runtime", "int_of_string"
              ; to_string = "Pervasives", "string_of_int" }
            ; { typ = None, "string"
              ; opt = false
              ; name = "name"
              ; of_string = "Ppx_mysql_runtime", "string_of_string"
              ; to_string = "Ppx_mysql_runtime", "identity" } ] } }

let query_list3 =
  "SELECT @int{COUNT(*)} FROM users WHERE age > %int{age} OR id IN (%list{%int64{id}}) \
   OR name = %string{name}"

let parsed_query_list3 =
  { sql = "SELECT COUNT(*) FROM users WHERE age > ? OR id IN () OR name = ?"
  ; in_params =
      [ { typ = None, "int"
        ; opt = false
        ; name = "age"
        ; of_string = "Ppx_mysql_runtime", "int_of_string"
        ; to_string = "Pervasives", "string_of_int" }
      ; { typ = None, "string"
        ; opt = false
        ; name = "name"
        ; of_string = "Ppx_mysql_runtime", "string_of_string"
        ; to_string = "Ppx_mysql_runtime", "identity" } ]
  ; out_params =
      [ { typ = None, "int"
        ; opt = false
        ; name = "COUNT(*)"
        ; of_string = "Ppx_mysql_runtime", "int_of_string"
        ; to_string = "Pervasives", "string_of_int" } ]
  ; list_params =
      Some
        { subsql = "?"
        ; string_index = 51
        ; param_index = 1
        ; params =
            [ { typ = None, "int64"
              ; opt = false
              ; name = "id"
              ; of_string = "Ppx_mysql_runtime", "int64_of_string"
              ; to_string = "Int64", "to_string" } ] } }

let query_bad0 = "SELECT true FROM users WHERE id = %int{ID}"

let error_bad0 = `Bad_identifier "ID"

let query_bad1 = "SELECT @foo{id} FROM users"

let error_bad1 = `Unknown_type_spec "foo"

let query_bad2 = "SELECT id, name FROM users WHERE id = %foo{id}"

let error_bad2 = `Unknown_type_spec "foo"

let query_bad3 = "SELECT 'hello"

let error_bad3 = `Unterminated_string

let query_bad4 = "SELECT \"hello"

let error_bad4 = `Unterminated_string

let query_bad5 = "SELECT true\\"

let error_bad5 = `Escape_at_end

let query_bad6 = "SELECT @int{true FROM users"

let error_bad6 = `Unterminated_bracket

let query_bad7 = "SELECT true FROM users WHERE %int{id"

let error_bad7 = `Unterminated_bracket

let query_list_bad0 = "SELECT true FROM users WHERE id IN (%list?{%int{id}})"

let error_list_bad0 = `Optional_list

let query_list_bad1 = "SELECT true FROM users WHERE id IN (%list{%list{%int{id}}})"

let error_list_bad1 = `Nested_list

let query_list_bad2 = "SELECT true FROM users WHERE id IN (%list{@int{id}}})"

let error_list_bad2 = `Out_params_in_list

let query_list_bad3 = "SELECT true FROM users WHERE id IN (%list{%int{id})"

let error_list_bad3 = `Unterminated_list

let query_list_bad4 = "SELECT @list{*} FROM users"

let error_list_bad4 = `Unknown_type_spec "list"

let query_list_bad5 = "SELECT * FROM users WHERE id IN (%list{})"

let error_list_bad5 = `Empty_list_params

let query_list_bad6 =
  "SELECT * FROM users WHERE id IN (%list{%int{id}}) AND name IN (%list{%string{name}})"

let error_list_bad6 = `Multiple_lists_not_supported

let test_parse_query () =
  let run desc query expected =
    Alcotest.(
      check (result parsed_query_mod parse_error_mod) desc expected (Query.parse query))
  in
  run "query_0" query_0 (Ok parsed_query_0);
  run "query_out1" query_out1 (Ok parsed_query_out1);
  run "query_out2" query_out2 (Ok parsed_query_out2);
  run "query_out3" query_out3 (Ok parsed_query_out3);
  run "query_out4" query_out4 (Ok parsed_query_out4);
  run "query_in1" query_in1 (Ok parsed_query_in1);
  run "query_in2" query_in2 (Ok parsed_query_in2);
  run "query_in3" query_in3 (Ok parsed_query_in3);
  run "query_in4" query_in4 (Ok parsed_query_in4);
  run "query_inout" query_inout (Ok parsed_query_inout);
  run "query_quoted0" query_quoted0 (Ok parsed_query_quoted0);
  run "query_quoted1" query_quoted1 (Ok parsed_query_quoted1);
  run "query_quoted2" query_quoted2 (Ok parsed_query_quoted2);
  run "query_quoted3" query_quoted3 (Ok parsed_query_quoted3);
  run "query_list0" query_list0 (Ok parsed_query_list0);
  run "query_list1" query_list1 (Ok parsed_query_list1);
  run "query_list2" query_list2 (Ok parsed_query_list2);
  run "query_list3" query_list3 (Ok parsed_query_list3);
  run "query_bad0" query_bad0 (Error error_bad0);
  run "query_bad1" query_bad1 (Error error_bad1);
  run "query_bad2" query_bad2 (Error error_bad2);
  run "query_bad3" query_bad3 (Error error_bad3);
  run "query_bad4" query_bad4 (Error error_bad4);
  run "query_bad5" query_bad5 (Error error_bad5);
  run "query_bad6" query_bad6 (Error error_bad6);
  run "query_bad7" query_bad7 (Error error_bad7);
  run "query_list_bad0" query_list_bad0 (Error error_list_bad0);
  run "query_list_bad1" query_list_bad1 (Error error_list_bad1);
  run "query_list_bad2" query_list_bad2 (Error error_list_bad2);
  run "query_list_bad3" query_list_bad3 (Error error_list_bad3);
  run "query_list_bad4" query_list_bad4 (Error error_list_bad4);
  run "query_list_bad5" query_list_bad5 (Error error_list_bad5);
  run "query_list_bad6" query_list_bad6 (Error error_list_bad6)

(** {1 Functions and values for {!test_remove_duplicates}} *)

let param_foo32t =
  { typ = None, "int32"
  ; opt = true
  ; name = "foo"
  ; of_string = "Int32", "of_string"
  ; to_string = "Int32", "to_string" }

let param_foo64t =
  { typ = None, "int64"
  ; opt = true
  ; name = "foo"
  ; of_string = "Int64", "of_string"
  ; to_string = "Int64", "to_string" }

let param_foo32f =
  { typ = None, "int32"
  ; opt = false
  ; name = "foo"
  ; of_string = "Int32", "of_string"
  ; to_string = "Int32", "to_string" }

let param_bar32t =
  { typ = None, "int32"
  ; opt = true
  ; name = "bar"
  ; of_string = "Int32", "of_string"
  ; to_string = "Int32", "to_string" }

let test_remove_duplicates () =
  let run desc params expected =
    Alcotest.(
      check
        (result (list param_mod) conflicting_spec_mod)
        desc
        expected
        (Query.remove_duplicates params))
  in
  run "Duplicate 'foo'" [param_foo32t; param_foo32t] (Ok [param_foo32t]);
  run
    "Duplicate 'foo' and 'bar'"
    [param_foo32t; param_bar32t; param_foo32t; param_bar32t]
    (Ok [param_foo32t; param_bar32t]);
  run
    "Redefined 'foo' with different type"
    [param_foo32t; param_foo64t]
    (Error (`Conflicting_spec "foo"));
  run
    "Redefined 'foo' with different opt"
    [param_foo32t; param_foo32f]
    (Error (`Conflicting_spec "foo"))

(** {1 Main} *)

let testset =
  [ "parse_query", `Quick, test_parse_query
  ; "remove_duplicates", `Quick, test_remove_duplicates ]

let () = Alcotest.run "Query module" ["Query", testset]
