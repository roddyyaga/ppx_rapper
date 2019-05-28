let test_no_params = [%mysql select_one "SELECT TRUE"]

let test_single_output_params =
  [%mysql select_one "SELECT @string{name} FROM users WHERE id = 1"]

let test_pair_output_params =
  [%mysql select_one "SELECT @int{id}, @string{name} FROM users WHERE id = 1"]

let test_one_input_params =
  [%mysql select_one "SELECT @string{name} FROM users WHERE id = %int{id}"]

let test_two_input_pair_output_params =
  [%mysql
    select_one
      "SELECT @int{id}, @string{name} FROM users WHERE id = %int{id} OR name = \
       %string{name}"]

let test_select_all = [%mysql select_all "SELECT @int{id}, @string{name} FROM users"]

let test_repeated_input_params =
  [%mysql
    select_all
      "SELECT @int{id}, @string{name} FROM users WHERE id <> %int{id} AND id <> %int{id}"]

let test_select_opt =
  [%mysql select_opt "SELECT @int{id}, @string{name} FROM users WHERE id = %int{id}"]

let test_execute = [%mysql execute "DELETE FROM users WHERE id = %int{id}"]

let test_int =
  [%mysql
    select_one "SELECT @int{a}, @int?{b} FROM users where a = %int{a} OR b = %int?{b}"]

let test_int32 =
  [%mysql
    select_one
      "SELECT @int32{a}, @int32?{b} FROM users where a = %int32{a} OR b = %int32?{b}"]

let test_int64 =
  [%mysql
    select_one
      "SELECT @int64{a}, @int64?{b} FROM users where a = %int64{a} OR b = %int64?{b}"]

let test_bool =
  [%mysql
    select_one
      "SELECT @bool{a}, @bool?{b} FROM users where a = %bool{a} OR b = %bool?{b}"]

let test_string =
  [%mysql
    select_one
      "SELECT @string{a}, @string?{b} FROM users where a = %string{a} OR b = %string?{b}"]

let test_custom_type =
  [%mysql
    select_one "SELECT @Id{a}, @Phone?{b} FROM users where a = %Id{a} OR b = %Phone?{b}"]

let test_list0 =
  [%mysql
    select_all "SELECT @int{id}, @string{name} FROM users WHERE id IN (%list{%int{id}})"]

let test_list1 =
  [%mysql
    execute
      "INSERT INTO users (id, name, phone) VALUES %list{(%int{id}, %string{name}, NULL)}"]

let test_list2 =
  [%mysql
    select_all
      "SELECT @int{id}, @string{name} FROM users WHERE name = %string{name} OR id IN \
       (%list{%int{id}}) OR age > %int{age}"]

let test_list3 =
  [%mysql
    execute
      "INSERT INTO users (id, name, real_name, age) VALUES %list{(%int{id}, \
       %string{name}, %string{name}, %int{age})}"]

let test_cached0 =
  [%mysql
    select_all ~cached:true "SELECT @int{id}, @string{name} FROM users WHERE id IN (%list{%int{id}})"]

let test_cached1 =
  [%mysql
    select_all ~cached:false "SELECT @int{id}, @string{name} FROM users WHERE id IN (%list{%int{id}})"]
