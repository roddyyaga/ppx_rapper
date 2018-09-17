let test_no_params = [%mysql Select_one "SELECT TRUE"]

let test_single_output_params =
  [%mysql Select_one "SELECT @string{name} FROM users WHERE id = 1"]


let test_pair_output_params =
  [%mysql Select_one "SELECT @int{id}, @string{name} FROM users WHERE id = 1"]


let test_one_input_params =
  [%mysql Select_one "SELECT @string{name} FROM users WHERE id = %int{id}"]


let test_two_input_pair_output_params =
  [%mysql
    Select_one
      "SELECT @int{id}, @string{name} FROM users WHERE id = %int{id} OR name = \
       %string{name}"]


let test_select_all = [%mysql Select_all "SELECT @int{id}, @string{name} FROM users"]

let test_repeated_input_params =
  [%mysql
    Select_all
      "SELECT @int{id}, @string{name} FROM users WHERE id <> %int{id} AND id <> %int{id}"]


let test_select_opt =
  [%mysql Select_opt "SELECT @int{id}, @string{name} FROM users WHERE id = %int{id}"]


let test_execute = [%mysql Execute "DELETE FROM users WHERE id = %int{id}"]

let test_int = [%mysql Select_one "SELECT @int{a}, @int?{b} FROM users where a = %int{a} OR b = %int?{b}"]

let test_int32 = [%mysql Select_one "SELECT @int32{a}, @int32?{b} FROM users where a = %int32{a} OR b = %int32?{b}"]

let test_int64 = [%mysql Select_one "SELECT @int64{a}, @int64?{b} FROM users where a = %int64{a} OR b = %int64?{b}"]

let test_bool = [%mysql Select_one "SELECT @bool{a}, @bool?{b} FROM users where a = %bool{a} OR b = %bool?{b}"]

let test_string = [%mysql Select_one "SELECT @string{a}, @string?{b} FROM users where a = %string{a} OR b = %string?{b}"]
