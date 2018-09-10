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
