let f0 = [%mysql Select_one "SELECT TRUE"]

let f1 = [%mysql Select_one "SELECT @string{name} FROM users WHERE id = 1"]

let f2 = [%mysql Select_one "SELECT @string{name} FROM users WHERE id = %int{id}"]

let f3 = [%mysql Select_one "SELECT @int{id}, @string{name} FROM users WHERE id = %int{id}"]

let f4 = [%mysql Select_one "SELECT @int{id}, @string{name} FROM users WHERE id = %int{id} OR name = %string{name}"]

let f5 = [%mysql Select_all "SELECT @int{id}, @string{name} FROM users WHERE id <> %int{id} AND id <> %int{id}"]

let f6 = [%mysql Select_opt "SELECT @int{id}, @string{name} FROM users WHERE id = %int{id}"]

let f7 = [%mysql Execute "DELETE FROM users WHERE id = %int{id}"]
