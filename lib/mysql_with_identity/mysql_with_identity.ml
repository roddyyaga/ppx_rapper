type dbh = Mysql.dbd

module IO = struct
  type 'a t = 'a

  let return x = x

  let bind x f = f x
end

module Prepared = Mysql.Prepared
