include Ppx_mysql_runtime.Make_context (struct
  module IO = struct
    type 'a t = 'a

    let return x = x

    let bind x f = f x
  end

  module Prepared = struct
    type dbh = Mysql.dbd

    type stmt = Mysql.Prepared.stmt

    type stmt_result = Mysql.Prepared.stmt_result

    type error = exn

    let wrap f x = try Ok (f x) with exn -> Error exn

    let create dbh sql = wrap (Mysql.Prepared.create dbh) sql

    let execute_null stmt args = wrap (Mysql.Prepared.execute_null stmt) args

    let fetch stmt_res = wrap Mysql.Prepared.fetch stmt_res
  end
end)
