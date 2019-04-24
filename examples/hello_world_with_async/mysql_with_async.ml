open Async
open Core

include Ppx_mysql_runtime.Make_context (struct
  module IO = struct
    type 'a t = 'a Deferred.t

    let return = Deferred.return

    let bind x f = Deferred.bind x ~f
  end

  module Prepared = struct
    type dbh = Mysql.dbd

    type stmt = Mysql.Prepared.stmt

    type stmt_result = Mysql.Prepared.stmt_result

    type error = exn

    let wrap f x = Deferred.return (try Ok (f x) with exc -> Error exc)

    let create dbh sql = wrap (Mysql.Prepared.create dbh) sql

    let close stmt = wrap Mysql.Prepared.close stmt

    let execute_null stmt args = wrap (Mysql.Prepared.execute_null stmt) args

    let fetch stmt_res = wrap Mysql.Prepared.fetch stmt_res
  end
end)
