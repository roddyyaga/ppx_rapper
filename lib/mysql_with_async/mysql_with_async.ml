open Async

type dbh = Mysql.dbd

module IO = struct
  type 'a t = 'a Deferred.t

  let return = Deferred.return

  let bind x f = Deferred.bind x ~f
end

module Prepared = struct
  type stmt = Mysql.Prepared.stmt

  type stmt_result = Mysql.Prepared.stmt_result

  let create dbd sql = Deferred.return @@ Mysql.Prepared.create dbd sql

  let close stmt = Deferred.return @@ Mysql.Prepared.close stmt

  let execute_null stmt args = Deferred.return @@ Mysql.Prepared.execute_null stmt args

  let fetch stmt_res = Deferred.return @@ Mysql.Prepared.fetch stmt_res
end
