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

    type error = [`Mysql_exception of exn]

    let pool = Thread_pool.init ~name:"default" ~threads:8 ~create:Fn.id ~destroy:Fn.id

    let wrap f x =
      pool
      >>= fun pool ->
      Thread_pool.with' pool (fun () -> Deferred.return @@ `Ok (f x))
      >>= function
      | Ok _ as v ->
          Deferred.return v
      | Error e ->
          Deferred.return @@ Error (`Mysql_exception (Error.to_exn e))


    let create dbd sql = wrap (Mysql.Prepared.create dbd) sql

    let close stmt = wrap Mysql.Prepared.close stmt

    let execute_null stmt args = wrap (Mysql.Prepared.execute_null stmt) args

    let fetch stmt_res = wrap Mysql.Prepared.fetch stmt_res
  end
end)
