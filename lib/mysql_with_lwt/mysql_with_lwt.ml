type dbh = Mysql.dbd

module IO = Lwt

module Prepared = struct
  type stmt = Mysql.Prepared.stmt

  type stmt_result = Mysql.Prepared.stmt_result

  let create dbd sql = Lwt_preemptive.detach (Mysql.Prepared.create dbd) sql

  let close stmt = Lwt_preemptive.detach Mysql.Prepared.close stmt

  let execute_null stmt args =
    Lwt_preemptive.detach (Mysql.Prepared.execute_null stmt) args


  let fetch stmt_res = Lwt_preemptive.detach Mysql.Prepared.fetch stmt_res
end
