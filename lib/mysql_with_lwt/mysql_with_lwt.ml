include Ppx_mysql_runtime.Make_context (struct
  module IO = Lwt

  module Prepared = struct
    type dbh = Mysql.dbd

    type stmt = Mysql.Prepared.stmt

    type stmt_result = Mysql.Prepared.stmt_result

    type error = [`Mysql_exception of exn]

    let wrap f x =
      let open Lwt.Infix in
      Lwt.catch
        (fun () -> Lwt_preemptive.detach f x >>= fun v -> Lwt.return_ok v)
        (fun exn -> Lwt.return_error @@ `Mysql_exception exn)


    let create dbd sql = wrap (Mysql.Prepared.create dbd) sql

    let close stmt = wrap Mysql.Prepared.close stmt

    let execute_null stmt args = wrap (Mysql.Prepared.execute_null stmt) args

    let fetch stmt_res = wrap Mysql.Prepared.fetch stmt_res
  end
end)
