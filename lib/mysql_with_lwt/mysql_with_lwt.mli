include
  Ppx_mysql_runtime.PPX_CONTEXT
  with type 'a IO.t = 'a Lwt.t
   and type Prepared.dbh = Mysql.dbd
