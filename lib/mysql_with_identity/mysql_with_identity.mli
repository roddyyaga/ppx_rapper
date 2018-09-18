include
  Ppx_mysql_runtime.PPX_MYSQL_CONTEXT
  with type 'a IO.t = 'a
   and type Prepared.dbh = Mysql.dbd
   and type Prepared.error = exn
