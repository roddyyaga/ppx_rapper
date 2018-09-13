include
  Ppx_mysql_runtime.PPX_CONTEXT
  with type 'a IO.t = 'a Async.Deferred.t
   and type Prepared.dbh = Mysql.dbd
   and type Prepared.error = Core.Error.t
