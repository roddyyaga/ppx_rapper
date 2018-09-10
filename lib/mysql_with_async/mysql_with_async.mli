include Ppx_mysql_runtime.PPX_CONTEXT with
  type dbh = Mysql.dbd and
  type 'a IO.t = 'a Async.Deferred.t
