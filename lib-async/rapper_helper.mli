include
  Rapper.RAPPER_HELPER
    with type 'a future := 'a Async.Deferred.t
     and module Stream = Caqti_async.Stream
