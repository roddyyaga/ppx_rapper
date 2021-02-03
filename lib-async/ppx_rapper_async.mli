include
  Rapper.RAPPER_HELPER
    with type 'a Rapper_helper.future := 'a Async.Deferred.t
     and module Rapper_helper.Stream = Caqti_async.Stream
