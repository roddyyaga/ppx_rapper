include Rapper.Make_helper (struct
  type +'a t = 'a Async.Deferred.t

  let return = Async.return

  let map f a = Async.Deferred.map ~f a

  module Stream = Caqti_async.Stream
end)
