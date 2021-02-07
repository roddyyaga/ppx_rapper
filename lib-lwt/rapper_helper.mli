include
  Rapper.RAPPER_HELPER
    with type 'a future := 'a Lwt.t
     and module Stream = Caqti_lwt.Stream
