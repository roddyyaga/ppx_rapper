include
  Rapper.RAPPER_HELPER
    with type 'a Rapper_helper.future := 'a Lwt.t
     and module Rapper_helper.Stream = Caqti_lwt.Stream
