include Rapper.Make_helper (struct
  type +'a t = 'a Lwt.t

  let return = Lwt.return

  let map = Lwt.map

  module Stream = Caqti_lwt.Stream
end)
