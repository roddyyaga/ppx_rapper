include Rapper.Make_helper (struct
  type +'a t = 'a

  let return = Fun.id
  let map : ('a -> 'b) -> 'a -> 'b = fun f t -> f t

  module Stream = Caqti_eio.Stream
end)
