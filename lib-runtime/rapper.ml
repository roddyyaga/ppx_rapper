module type CUSTOM = sig
  type t

  val t : t Caqti_type.t
end

(* Derived from implementation of List.group in Base.
   Original code is copyright (c) 2016--2020 Jane Street Group, LLC opensource@janestreet.com
   and the following permission notice applies to it:
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)
let list_group l ~break =
  let groups =
    List.fold_left
      (fun acc x ->
        match acc with
        | [] -> [ [ x ] ]
        | current_group :: tl ->
            if break (List.hd current_group) x then [ x ] :: current_group :: tl
              (* start new group *)
            else (x :: current_group) :: tl)
      [] l
    (* extend current group *)
  in
  match groups with [] -> [] | l -> List.rev_map List.rev l

let load_many (get_parent, parent_key) children_getters_and_setters data =
  data
  |> list_group ~break:(fun x y ->
         parent_key (get_parent x) <> parent_key (get_parent y))
  |> List.map (fun group ->
         let parents, children =
           ( List.map get_parent group,
             List.map
               (fun (getter, setter) -> (setter, List.map getter group))
               children_getters_and_setters )
         in
         let parent = List.hd parents in
         List.fold_left
           (fun current_parent (setter, children) ->
             setter current_parent children)
           parent children)

module Internal = struct
  module Dynparam = struct
    type t = Pack : 'a Caqti_type.t * 'a -> t

    let empty = Pack (Caqti_type.unit, ())

    let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
  end
end

module type IO = sig
  type +'a t

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t

  module Stream : Caqti_stream.S with type 'a future := 'a t
end

module type CONTEXT = sig
  module Rapper_io : sig
    type +'a future

    val map : 'a future -> ('a -> 'b) -> 'b future

    val fail : 'e -> ('a, 'e) result future

    module Stream : Caqti_stream.S with type 'a future := 'a future

    module type CONNECTION =
      Caqti_connection_sig.S
        with type 'a future := 'a future
         and type ('a, 'err) stream := ('a, 'err) Stream.t
  end
end

module Make_context (Io : IO) :
  CONTEXT
    with type 'a Rapper_io.future := 'a Io.t
     and module Rapper_io.Stream = Io.Stream = struct
  module Rapper_io = struct
    let map = Io.map

    let fail e = Io.return (Error e)

    module Stream = Io.Stream

    module type CONNECTION =
      Caqti_connection_sig.S
        with type 'a future := 'a Io.t
         and type ('a, 'err) stream := ('a, 'err) Stream.t
  end
end