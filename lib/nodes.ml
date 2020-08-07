module type S = sig
    type key
    type 'a t

    val create : int -> 'a -> 'a t
    val size : 'a t -> int

    val get : 'a t -> key -> 'a
    val set : 'a t -> key -> 'a -> unit

    val iter : f:(key -> 'a -> unit) -> 'a t -> unit

end

module Base = (struct
    type key = int
    type 'a t = 'a array

    let create size value = Array.make size value
    let size nodes = Array.length nodes

    let get nodes key = Array.get nodes key
    let set nodes key value = Array.set nodes key value

    let iter ~f nodes = Array.iteri f nodes

end : S with type key = int)
