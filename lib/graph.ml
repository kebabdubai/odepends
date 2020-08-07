module type S = sig
    type key
    type t

    val create : int -> t
    val size : t -> int

    val neighbours : t -> key -> key list
    val insert : t -> key -> key -> unit
    
    val iter : f:(key -> unit) -> t -> unit

end

module Make = functor (N : Nodes.S) -> (struct
    type key = N.key
    type t = (key list) N.t

    let create size = N.create size []
    let size graph = N.size graph

    let neighbours graph key = N.get graph key

    let insert graph key1 key2 = 
        let neighbours = N.get graph key1 in
        N.set graph key1 (key2 :: neighbours)

    let iter ~f graph = N.iter ~f:(fun key _ -> f key) graph

end : S with type key = N.key)

module Base = Make (Nodes.Base)
