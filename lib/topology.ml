module type S = sig
    type graph
    type key

    val sort : graph -> key list

end

module Make = functor (N : Nodes.S) (Q : Queue.S) -> (struct
    module G = Graph.Make (N)
    module V = Visit.Make (N)
    module S = Search.Make (N) (Queue.Fifo)

    type graph = G.t
    type key = G.key

    let sort graph =
        let visits = V.create (G.size graph) in 
        let order = Q.create () in

        G.iter ~f:(fun key ->
            match G.neighbours graph key with
            | [] -> ()
            | _ -> S.search ~after:(fun x -> Q.push x order) key graph visits) graph;
        Q.to_list order

end : S with type graph = Graph.Make (N).t
         and type key = Graph.Make (N).key)

module Base = Make (Nodes.Base) (Queue.Fifo)
