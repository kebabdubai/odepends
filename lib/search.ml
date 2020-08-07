module type S = sig
    type graph
    type key

    type visits

    val search : ?before:(key -> unit) ->
                 ?after:(key -> unit) ->
                 key -> graph -> visits -> unit

end

module Make = functor (N : Nodes.S) (Q : Queue.S) -> (struct
    module G = Graph.Make (N)
    module V = Visit.Make (N)

    type graph = G.t
    type key = G.key

    type visits = V.t

    type role = Child | Parent

    exception Cycle

    let search ?(before=ignore) ?(after=ignore) key graph visits =
        let queue = Q.create () in
        Q.push (Parent, key) queue;

        let search_child key =
            V.visit ~after_temporary:after
                    key visits
        in

        let search_neighbours key =
            match G.neighbours graph key with
            | [] -> begin
                V.visit ~after_temporary:after
                        key visits
            end
            | neighbours -> begin
                Q.push (Child, key) queue;
                List.iter (fun neighbour_key -> Q.push (Parent, neighbour_key) queue) neighbours
            end
        in

        let search_parent key =
            V.visit ~before_temporary:(fun _ -> raise Cycle)
                    ~before_none:before
                    ~after_none:search_neighbours
                    key visits
        in

        while not (Q.is_empty queue); do
            match Q.pop queue with
            | Child, key -> search_child key
            | Parent, key -> search_parent key
        done

end : S with type graph = Graph.Make (N).t
         and type key = Graph.Make (N).key
         and type visits = Visit.Make (N).t)
