module type S = sig
    type key
    type t

    val create : int -> t

    val visit : ?before_none:(key -> unit) ->
                ?after_none:(key -> unit) ->
                ?before_temporary:(key -> unit) ->
                ?after_temporary:(key -> unit) ->
                ?permanent:(key -> unit) ->
                key -> t -> unit
end

module Make = functor (N : Nodes.S) -> (struct
    type marker = None | Temporary | Permanent
    type key = N.key
    type t = marker N.t

    let create size = N.create size None

    let visit ?(before_none=ignore) ?(after_none=ignore) ?(before_temporary=ignore) ?(after_temporary=ignore)
              ?(permanent=ignore) key visits = 

        match N.get visits key with
        | None -> before_none key; N.set visits key Temporary; after_none key
        | Temporary -> before_temporary key; N.set visits key Permanent; after_temporary key
        | Permanent -> permanent key 

end : S with type key = N.key)
