module type S = sig
    type 'a t

    exception Empty

    val create : unit -> 'a t

    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a

    val is_empty : 'a t -> bool
    val to_list : 'a t -> 'a list

end

module Fifo : S = struct
    type 'a t = {
        mutable items : 'a list
    }

    exception Empty

    let create () =
        { items = [] }    
    
    let push x queue = 
        queue.items <- x :: queue.items
    
    let pop queue =
        match queue.items with
        | hd :: tl -> queue.items <- tl; hd
        | [] -> raise Empty

    let is_empty queue =
        match queue.items with
        | [] -> true
        | _ -> false

    let to_list queue = queue.items  

end
