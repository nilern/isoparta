structure Prism :> sig
    type ('a, 'b) t = {apply : 'a -> 'b, unapply : 'b -> 'a option}

    val apply : ('a, 'b) t -> 'a -> 'b
end = struct
    type ('a, 'b) t = {apply : 'a -> 'b, unapply : 'b -> 'a option}

    val apply : ('a, 'b) t -> 'a -> 'b = #apply
end

