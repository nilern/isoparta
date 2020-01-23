(* TODO: Switch to Prism since the Iso parse error is considered harmful? *)
structure PartialIso :> sig
    type ('a, 'b) t = {apply : 'a -> 'b option, unapply : 'b -> 'a option}

    val apply : ('a, 'b) t -> 'a -> 'b option
    val filter : ('a -> bool) -> ('a, 'a) t
end = struct
    type ('a, 'b) t = {apply : 'a -> 'b option, unapply : 'b -> 'a option}

    val apply : ('a, 'b) t -> 'a -> 'b option = #apply

    fun filter pred =
        let fun apply x = if pred x then SOME x else NONE
        in {apply, unapply = apply}
        end
end

