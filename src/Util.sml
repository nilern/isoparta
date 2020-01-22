structure Fn = struct
    fun op|> (v, f) = f v
end

structure Result = struct
    datatype ('l, 'r) t
        = Left of 'l
        | Right of 'r

    fun map f =
        fn Left err => Left err
         | Right v => Right (f v)

    fun flatMap f =
        fn Left err => Left err
         | Right v => f v

    fun orElse thunk =
        fn Left _ => thunk ()
         | res as Right _ => res
end

