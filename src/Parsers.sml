signature INPUT_STREAM = sig
    type instream
    type elem

    val eqElems : elem * elem -> bool
    val input1 : instream -> (elem * instream) option
end

functor ParserSyntaxFn(Input : INPUT_STREAM) :> sig
    include SYNTAX_DOMAIN where type tok = Input.elem

    datatype error
        = EOF
        | Unexpected of tok * tok
    
    val build : 'a t -> (Input.instream -> (error, 'a * Input.instream) Result.t)
end = struct
    datatype either = datatype Result.t
    type instream = Input.instream
    val op|> = Fn.|>

    infix 1 |>

    type tok = Input.elem

    datatype error
        = EOF
        | Unexpected of tok * tok

    type 'a impl = instream -> (error, 'a * instream) Result.t

    type 'a t = 'a impl option ref

    fun build (ref (SOME f)) = f
      | build (r as ref NONE) = (* OPTIMIZE *)
         (fn input =>
              (case !r
               of SOME f => f input
                | NONE => raise Fail "unreachable code reached"))

    fun token expected =
        ref (SOME (fn input =>
                       case Input.input1 input
                       of SOME (tok, input) =>
                           if Input.eqElems (tok, expected)
                           then Right (tok, input)
                           else Left (Unexpected (expected, tok))
                        | NONE => Left EOF))

    fun pure v =
        ref (SOME (fn input => Right (v, input)))

    fun map iso p =
        let val parse = build p
            val f = Prism.apply iso
        in  ref (SOME (fn input =>
                           parse input
                           |> Result.map (fn (v, input) => (f v, input))))
        end

    fun product p q =
        let val parse = build p
            val parse' = build q
        in  ref (SOME (fn input =>
                           parse input
                           |> Result.flatMap (fn (v, input) =>
                                                  parse' input
                                                  |> Result.map (fn (v', input) => ((v, v'), input)))))
        end

    fun alt p q =
        let val parse = build p
            val parse' = build q
        in  ref (SOME (fn input => parse input |> Result.orElse (fn () => parse' input)))
        end

    fun fix holey =
        let val parser = ref NONE
        in parser := !(holey parser)
         ; parser
        end
end

