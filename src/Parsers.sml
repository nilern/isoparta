signature INPUT_STREAM = sig
    type instream
    type elem

    structure ElemSet : ORD_SET where type Key.ord_key = elem

    val eqElems : elem * elem -> bool
    val input1 : instream -> (elem * instream) option
end

structure GlaId :> sig
    type t

    val fresh : unit -> t

    structure HashTable : MONO_HASH_TABLE where type Key.hash_key = t
end = struct
    type t = int

    val counter = ref 0

    fun fresh () =
        let val id = !counter
        in counter := id + 1
         ; id
        end

    structure HashKey = struct
        type hash_key = t

        val hashVal = Word.fromInt
        val sameKey = op=
    end

    structure HashTable = HashTableFn(HashKey)
end

functor GrammarSyntaxFn(Input : INPUT_STREAM) :> SYNTAX_DOMAIN = struct
    datatype either = datatype Result.t
    type tok = Input.elem
    val op|> = Fn.|>
    infix 1 |>

    datatype error
        = EOF
        | Unexpected of tok * tok

    datatype gla_edge
        = Terminal of tok * gla_state
        | Epsilon of gla_state

    withtype gla_state = gla_edge option ref list ref

    fun insertGlaEdge state edge = state := edge :: !state

    type state_cache = (gla_state * gla_state) GlaId.HashTable.hash_table

    type 'a expr =
        { id : GlaId.t
        , insertBetween : state_cache -> gla_state * gla_state -> unit
        , build : Input.instream -> (error, 'a * Input.instream) Result.t }

    type 'a t = 'a expr option ref

    fun doInsertBetween (p : 'a t) cache states =
        case !p
        of SOME {id, ...} =>
            (case GlaId.HashTable.find cache id
             of SOME (start, accept) =>
                 let val (pred, succ) = states
                 in insertGlaEdge pred (ref (SOME (Epsilon start)))
                  ; insertGlaEdge accept (ref (SOME (Epsilon succ)))
                 end)

    fun build (ref (SOME p) : 'a t) = #build p
      | build (r as ref NONE) =
         let val parse = ref NONE
         in  fn input =>
                 (case !parse
                  of SOME f => f input
                   | NONE => 
                      (case !r
                       of SOME p =>
                           let val f = #build p
                           in parse := SOME f
                            ; f input
                           end
                        | NONE => raise Fail "unreachable code reached"))
         end

    fun token expected =
        ref (SOME { id = GlaId.fresh ()
                  , insertBetween =
                      fn _ => fn (pred, succ) =>
                          insertGlaEdge pred (ref (SOME (Terminal (expected, succ))))
                  , build = fn input =>
                                case Input.input1 input
                                of SOME (tok, input) =>
                                    if Input.eqElems (tok, expected)
                                    then Right (tok, input)
                                    else Left (Unexpected (expected, tok))
                                 | NONE => Left EOF })
    fun pure v =
        ref (SOME { id = GlaId.fresh ()
                  , insertBetween =
                      fn _ => fn (pred, succ) =>
                          insertGlaEdge pred (ref (SOME (Epsilon succ)))
                  , build = fn input => Right (v, input) })

    fun map iso p =
        let val parse = build p
            val f = Prism.apply iso
        in  ref (SOME { id = GlaId.fresh ()
                      , insertBetween = doInsertBetween p
                      , build = fn input =>
                                    parse input
                                    |> Result.map (fn (v, input) => (f v, input)) })
        end

    fun product p q =
        let val parse = build p
            val parse' = build q
        in  ref (SOME { id = GlaId.fresh ()
                      , build = fn input =>
                                    parse input
                                    |> Result.flatMap (fn (v, input) =>
                                                           parse' input
                                                           |> Result.map (fn (v', input) => ((v, v'), input))) })
        end

    fun alt p q =
        let val parse = build p
            val parse' = build q
        in  ref (SOME { id = GlaId.fresh ()
                      , build = fn input => parse input |> Result.orElse (fn () => parse' input) })
        end

    fun fix holey =
        let val parser = ref NONE
        in parser := !(holey parser)
         ; parser
        end
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

