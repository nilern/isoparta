functor ArithFn(SyntaxDomain : SYNTAX_DOMAIN where type tok = char) = struct
    structure Syntax = TextSyntaxFn(SyntaxFn(SyntaxDomain))

    val digit =
        let fun loop n acc =
                if n < 10
                then loop (n + 1) (Syntax.alt acc (Syntax.token (valOf (Char.fromString (Int.toString n)))))
                else acc
        in loop 1 (Syntax.token (valOf (Char.fromString (Int.toString 0))))
        end

    val atom = Syntax.many digit
end

