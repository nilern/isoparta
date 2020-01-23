functor ArithFn(SyntaxDomain : SYNTAX_DOMAIN where type tok = char) = struct
    structure Syntax = TextSyntaxFn(SyntaxFn(SyntaxDomain))

    val atom = Syntax.many Syntax.digit
end

