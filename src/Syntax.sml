signature SYNTAX_DOMAIN = sig
    type 'a t
    type tok

    val token : tok t
    val pure : 'a -> 'a t
    val map : ('a, 'b) PartialIso.t -> 'a t -> 'b t
    val product : 'a t -> 'b t -> ('a * 'b) t
    val alt : 'a t -> 'a t -> 'a t
    (* TODO: val fix : ('a t -> 'a t) -> 'a t *)
end

signature SYNTAX = sig
    include SYNTAX_DOMAIN
end

functor SyntaxFn(SyntaxDomain : SYNTAX_DOMAIN) :> SYNTAX
    where type 'a t = 'a SyntaxDomain.t
    where type tok = SyntaxDomain.tok
= struct
    open SyntaxDomain
end

functor TextSyntaxFn(Syntax : SYNTAX where type tok = char) :> sig
    include SYNTAX
        where type 'a t = 'a Syntax.t
        where type tok = Syntax.tok

    val digit : char t
end = struct
    open Syntax

    val digit = map (PartialIso.filter Char.isDigit) token
end

