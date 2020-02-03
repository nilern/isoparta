structure Tests = struct
    structure TextParsers = ParserSyntaxFn(struct
        open TextIO.StreamIO

        val eqElems = op=
    end)
    structure ArithParser = ArithFn(TextParsers)

    fun testParser () =
        let val parse = TextParsers.build ArithParser.atom
        in case parse (TextIO.getInstream (TextIO.openString "42"))
           of Result.Right ([#"4", #"2"], _) => ()
            | Result.Right (res, _) =>
               raise Fail ("Erroneous parse '" ^ List.foldl (fn (c, acc) => acc ^ String.str c)
                                                            "" res ^ "'")
            | Result.Left err => raise Fail "Parser failed"
        end

    fun main (name, _) =
        ( print (name ^ " running tests.\n")
        ; testParser ()
        ; print "All tests passed.\n"
        ; OS.Process.success )
        handle exn =>
            ( print ("unhandled exception: " ^ exnMessage exn ^ "\n")
            ; List.app (fn s => print ("\t" ^ s ^ "\n")) (SMLofNJ.exnHistory exn)
            ; OS.Process.failure )
end

