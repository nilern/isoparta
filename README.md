# Isoparta

The goal is to make [invertible syntax
descriptions](https://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)
using module functors instead of type classes. I think this is a Final Tagless
encoding of the GADT approach seen
[here](https://jobjo.github.io/2019/05/19/applicative-parsing.html).

Another goal is to generate Strong LL parsers in an extra "JIT compile" step.
This is unlike the Parsec semantics where although parsers have one token of
lookahead (unless `try` is used), lookahead conflicts are silently resolved in
order of alternatives and the monadic interface enables context-dependent
parsing.

