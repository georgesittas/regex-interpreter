# Regex Interpreter

The interpreter consists of the following phases:

1. Parse a regular expression
2. Create an NFA for it by walking the produced parse tree
3. Convert the NFA to an equivalent DFA
4. Compute the DFA's result by feeding it the input string

The following algorithms have been implemented:

- NFA creation: [Thompson's construction algorithm](https://en.wikipedia.org/wiki/Thompson%27s_construction)
- NFA to DFA conversion: [Powerset construction](https://en.wikipedia.org/wiki/Powerset_construction)

## Syntax

The interpreter expects regular expressions to conform to the following syntax:

- `x`, a single character.
- `"_"`, the _empty symbol_.
- `"."`, _any_ character.
- `(r)`, `r` is a regular expression.
- `r1 | r2`, `r1` and `r2` are regular expressions (Union).
- `r1r2`, `r1` and `r2` are regular expressions (Concatenation).
- `r*`, `r` is a regular expression (Kleene star).

## Functions

The following functions can be used after loading RegInterpreter.hs:

- `makeNfa` receives a regular expression (`[Char]`) & returns an NFA (`Fsa`).
- `nfaToDfa` receives an NFA (`Fsa`) & returns a DFA (`Fsa`).
- `regexFullMatch` receives a tuple `(regex, string)` & returns `True` if `string` is accepted by `regex`.
- `regexPartMatch` receives a tuple `(regex, string)` & returns all prefixes in `string` accepted by `regex`.

## Testing

```haskell
➜  Regex-Interpreter git:(main) ghci testSuite.hs
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling RegParser        ( RegParser.hs, interpreted )
[2 of 3] Compiling RegInterpreter   ( RegInterpreter.hs, interpreted )
[3 of 3] Compiling Main             ( testSuite.hs, interpreted )
Ok, three modules loaded.
ghci> testAll
True
```

## Contributors

• [George Sittas](https://github.com/GeorgeSittas)\
• [Jim Rontogiannis](https://github.com/rondojim)
