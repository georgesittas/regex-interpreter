# Regex Interpreter

The interpreter consists of the following phases:

1. Parse a regular expression
2. Create an NFA for it by walking the produced parse tree
3. Convert the NFA to an equivalent DFA
4. Compute the DFA's result by feeding it the input string

In order to create the NFA, we've implemented the known [Thompson's construction algorithm](https://en.wikipedia.org/wiki/Thompson%27s_construction),
and for the NFA to DFA conversion, the corresponding [Powerset construction](https://en.wikipedia.org/wiki/Powerset_construction).

## Syntax

The interpreter accepts regular expressions that have the following form:

• `x`, where x is a single character (eg. "a").\
• `"_"`, denoting the _empty symbol_.\
• `"."`, which matches _any_ character (like "?" in a bash shell, for example).\
• `(r)`, where r is a regular expression.\
• `r1 | r2`, where `r1` and `r2` are regular expressions (Union).\
• `r1r2`, where `r1` and `r2` are regular expressions (Concatenation).\
• `r*`, where `r` is a regular expression (Kleene star).

## Functions

The following functions can be used after loading RegInterpreter.hs:

• makeNfa : outputs an NFA (`Fsa`), given as input a regular expression (`[Char]`).\
• nfaToDfa : outputs a DFA (`Fsa`), given as input an NFA (`Fsa`).\
• regexFullMatch : receives a tuple `(regex,string)` and answers `True` if `string` is accepted by `regex`.\
• regexPartMatch : receives a tuple `(regex,string)` and returns all prefixes in `string` accepted by `regex`.

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

• [George Sittas (Jo)](https://github.com/GeorgeSittas)\
• [Jim Rontogiannis](https://github.com/rondojim)
