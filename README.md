# Regular Expression Interpreter

The interpreter consists of the following phases:

1. Parse a regular expression
2. Create an NFA for it by walking the produced parse tree
3. Convert the NFA to an equivalent DFA
4. Compute the DFA's result by feeding it the input string

In order to create the NFA, we've implemented the known [Thompson's construction algorithm](https://en.wikipedia.org/wiki/Thompson%27s_construction),
and for the NFA to DFA conversion, the corresponding [Powerset construction](https://en.wikipedia.org/wiki/Powerset_construction).

## Syntax

The interpreter accepts regular expressions that have the following form:

• x, where x is a single character (eg. "a").\
• "\_", denoting the _empty symbol_.\
• ".", which matches _any_ character (like "?" in a bash shell, for example).\
• (r), where r is a regular expression.\
• r<sub>1</sub> | r<sub>2</sub>, where r<sub>1</sub> and r<sub>2</sub> are regular expressions (Union).\
• r<sub>1</sub>r<sub>2</sub>, where r<sub>1</sub> and r<sub>2</sub> are regular expressions (Concatenation).\
• r<sup>*</sup>, where r is a regular expression (Kleene star).

## Functions

The following functions can be used after loading RegInterpreter.hs:

• makeNfa : outputs an NFA (of type "Fsa"), given as input a regular expression (of type "[Char]").\
• nfaToDfa : outputs a DFA (of type "Fsa"), given as input an NFA (of type "Fsa").\
• regexFullMatch : receives a tuple (regex,string) and answers true of 'string' is accepted by 'regex'.\
• regexPartMatch : receives a tuple (regex,string) and returns a list of all prefixes in 'string' that are accepted by 'regex'.

## Contributors

• [George Sittas (Jo)](https://github.com/GeorgeSittas)\
• [Jim Rontogiannis](https://github.com/rondojim)
