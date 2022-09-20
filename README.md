Tokenizer
===
***WARNING this package is not tested enough for the moment.
Bugs are very likely here.***

This package provides solution for two problems:
- split input string on tokens of specificated sort;
- check tokenizing of *all possible* strings is unique.

Some examples
---
*If you have problems with understanding the syntax we use read two
sections bellow.*

Here everything is ok
```hs
> checkUniqueTokenizing $  parse <$> ["ab", "bc", "abc"]
Right ()
```
and we can split string on these token with deterministic result:
```hs
> tokenize (makeTokenizeMap $  parse <$> ["ab", "bc", "abc"]) "abbcabc"
Right [("ab","ab"),("bc","bc"),("abc","abc")]
> tokenize (makeTokenizeMap $  parse <$> ["ab", "bc", "abc"]) "abbcabca"
Left (NoWayTokenize 7 [("ab","ab"),("bc","bc"),("abc","abc")])
```

We can parse `"ab"` as `"a"` and `"b"` or `"ab"`
```hs
> checkUniqueTokenizing $  parse <$> ["ab", "a", "b"]
Left Conflicts: [("a",a),("b",b)] [("ab",ab)]
```
we *can* tokenize using this set of tokens, but sometimes it gives us
`TwoWaysTokenize` error:
```hs
> tokenize (makeTokenizeMap $  parse <$> ["a", "b", "ab"]) "bba"
Right [("b","b"),("b","b"),("a","a")]
> tokenize (makeTokenizeMap $  parse <$> ["a", "b", "ab"]) "aab"
Left (TwoWaysTokenize 1 [("a","a"),("ab","ab")] [("a","a"),("a","a"),("b","b")])
```
to solve the problem we can specify that that there should be no `b`
character after separate `a` token
```hs
> checkUniqueTokenizing $  parse <$> ["ab", "a?!b", "b"]
Right ()
> tokenize (makeTokenizeMap $  parse <$> ["a?!b", "b", "ab"]) "aab"
Right [("a?!b","a"),("ab","ab")]
```

More complex example. Problem is for string `"ababab"`:
```hs
> checkUniqueTokenizing $  parse <$> ["ab", "aba", "bab"]
Left Conflicts: [("ab",ab),("ab",ab),("ab",ab)] [("aba",aba),("bab",bab)]
```

Here even `"aab"` can be split as `aa` and then `b` or `a*b`. However, current
algorithm gives another conflict `"aaa*b"` can be spit as `aa` and `a*b` or
simply `a*b`:
``` hs
> checkUniqueTokenizing $  parse <$> ["a*b", "aa", "b"]
Left Conflicts: [("aa",aa),("a*b",ab)] [("a*b",aaab)]
```

Try it yourself by executing `cabal repl examples -f examples`

What is a token?
---
A token is a parts' of string template. It consists of three parts. Each part
provides some restrictions on characters of a string that can be matched.
The main part of token is it's `body`. It describes characters of a string part,
matchable by token. Two others parts `behind` and `ahead` restrict symbols, that
can be situated before/after matched part respectively. Note that they are
assumed to be satisfied if begin/end of line is achieved.

Each part of token is a list, describing subsequent symbols from left to right.
In `behind` and `ahead` part we can specify for each position what symbols can
be used or cannot be used via `BlackWhiteSet`. In token's body we can restrict
not only one position, but some of subsequent positions. More precisely, we can
mark a `BlackWhiteSet` to be `Repeatable` one or some (it's one or more) times.

Syntax, used in examples
---
To make examples more readable we provide simple language for describing tokens.
We'll use alpha characters as symbols and some punctuation for describing
token's structure:
- `{` and `}` for grouping set of characters, containing more then one char;
- `!` means "all, except those" (`BlackSet` in this package' terminology);
- `*` behind the charset means "some characters, containing in this set";
- `?` at the beginning of `behind`/`ahead` parts;
- `<` and `>` for grouping complex `behind`/`ahead` parts.

The grammar in EBNF:
```ebnf
BlackWhiteSet     := ['!'], (letter | '{', {letter}, '}');
Repeatable        := charset, ['*'];

ahead_or_behind   := '?', (symbol | '<', {symbol}, '>');
body              := symbol, {symbol}
token             := [ahead_or_behind], body, [ahead_or_behind];
```
Parser for tokens, described in this manner is available in `examples/Main.hs`

Technical details
---
Uniqueness checking is provided by a modification of Sardinas-Patterson's
algorithm. Tokenizing process is written in the most simple way with
non-exponential asymptotic of the input string's length.

Usage
---
It's very likely, that all you need is exported from `Data.Text.Tokenizer`.

Bug reports and feature requests
---
Feel free open issues at
[the GitHub repo](https://github.com/Lev135/tokenizer/issues)

Contribution
---
I would be vary glad for any contribution. The are many ways to improve this lib:
- improve documentation and examples;
- add more tests to check, that everything works nice;
- improve performance (I think there are many opportunities here in both
  algorithms);
- add benchmarks (connected with the previous).
- *(this issue is mostly for me :)*
  improve code readability (I've tried not to make it absolutely terrible, but
  it's definitely not perfect);

I know, that some of those problems (especially code readability) should be
closed by myself, but unfortunately I have no time to deal with them now.

Maybe, I'm the package is too raw to publish it, but there are some reasons for
me to do so:
- I don't no when it will be improved enough;
- it is needed for my main project ([FineTeX](https://github.com/lev135/FineTeX)).
