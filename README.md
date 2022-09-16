Tokenizer
===

This package provides solution for two problems:
- split input string on tokens of specificated sort;
- check tokenizing of *all possible* strings is unique.

The language for tokens' definitions is a subset of regular expressions
with stars and alternatives only over symbols.

Although package itself uses data structures over arbitrary char type `c` to
describe tokens and input strings, we will use special (simple enough)
regexp-like language for describing examples. We will use letters for symbols
and special characters for syntax constructions:
```ebnf
charset    := ['!'], (letter | '(', {letter}, ')');
symbol     := charset, ['*'];
look_aside := '?', (symbol | '<', {symbol}, '>');
token      := [look_aside], {symbol}, [look_aside];
```
As with regexps semantics for these constructions is that they accept some parts
of strings and rejects other:
- Positive charset (without `!`) accepts only listed characters, while negative
(with `!` before it) rejects those. So `!a` means "all characters, except `a`",
while `(abc)` means "only `a` or `b` or `c`".
- Charset with star after it means that there can be zero or more characters of
this kind. So `(ab)*c` means "many `a` or `b` and one `c` after them" and
accepts `c`, `ac`, `bac`, but rejects `ab`.
- Besides consumable characters token can contain lookahead/behind section,
starting from `?`. It controls surrounding characters if they are provided.
The latter means, that they will never fail if a line ends/not starts earlier.
So `?ab?c` accepts `b` symbol from `b`, `ab`, `bc`, `abc`, but rejects from
`bd` and `db`.

In unordered mode we work with some set of tokens. The first thing we want to
know is if every line is uniquely splitable on these tokens. For example, for
tokens `ab`, `a*` it is so, but if we add `bab`, string `abab` can be split as
`a bab` or `ab ab`
