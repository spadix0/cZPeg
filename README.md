# Compile-Time Parsing Expression Grammars for Zig

*cZPeg* generates recursive descent parsers *at compile-time* in [Zig] using a
functional API inspired by [LPeg] to declare and compose patterns.  There is
also an `re` module (WiP) for generating parsers from more compact PEG-style
pattern strings.


### Install

Make sure you have a recent [Zig], we're currently testing with 0.8.0-dev
drops from early 2021.

Until Zig's package distribution infrastructure has evolved, it's probably
easiest to simply clone the contents of the `czpeg/src/` tree into your
project somewhere, then from your `build.zig`, add:

```zig
// build.zig

// if this is your LibExeObjStep
const exe = b.addExecutable(...);

// add new package like this
exe.addPackagePath("czpeg", "lib/czpeg.zig");
```

Now you can import and use the package in your project:

```zig
const std = @import("std");
const czpeg = @import("czpeg");
test "greeting" {
    const pat = czpeg.Pattern.pat;
    const p = pat(.{ "Hello", pat(" ").rep(1, -1), pat(1).rep(1, -1).cap() });
    std.testing.expectEqualStrings("world", p.matchLean("Hello world").?);
}
```


### Test

From the cZPeg root, run all available tests using the usual chant:

```console
$ zig build test
```

It's a good idea to run this regularly as Zig matures to make sure everything
stays in sync.


### API

A brief interface comparison for reference.  `p` and `q` are other patterns,
`n` is a non-negative integer:

| cZPeg                     | cZPeg.re  | LPeg         | LPeg.re   | PCRE      |
|---------------------------|-----------|--------------|-----------|-----------|
| `pat("s")`<br>`str("s")`  | `'s'`<br>`"s"` | `P(s)`<br>`S(s)` | `'s'`<br>`"s"` | `s` |
| `pat(1)`<br>`any(1)`      | `.`       | `P(1)`       | `.`       | `.`       |
| `pat(n)`<br>`any(n)`      | `.^n`     | `P(n)`       | `.^n`     | `.{n}`    |
| `set("aeiou")`            | `[aeiou]` | `S('aeiou')` | `[aeiou]` | `[aeiou]` |
| `span('0', '9')`          | `[0-9]`<br>`%d` | `R('09')` | `[0-9]`<br>`%d` | `[0-9]`<br>`\d` |
| `pat(-1)`                 | `!.`      | `P(-1)`      | `!.`      | `$`       |
| `rep(0, 1, p)`<br>`p.rep(0, 1)` | `p?` | `patt^-1`   | `p?`      | `p?`      |
| `rep(0, -1, p)`<br>`p.rep(0, -1)` | `p*` | `patt^0`  | `p*`      | `p*`      |
| `rep(1, -1, p)`<br>`p.rep(1, -1)` | `p+` | `patt^1`  | `p+`      | `p+`      |
| `rep(n, -1, p)`<br>`p.rep(n, -1)` | `p^+n` | `p^n`   | `p^+n`    | `p{n,}`   |
| `rep(0, n, p)`<br>`p.rep(0, n)` | `p^-n` | `p^-n`    | `p^-n`    | `p{,n}`   |
| `rep(n, n, p)`<br>`p.rep(n, n)` | `p^n` |            | `p^n`     | `p{n}`    |
| `pat(.{p, q})`<br>`seq(.{p, q})` | `p q` | `p * q`   | `p q`     | `pq`      |
| `alt(.{p, q})`            | `p / q`   | `p + q`      | `p / q`   | `p\|q`    |
| `if_(p)`                  | `&p`      | `#p`         | `&p`      | `(?=p)`   |
| `not(p)`                  | `!p`      | `-p`         | `!p`      | `(?!p)`   |
| `(p)`                     | `(p)`     | `(p)`        | `(p)`     | `(:?p)`   |
| `pos()`                   | `{}`      | `Cp()`       | `{}`      |           |
| `cap(p)`<br>`p.cap()`     | `{p}`     | `C(p)`       | `{p}`     | `(p)`     |
| `grok(type, f, p)`<br>`p.grok(type, f)` | `p => f` | `Cmt(p, f)` | `p => f` | |
| `foldRep()`               |           | `Cf()`       | `~>`      |           |
| `ref(&g, "name", type)`   | `name`    | `V('name')`  | `name`    |           |
| `name = p`                | `name <- p` | `{ name = p }` | `name <- p` |     |
| unsupported | | `/ cap` `B` `Carg`<br>`Cb` `Cc` `Cg` `Cs` | `{:p:}` `{~p~}` `{\|p\|}`<br>`=name` `->` | *`\n`* |


[Zig]: https://ziglang.org
[LPeg]: http://www.inf.puc-rio.br/~roberto/lpeg/
