const std = @import("std");
const asc = std.ascii;

const czpeg = @import("../czpeg.zig");
const util = @import("util.zig");
const P = czpeg.Pattern;

// taking cue from LPeg, make this (semantic only) distinction:
// 'P' is used to parse expressions, generating re meta-parser and
// 'R' is used to create expressions, generating output parser
// (although P does reuse `classes` table, which is on R...)
const R = P;

/// Parse PEG-style regular expression and return czpeg pattern
pub fn compile(comptime re: []const u8, comptime args: anytype) type {
    const RE = struct {
        const pattern = exp._(-1);

        pub const exp = spc._(alt);
        const expref = P.ref(&@This(), "exp", anyerror!type);

        const alt = P.seq(.{ "/", spc, seq })
            .foldRep(0, -1, seq, {}, foldAlt);

        const seq = pfx.foldRep(0, -1, genConst(R.pat(true)).gen, {}, foldCat);

        fn foldCat(comptime acc: *type, comptime pat: type) void {
            acc.* = P.cat(acc.*, pat);
        }

        const pfxref = P.ref(&@This(), "pfx", anyerror!type);
        pub const pfx = P.alt(.{
            .{ "&", spc, pfxref.grok(type, R.if_) },
            .{ "!", spc, pfxref.grok(type, R.not) },
            sfx,
        });

        const sfx = P.seq(.{
            .{
                P.grok(type, genRep(1, -1).gen, "+"),
                P.grok(type, genRep(0, -1).gen, "*"),
                P.grok(type, genRep(0, 1).gen, "?"),
                P.grok(type, rep, .{
                    "^", P.set("+-").cap().rep(0, 1), num
                }),
                //.{ "=>", spc, name }, // FIXME TBD
            },
            spc
        }).foldRep(0, -1, P.cat(pri, spc), {}, foldSfx);

        fn foldSfx(comptime acc: *type, comptime composer: type) void {
            acc.* = composer.compose(acc.*);
        }

        fn genRep(comptime nmin: comptime_int, comptime nmax: comptime_int) type {
            return struct {
                fn gen() type {
                    return struct {
                        fn compose(comptime pre: type) type {
                            return pre.rep(nmin, nmax);
                        }
                    };
                }
            };
        }

        fn rep(comptime spec: anytype) type {
            return if (spec[0] == null)
                genRep(spec[1], spec[1]).gen()
            else if (spec[0].?[0] == '+')
                genRep(spec[1], -1).gen()
            else
                genRep(0, spec[1]).gen();
        }

        const pri = P.alt(.{
            .{ "(", expref, ")" },
            str.grok(type, R.str),
            cls,
            def,
            P.grok(type, R.pos, "{}"),
            .{ "{", expref.grok(type, R.cap), "}" },
            P.grok(type, genConst(R.any(1)).gen, "."),
            P.grok(type, refArg, name.cap()),
        });

        fn genConst(comptime pat: type) type {
            return struct {
                fn gen() type { return pat; }
            };
        }

        fn refArg(comptime nm: []const u8) type {
            return @field(args, nm);
        }
    };

    comptime const merr = RE.pattern.matchLean(re);
    comptime const m =
        if (comptime (@typeInfo(@TypeOf(merr)) != .ErrorUnion)) merr
        else if (merr) |v| v
        else |e| @compileError(
            "error " ++ @errorName(e) ++ " while parsing pattern: '" ++ re ++ "'");
    if (comptime (m == null))
        @compileError("invalid pattern: \"" ++ re ++ "\"");
    return m.?;
}


const spc = P.alt(.{
    classes.s,
    .{ "//", P.charset(~(@as(u256, 1) << '\n')).rep(0, -1) },
}).rep(0, -1);

const name = word0._(word.rep(0, -1));

const cls = P.seq(.{
    "[",
    P.rep(0, 1, P.cap("^")),
    P.not("]")._(item).foldRep(0, -1, item, {}, foldAlt),
    "]"
}).grok(type, genCls);

fn genCls(comptime s: anytype) ?type {
    return
        if (s[0] == null) s[1]
        else s[1].inv();
}

fn foldAlt(comptime acc: *type, comptime pat: type) void {
    acc.* = R.alt(.{ acc.*, pat });
}

const item = P.alt(.{ def, range, P.grok(type, genChar, 1) });

fn genChar(comptime s: []const u8) type {
    return R.charset(1 << s[0]);
}

const range = P.seq(.{
    P.cap(1),
    "-",
    P.charset(~(@as(u256, 1) << ']')).cap()
}).grok(type, genRange);

fn genRange(comptime s: anytype) type {
    return R.span(s[0][0], s[1][0]);
}


const def = P.cat("%", name.grok(type, getDef));

fn getDef(comptime s: []const u8) type {
    return @field(classes, s);
}

const str = P.alt(.{
    .{ "'", P.not("'")._(1).rep(0, -1).cap(), "'" },
    .{ "\"", P.not("\"")._(1).rep(0, -1).cap(), "\"" },
});

const ParseError = error{Overflow,InvalidCharacter};
const num = classes.d.rep(1, -1).grok(ParseError!u32, parseU32Dec);

fn parseU32Dec(s: []const u8) !u32 {
    return try std.fmt.parseInt(u32, s, 10);
}

// use "word" definition consistent w/PCRE and LPeg (includes '_')
// vs isalnum/ascii.AlNum (no '_')
const word0 = R.alt(.{ "_", R.span('A', 'Z'), R.span('a', 'z') });
const word = R.alt(.{ word0, R.span('0', '9') });

fn invCls(comptime f: fn(u8)bool) type {
    return R.cls(asc.isAlpha).inv();
}

const classes = .{
    .a = R.cls(asc.isAlpha),
    .c = R.cls(asc.isCntrl),
    .d = R.cls(asc.isDigit),
    .g = R.cls(asc.isGraph),
    .l = R.cls(asc.isLower),
    .p = R.cls(asc.isPunct),
    .s = R.cls(asc.isSpace),
    .u = R.cls(asc.isUpper),
    .w = word,
    .x = R.cls(asc.isXDigit),

    .A = invCls(asc.isAlpha),
    .C = invCls(asc.isCntrl),
    .D = invCls(asc.isDigit),
    .G = invCls(asc.isGraph),
    .L = invCls(asc.isLower),
    .P = invCls(asc.isPunct),
    .S = invCls(asc.isSpace),
    .U = invCls(asc.isUpper),
    .W = word.inv(),
    .X = invCls(asc.isXDigit),
};


//----------------------------------------------------------------------------
const testing = std.testing;
const expect = testing.expect;
const expectStr = testing.expectEqualStrings;
const expectOk = util.expectOk;
const expectEqual = util.expectEqual;
const chkNoM = util.chkNoM;
const chkMatch = util.chkMatch;
const chkCap = util.chkCap;
const chkError = util.chkError;

// checkers that force comptime parser for testing patterns on R
// (works without sometimes, but not always)

fn ctChkNoM(comptime pat: anytype, comptime s: []const u8) void {
    comptime var p = czpeg.Parser.init(s, &util.noalloc);
    comptime const m = expectOk(p.match(pat));
    if (comptime (m != null)) {
        @compileLog("length", comptime p.pos(), "match", m);
        @compileError("expected no match, found match");
    }
    if (comptime (p.pos() != 0)) {
        @compileLog("length", comptime p.pos());
        @compileError("expected match length == 0, found > 0");
    }
}

fn ctChkCap(comptime pat: anytype, comptime s: []const u8, comptime n: usize)
    czpeg.MatchType(P.pat(pat))
{
    comptime var p = czpeg.Parser.init(s, &util.noalloc);
    comptime const m = expectOk(p.match(pat));
    if (comptime (m == null))
        @compileError("expected match, found null ("
                      ++ @typeName(@TypeOf(m)) ++ ")");
    if (comptime (n != p.pos())) {
        @compileLog("expected", n, "actual", comptime p.pos());
        @compileError("match length mismatch");
    }
    return comptime m.?;
}

fn ctChkMatch(comptime pat: anytype, comptime s: []const u8, comptime n: usize) void {
    comptime const m = ctChkCap(pat, s, n);
    if (comptime (m != {}))
        @compileError("expected non-capturing match, found "
                      ++ @typeName(@TypeOf(m)));
}

pub fn chkFull(comptime pat: anytype, s: []const u8) void {
    chkMatch(pat, s, s.len);
}


test "spc" {
    chkMatch(spc, "", 0);
    chkMatch(spc, "a", 0);
    chkMatch(spc, " ", 1);
    chkMatch(spc, " \t\n\r :O", 5);
}

test "name" {
    chkNoM(name, "");
    chkNoM(name, "42");
    chkMatch(name, "a ", 1);
    chkMatch(name, "_priv", 5);
    chkMatch(name, "yomama8yodog*", 12);
    chkMatch(name, "a_b_", 4);
}

test "num" {
    chkNoM(num, "");
    chkNoM(num, "a");
    expectEqual(@as(u32, 0), chkCap(num, "0", 1));
    expectEqual(@as(u32, 42), chkCap(num, "42", 2));
    chkError(num, "9999999999", error.Overflow);
}

test "str" {
    chkNoM(str, "");
    chkNoM(str, "a");
    expectEqual(@as([]const u8, "a"), chkCap(str, "'a'b", 3));
    expectEqual(@as([]const u8, "yomama"), chkCap(str, "\"yomama\"", 8));
}

test "def" {
    ctChkNoM(def, "");
    ctChkNoM(def, "d");
    const p = ctChkCap(def, "%d", 2);
    expect(@TypeOf(p) == type);
    chkNoM(p, "a");
    chkMatch(p, "42", 1);
}

test "cls ]" {
    const p = ctChkCap(cls, "[]]", 3);
    expect(@TypeOf(p) == type);
    chkNoM(p, "");
    chkNoM(p, "a");
    chkMatch(p, "]", 1);
}

test "cls range" {
    const p = ctChkCap(cls, "[w-y]", 5);
    expect(@TypeOf(p) == type);
    chkNoM(p, "");
    chkNoM(p, "]");
    chkNoM(p, "v");
    chkNoM(p, "z");
    chkMatch(p, "w", 1);
    chkMatch(p, "x", 1);
    chkMatch(p, "y", 1);
}

test "cls def" {
    const p = ctChkCap(cls, "[%x]", 4);
    expect(@TypeOf(p) == type);
    chkNoM(p, "");
    chkNoM(p, "g");
    chkMatch(p, "a", 1);
    chkMatch(p, "A", 1);
    chkMatch(p, "5", 1);
}

test "cls" {
    ctChkNoM(cls, "");

    const p = ctChkCap(cls, "[]_a-c%d%u-]", 12);
    expect(@TypeOf(p) == type);
    chkNoM(p, "");
    chkNoM(p, "d");
    chkNoM(p, "z");
    chkNoM(p, "[");
    chkNoM(p, ".");
    chkNoM(p, "%");
    chkMatch(p, "]", 1);
    chkMatch(p, "8", 1);
    chkMatch(p, "M", 1);
    chkMatch(p, "a", 1);
    chkMatch(p, "b", 1);
    chkMatch(p, "c", 1);
    chkMatch(p, "_", 1);
    chkMatch(p, "-", 1);
}

test "cls neg" {
    const p = ctChkCap(cls, "[^]_a-c%d%u-]", 13);
    expect(@TypeOf(p) == type);
    chkNoM(p, "");
    chkNoM(p, "]");
    chkNoM(p, "8");
    chkNoM(p, "M");
    chkNoM(p, "a");
    chkNoM(p, "b");
    chkNoM(p, "c");
    chkNoM(p, "_");
    chkNoM(p, "-");
    chkMatch(p, "d", 1);
    chkMatch(p, "z", 1);
    chkMatch(p, "[", 1);
    chkMatch(p, ".", 1);
    chkMatch(p, "%", 1);
}

test "compile str" {
    const p = compile("'a'", .{});
    expect(@TypeOf(p) == type);
    chkNoM(p, "b");
    chkMatch(p, "aa", 1);
}

test "compile cls" {
    const p = compile(" [ac] ", .{});
    expect(@TypeOf(p) == type);
    chkNoM(p, "b");
    chkMatch(p, "a", 1);
}

test "compile def" {
    const p = compile("%d", .{});
    expect(@TypeOf(p) == type);
    chkNoM(p, "c");
    chkMatch(p, "4", 1);
}

test "compile any(1)" {
    const p = compile(".", .{});
    expect(@TypeOf(p) == type);
    chkNoM(p, "");
    chkMatch(p, "ab", 1);
}

test "group" {
    const p = compile("((%a()))", .{});
    expect(@TypeOf(p) == type);
    chkNoM(p, "0");
    chkMatch(p, "za", 1);
}

test "rep +" {
    comptime const p = compile("'a'+", .{});
    chkNoM(p, "");
    chkNoM(p, "b");
    chkMatch(p, "a", 1);
    chkMatch(p, "aaa", 3);
    chkMatch(p, "aaba", 2);
}

test "rep *" {
    comptime const p = compile("'a' *", .{});
    chkMatch(p, "", 0);
    chkMatch(p, "b", 0);
    chkMatch(p, "a", 1);
    chkMatch(p, "aaa", 3);
    chkMatch(p, "aaba", 2);
}

test "rep ?" {
    comptime const p = compile("'a'?", .{});
    chkMatch(p, "", 0);
    chkMatch(p, "b", 0);
    chkMatch(p, "a", 1);
    chkMatch(p, "aaa", 1);
}

test "rep n" {
    comptime const p = compile("'a'^3", .{});
    chkNoM(p, "");
    chkNoM(p, "aa");
    chkNoM(p, "aba");
    chkMatch(p, "aaa", 3);
    chkMatch(p, "aaaa", 3);
}

test "rep +n" {
    comptime const p = compile("'a'^+3", .{});
    chkNoM(p, "");
    chkNoM(p, "aa");
    chkNoM(p, "aba");
    chkMatch(p, "aaa", 3);
    chkMatch(p, "aaaa", 4);
    chkMatch(p, "aaaba", 3);
}

test "rep -n" {
    comptime const p = compile("'a'^-3", .{});
    chkMatch(p, "", 0);
    chkMatch(p, "b", 0);
    chkMatch(p, "a", 1);
    chkMatch(p, "aa", 2);
    chkMatch(p, "aaa", 3);
    chkMatch(p, "aaaa", 3);
    chkMatch(p, "ababa", 1);
}

test "capture pos" {
    {
        const p = compile("{}", .{});
        expect(@TypeOf(p) == type);
        expectEqual(@as(usize, 0), chkCap(p, " ", 0));
    }
    {
        const p = compile("'a'+ {} 'b'*", .{});
        expect(@TypeOf(p) == type);
        chkNoM(p, "");
        chkNoM(p, "b");
        expectEqual(@as(usize, 1), chkCap(p, "a", 1));
        expectEqual(@as(usize, 2), chkCap(p, "aab", 3));
    }
}

test "capture raw" {
    const p = compile("'Hello' %s+ {%w+}'!'", .{});
    chkNoM(p, "Hello yomama");
    expectStr("world", chkCap(p, "Hello world!", 12));
}

test "pfx" {
    {
        const p = compile("&'a'", .{});
        chkNoM(p, "ba");
        chkMatch(p, "aa", 0);
    }
    {
        const p = compile("! 'a'", .{});
        chkNoM(p, "aa");
        chkMatch(p, "ba", 0);
    }
}

test "seq" {
    const p = compile("'a'+ 'b'+ 'c'*", .{});
    chkNoM(p, "");
    chkNoM(p, "a");
    chkNoM(p, "ba");
    chkMatch(p, "ab", 2);
    chkMatch(p, "abc", 3);
    chkMatch(p, "abcccd", 5);
    chkMatch(p, "aaaabb", 6);
}

test "alt" {
    const p = compile("'a' / 'ab' / 'ba'", .{});
    chkNoM(p, "");
    chkMatch(p, "aaa", 1);
    chkMatch(p, "aba", 1); // NB 'a' matches first!
    chkMatch(p, "baa", 2);
}

// re grammar
// edited from http://www.inf.puc-rio.br/~roberto/lpeg/re.html
//  * remove multiple rule grammars entirely (just integrate w/czpeg instead)
//  * remove old-style "<name>" non-terminals
//  * remove substitution, table, fold, string, number and -> captures
//  * remove named and anonymous group captures
//  * remove back-references
//  * change comments to use "//" for consistency w/zig

// pattern  <- exp !.
// exp      <- S seq ('/' S seq)*
// seq      <- prefix*
// prefix   <- '&' S prefix
//           / '!' S prefix
//           / suffix
// suffix   <- primary S (([+*?]
//           / '^' [+-]? num
//           / '=>' S name) S)*
//
// primary  <- '(' exp ')'
//           / string
//           / class
//           / def
//           / '{}'
//           / '{' exp '}'
//           / '.'
//           / name
//
// class    <- '[' '^'? item (!']' item)* ']'
// item     <- def / range / .
// range    <- . '-' [^]]
//
// S        <- (%s / '//' [^\n]*)*      // spaces and comments
// name     <- [%a_]%w*
// num      <- %d+
// string   <- '"' [^"]* '"' / "'" [^']* "'"
// def      <- '%' name

test "PEG pattern grammar" {
    const PEG = struct {
        pub const pattern = compile("exp !.", refs);
        pub const exp = compile("S seq ('/' S seq)*", refs);
        pub const seq = compile("prefix*", refs);
        pub const prefix = compile(
            \\   '&' S prefix
            \\ / '!' S prefix
            \\ / suffix
        , refs);
        pub const suffix = compile(
            \\   primary S (([+*?]
            \\ / '^' [+-]? %d+
            \\ / '=>' S name) S)*
        , refs);

        pub const primary = compile(
            \\   '(' exp ')'
            \\ / string
            \\ / '[' '^'? item (!']' item)* ']'
            \\ / def
            \\ / '{}'
            \\ / '{' exp '}'
            \\ / '.'
            \\ / name
        , refs);

        pub const item_ = compile("def / . '-' [^]] / .", refs);

        pub const S = compile("(%s / '//' [^\n]*)* // spaces and comments", .{});
        pub const name_ = compile("[_%a]%w*", .{});
        pub const string = compile(
            \\ '"' [^"]* '"' / "'" [^']* "'"
        , .{});
        pub const def_ = compile("'%' name", refs);

        fn ref(nm: []const u8) type {
            return P.ref(&@This(), nm, void);
        }

        const refs = .{
            .exp = ref("exp"),
            .seq = ref("seq"),
            .prefix = ref("prefix"),
            .suffix = ref("suffix"),
            .primary = ref("primary"),
            .item = ref("item_"),
            .S = ref("S"),
            .name = ref("name_"),
            .string = ref("string"),
            .def = ref("def_"),
        };
    };

    chkNoM(PEG.pattern, "<-");
    chkFull(PEG.pattern, "exp !.");
    chkFull(PEG.pattern, "S seq ('/' S seq)*");
    chkFull(PEG.pattern, "prefix*");
    chkFull(PEG.pattern,
        \\   '&' S prefix
        \\ / '!' S prefix
        \\ / suffix
    );
    chkFull(PEG.pattern,
        \\   primary S (([+*?]
        \\ / '^' [+-]? %d+
        \\ / '=>' S name) S)*
    );
    chkFull(PEG.pattern,
        \\   '(' exp ')'
        \\ / string
        \\ / '[' '^'? item (!']' item)* ']'
        \\ / def
        \\ / '{}'
        \\ / '{' exp '}'
        \\ / '.'
        \\ / name
    );
    chkFull(PEG.pattern, "def / . '-' [^]] / .");
    chkFull(PEG.pattern, "(%s / '//' [^\n]*)* // spaces and comments");
    chkFull(PEG.pattern,
        \\ '"' [^"]* '"' / "'" [^']* "'"
    );
    chkFull(PEG.pattern, "'%' name");
}
