const std = @import("std");
const asc = std.ascii;
const TypeInfo = std.builtin.TypeInfo;
const Allocator = std.mem.Allocator;

const czpeg = @import("../czpeg.zig");
const Parser = czpeg.Parser;
const P = czpeg.Pattern;

// taking cue from LPeg, make this (semantic only) distinction:
// 'P' is used to parse expressions, generating re meta-parser and
// 'R' is used to create expressions, generating output parser
// (although P does reuse `classes` table, which is on R...)
const R = P;

/// build parser by parsing PEG pattern or grammar.  returns toplevel pattern
/// or first grammar rule.
pub fn compile(comptime re: []const u8, comptime args: anytype) type {
    comptime const G = Preparse.GrammarType(re);
    comptime var g: G = undefined;

    const RE = struct {
        const pattern = exp._(-1);

        pub const exp = spc._(.{ grammar, alt });
        const expref = P.ref(&@This(), "exp", type);

        const grammar = rule.foldRep(0, -1, rule, {}, foldFirst);

        fn foldFirst(comptime acc: *type, comptime rule: type) !void { }

        pub const rule = P.seq(.{ name.cap(), spc, "<-", expref })
            .grok(type, saveRule);

        fn saveRule(comptime spec: anytype) type {
            @field(g, spec[0]) = spec[1];
            return spec[1];
        }

        const alt = P.seq(.{ "/", spc, seq })
            .foldRep(0, -1, seq, {}, foldAlt);

        fn foldAlt(comptime acc: *type, comptime pat: type) !void {
            acc.* = R.alt(.{ acc.*, pat });
        }

        const seq = pfx.foldRep(0, -1, genConst(R.pat(true)).gen, {}, foldCat);

        fn foldCat(comptime acc: *type, comptime pat: type) !void {
            acc.* = P.cat(acc.*, pat);
        }

        const pfxref = P.ref(&@This(), "pfx", type);
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

        fn foldSfx(comptime acc: *type, comptime composer: type) !void {
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
            P.grok(type, refRule, name._(P.not(.{ spc, "<-" })).cap()),
        });

        fn genConst(comptime pat: type) type {
            return struct {
                fn gen() type { return pat; }
            };
        }

        fn refRule(comptime nm: []const u8) type {
            if (!@hasField(@TypeOf(g), nm))
                @compileError("undefined reference to rule " ++ nm);
            return @field(g, nm); // R.ref(&g, nm, type);
        }
    };

    comptime const m = if (RE.pattern.matchLean(re)) |v| v
        else |e| @compileError(
            "error " ++ @errorName(e) ++ " while parsing pattern: '" ++ re ++ "'");
    if (comptime (m == null))
        @compileError("invalid pattern: \"" ++ re ++ "\"");
    return m.?;
}


const Preparse = struct {
    const first_rule = P.seq(.{ spc, name.cap(), spc, "<-" });

    const findRule = struct {
        pub fn parse(p: *Parser) !?[]const u8 {
            while (true) {
                _ = try P.charset(~id0set).rep(0, -1).parse(p);
                if (try P.seq(.{ name.cap(), spc, "<-" }).parse(p)) |r|
                    return r;
                if ((try P.charset(id0set).rep(1, -1).parse(p)) == null)
                    return null;
            }
        }
    };

    const count_rules = P.foldRep(0, -1, findRule, 0, {}, foldCount);

    fn foldCount(acc: *usize, c: []const u8) !void {
        acc.* += 1;
    }

    /// generate struct type to hold named productions
    fn GrammarType(re: []const u8) type {
        const n = (try count_rules.matchLean(re)).?;
        var fields: [n]TypeInfo.StructField = undefined;
        var i = 0;
        var p = Parser.init(re, &noalloc);
        @setEvalBranchQuota(1<<16);
        while (try findRule.parse(&p)) |r| {
            fields[i] = .{
                .name = r,
                .field_type = type,
                .default_value = @as(?type, null),
                .is_comptime = false,
                .alignment = 0,
            };
            i += 1;
        }

        return @Type(.{
            .Struct = .{
                .layout = .Auto,
                .fields = &fields,
                .decls = &[_]TypeInfo.Declaration{},
                .is_tuple = false,
            }
        });
    }
};


const spc = P.alt(.{
    classes.s,
    .{ "//", P.charset(~(@as(u256, 1) << '\n')).rep(0, -1) },
}).rep(0, -1);

const name = P.charset(id0set)._(classes.w.rep(0, -1));

const cls = P.seq(.{
    "[",
    P.rep(0, 1, P.cap("^")),
    mergeItems,
    "]"
}).grok(type, genCls);

fn genCls(comptime s: anytype) ?type {
    return if (s[0] == null) s[1] else R.charset(~s[1].chars);
}

/// fold character classes without allocating captures
const mergeItems = struct {
    pub fn parse(p: *Parser) !?type {
        const c0 = try item.parse(p);
        if (c0 == null)
            @compileError("unexpected end of pattern in class");

        comptime var cs: u256 = c0.?.chars;

        while (true) {
            if (try p.get(1)) |s| {
                if (s[0] == ']')
                    break;
                if (comptime try item.parse(p)) |c| {
                    cs |= c.chars;
                    continue;
                }
            }
            @compileError("unexpected end of pattern in class");
        }

        return R.charset(cs);
    }
};

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
    return R.charset(spanset(s[0][0], s[1][0]));
}


const def = P.cat("%", name.grok(type, getDef));

fn getDef(comptime s: []const u8) type {
    return @field(classes, s);
}

const str = P.alt(.{
    .{ "'", P.not("'")._(1).rep(0, -1).cap(), "'" },
    .{ "\"", P.not("\"")._(1).rep(0, -1).cap(), "\"" },
});

const num = classes.d.rep(1, -1).grok(u32, parseU32Dec);

fn parseU32Dec(s: []const u8) !u32 {
    return try std.fmt.parseInt(u32, s, 10);
}

fn spanset(comptime lo: u8, comptime hi: u8) u256 {
    comptime var cs: u256 = 0;
    comptime var c = lo;
    while (c <= hi) : (c += 1)
        cs |= 1 << c;
    return cs;
}

fn ascset(comptime f: fn(u8) bool) u256 {
    comptime var cs: u256 = 0;
    comptime var c = 0;

    @setEvalBranchQuota(1<<16);
    inline while (c < 256) : (c += 1) {
        if (comptime f(c))
            cs |= 1 << c;
    }
    return cs;
}

const id0set = spanset('A', 'Z') | 1<<'_' | spanset('a', 'z');
const wordset = id0set | spanset('0', '9');

fn mkdefP(comptime f: fn(u8) bool) type {
    return comptime R.charset(ascset(f));
}

fn mkdefN(comptime f: fn(u8) bool) type {
    return comptime R.charset(~ascset(f));
}

const classes = .{
    .a = mkdefP(asc.isAlpha),
    .c = mkdefP(asc.isCntrl),
    .d = mkdefP(asc.isDigit),
    .g = mkdefP(asc.isGraph),
    .l = mkdefP(asc.isLower),
    .p = mkdefP(asc.isPunct),
    .s = mkdefP(asc.isSpace),
    .u = mkdefP(asc.isUpper),
    .x = mkdefP(asc.isXDigit),

    .A = mkdefN(asc.isAlpha),
    .C = mkdefN(asc.isCntrl),
    .D = mkdefN(asc.isDigit),
    .G = mkdefN(asc.isGraph),
    .L = mkdefN(asc.isLower),
    .P = mkdefN(asc.isPunct),
    .S = mkdefN(asc.isSpace),
    .U = mkdefN(asc.isUpper),
    .X = mkdefN(asc.isXDigit),

    // use "word" definition consistent w/PCRE and LPeg (includes '_')
    // vs isalnum/ascii.AlNum (no '_')
    .w = R.charset(wordset),
    .W = R.charset(~wordset),
};


// FIXME can't reuse from czpeg...
var noalloc = Allocator {
    .allocFn = noAlloc,
    .resizeFn = noResize,
};

fn noAlloc(a: *Allocator, b: usize, c: u29, d: u29, e: usize) Allocator.Error![]u8 {
    std.debug.panic("Parser attempted to alloc during compile", .{});
}

fn noResize(a: *Allocator, b: []u8, c: u29, d: usize, e: u29, f: usize) Allocator.Error!usize {
    unreachable;
}


//----------------------------------------------------------------------------
const testing = std.testing;
const expect = testing.expect;
const expectStr = testing.expectEqualStrings;
const expectEqual = czpeg.expectEqual;
const chkNoM = czpeg.chkNoM;
const chkMatch = czpeg.chkMatch;
const chkCap = czpeg.chkCap;

// checkers that force comptime parser for testing patterns on R
// (works without sometimes, but not always)

pub fn ctChkNoM(comptime pat: anytype, comptime s: []const u8) void {
    comptime var p = Parser.init(s, &noalloc);
    comptime const m = czpeg.expectOk(p.match(pat));
    if (comptime (m != null)) {
        @compileLog("length", comptime p.pos(), "match", m);
        @compileError("expected no match, found match");
    }
    if (comptime (p.pos() != 0)) {
        @compileLog("length", comptime p.pos());
        @compileError("expected match length == 0, found > 0");
    }
}

pub fn ctChkCap(comptime pat: anytype, comptime s: []const u8, comptime n: usize)
    czpeg.MatchType(P.pat(pat))
{
    comptime var p = Parser.init(s, &noalloc);
    comptime const m = czpeg.expectOk(p.match(pat));
    if (comptime (m == null))
        @compileError("expected match, found null ("
                      ++ @typeName(@TypeOf(m)) ++ ")");
    if (comptime (n != p.pos())) {
        @compileLog("expected", n, "actual", comptime p.pos());
        @compileError("match length mismatch");
    }
    return comptime m.?;
}

pub fn ctChkMatch(comptime pat: anytype, comptime s: []const u8, comptime n: usize) void {
    comptime const m = ctChkCap(pat, s, n);
    if (comptime (m != {}))
        @compileError("expected non-capturing match, found "
                      ++ @typeName(@TypeOf(m)));
}


test "spc" {
    ctChkMatch(spc, "", 0);
    ctChkMatch(spc, "a", 0);
    ctChkMatch(spc, " ", 1);
    ctChkMatch(spc, " \t\n\r :O", 5);
}

test "name" {
    ctChkNoM(name, "");
    ctChkNoM(name, "42");
    ctChkMatch(name, "a ", 1);
    ctChkMatch(name, "_priv", 5);
    ctChkMatch(name, "yomama8yodog*", 12);
    ctChkMatch(name, "a_b_", 4);
}

test "num" {
    ctChkNoM(num, "");
    ctChkNoM(num, "a");
    expectEqual(@as(u32, 0), ctChkCap(num, "0", 1));
    expectEqual(@as(u32, 42), ctChkCap(num, "42", 2));
}

test "str" {
    ctChkNoM(str, "");
    ctChkNoM(str, "a");
    expectEqual(@as([]const u8, "a"), ctChkCap(str, "'a'b", 3));
    expectEqual(@as([]const u8, "yomama"), ctChkCap(str, "\"yomama\"", 8));
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

test "Preparse.count_rules" {
    const G = struct {
        fn chkCount(exp: usize, comptime s: []const u8) void {
            expectEqual(exp, czpeg.expectOk(Preparse.count_rules.matchLean(s)).?);
        }
    };

    G.chkCount(0, "");
    G.chkCount(0, "yomama wuz here");
    G.chkCount(1, " ( a <- )");
    G.chkCount(1, " a <- ");
    G.chkCount(1, "a <- <- invalid anyway");
    G.chkCount(3, "a<-some junk ... b <- and more / c <- %<[] <-");
}

test "Preparse.GrammarType" {
    const G = struct {
        fn chkRules(comptime s: []const u8, comptime exp: anytype) void {
            comptime const Rules = Preparse.GrammarType(s);
            comptime const fields = @typeInfo(Rules).Struct.fields;
            expectEqual(exp.len, fields.len);
            inline for (exp) |nm, i|
                expectEqual(@as([]const u8, nm), fields[i].name);
        }
    };
    G.chkRules("", .{});
    G.chkRules("a <- ...", .{ "a" });
    G.chkRules("meh {(x <- ...) y<-foo z<-bar}", .{ "x", "y", "z" });
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

test "rule" {
    const p = compile("(ab <- 'a'+ 'b'+) ab", .{});
    chkNoM(p, "");
    chkNoM(p, "ab");
    chkMatch(p, "abab", 4);
}

// re grammar
// edited from http://www.inf.puc-rio.br/~roberto/lpeg/re.html
//  * remove old-style "<name>" non-terminals
//  * remove substitution, table, fold, string, number and -> captures
//  * remove named and anonymous group captures
//  * remove back-references
//  * change comments to use "//" for consistency w/zig

const test_grammar =
  \\ pattern	<- exp !.
  \\ exp	<- S (grammar / alt)
  \\
  \\ grammar	<- rule+
  \\ rule	<- name arrow exp
  \\
  \\ alt	<- seq ('/' S seq)*
  \\ seq	<- prefix*
  \\ prefix	<- '&' S prefix
  \\		 / '!' S prefix
  \\		 / suffix
  \\ suffix	<- primary S (([+*?]
  \\		     / '^' [+-]? num
  \\		     / '=>' S name) S)*
  \\
  \\ primary	<- '(' exp ')'
  \\		 / string
  \\		 / class
  \\		 / def
  \\		 / '{}'
  \\		 / '{' exp '}'
  \\		 / '.'
  \\		 / name !arrow
  \\
  \\ class	<- '[' '^'? item (!']' item)* ']'
  \\ item	<- def / range / .
  \\ range	<- . '-' [^]]
  \\
  \\ S		<- (%s / '//' [^\n]*)*		// spaces and comments
  \\ name	<- [%a_]%w*
  \\ arrow	<- S '<-'
  \\ num	<- %d+
  \\ string	<- '"' [^"]* '"' / "'" [^']* "'"
  \\ def	<- '%' name
;
