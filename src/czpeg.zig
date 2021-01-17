const std = @import("std");
const math = std.math;
const meta = std.meta;
const Allocator = std.mem.Allocator;

pub const re = @import("czpeg/re.zig");

/// Namespace for "pattern" factories.  Each pattern is a new (duck) type that
/// implements `.parse()`.  Pattern factories introspect their parameters *at
/// compile time* to generate appropriate types.  It is considered appropriate
/// to `usingnamespace czpeg.Pattern` from a local pattern/grammar namespace.

pub const Pattern = struct {
    /// match generic pattern arg, which can be one of
    ///   * pattern — returns existing pattern directly
    ///   * string — matches string literally (`str(pattern)`)
    ///   * int n >= 0 — matches exact number of characters (`any(n)`)
    ///   * int n < 0 — succeeds only if there are fewer than -n input
    ///       characters remaining (`not(any(-n))`)
    ///   * true — always succeeds without consuming any input (`any(0)`)
    ///   * false — always fails without consuming any input (`not(any(0))`)
    ///   * tuple — matches concatenated sequence of patterns (`seq(pattern)`)
    ///   * function — calls function directly to perform match

    pub fn pat(comptime pattern: anytype) type {
        return comptime switch (@typeInfo(@TypeOf(pattern))) {
            .Type => pattern,
            .Struct => seq(pattern),
            .Pointer => str(pattern),
            .ComptimeInt, .Int => if (pattern >= 0)
                    any(pattern)
                else
                    not(any(-pattern)),
            .Bool => if (pattern)
                    any(0)
                else
                    not(any(0)),
            .Fn => wrap(pattern),
            else => @compileError(
                "Unsupported match on `" ++ @typeName(@TypeOf(pattern)) ++ "`")
        };
    }

    fn wrap(comptime f: anytype) type {
        comptime const R = @typeInfo(@TypeOf(f)).Fn.return_type.?;
        if (comptime (R == type)) // NB parse() fns always return optional
            return f();

        comptime const E = ErrorOf(R);
        comptime const T = StripOption(StripError(R));
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                return f(p);
            }
        };
    }

    // terminal patterns

    /// match string `pattern` literally
    pub fn str(comptime pattern: []const u8) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) ?void {
                if (p.get(pattern.len)) |s| {
                    if (std.mem.eql(u8, s, pattern)) {
                        p.take(pattern.len);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    /// match exactly `n` characters
    pub fn any(comptime n: comptime_int) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) ?void {
                if (p.get(n)) |_| {
                    p.take(n);
                    return {};
                }
                return null;
            }
        };
    }

    /// match any 1 character based on provided function
    /// (eg see std.ascii.is*)
    pub fn cls(comptime f: fn(u8)bool) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) ?void {
                if (p.get(1)) |s| {
                    if (f(s[0])) {
                        p.take(1);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    /// match any character from bit set `chars`
    pub fn charset(comptime chars_: u256) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());
            pub const chars = chars_;

            pub fn parse(p: *Parser) ?void {
                if (p.get(1)) |s| {
                    if (chars_ & @as(u256, 1)<<s[0] != 0) {
                        p.take(1);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    /// match any 1 character found in string `chars`
    pub fn set(comptime chars: []const u8) type {
        if (comptime (chars.len == 0))
            return comptime pat(false);
        if (comptime (chars.len == 1))
            return comptime str(chars);

        comptime var cs: u256 = 0;
        @setEvalBranchQuota(1<<16);
        inline for (chars) |c|
            cs |= 1 << c;

        return charset(cs);
    }

    /// match any 1 character in range `lo` to `hi` inclusive
    pub fn span(comptime lo: u8, comptime hi: u8) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) ?void {
                if (p.get(1)) |s| {
                    if (lo <= s[0] and s[0] <= hi) {
                        p.take(1);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    // non-terminal pattern compositions

    /// indirect reference to another pattern.  used, eg, to break grammar
    /// rule reference cycles.  return type cannot be introspected (without
    /// reintroducing cycle), so must be provided.
    pub fn ref(
        comptime scope: *const type,
        comptime name: []const u8,
        comptime R: type,
    ) type {
        comptime const E = ErrorOf(R);
        comptime const T = StripError(R);

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                return @field(scope.*, name).parse(p);
            }
        };
    }

    /// matches only if pattern does not match (negative lookahead assertion)
    pub fn not(comptime pattern: anytype) type {
        comptime const P = pat(pattern);
        comptime const E = MatchError(P);
        if (comptime (MatchType(P) != void)) {
            @compileLog(MatchType(P));
            @compileError("Unsupported capture in not()");
        }

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, void) {
                const saved = p.save();
                defer p.restore(saved);

                const err = P.parse(p);
                const opt = if (comptime canError(@TypeOf(err))) try err else err;
                return if (opt == null) {} else null;
            }
        };
    }

    /// matches pattern but consumes no input (positive lookahead assertion)
    pub fn if_(comptime pattern: anytype) type {
        comptime const P = pat(pattern);
        comptime const E = MatchError(P);
        comptime const T = MatchType(P);

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const saved = p.save();
                defer p.restore(saved);

                return P.parse(p);
            }
        };
    }

    pub const Folder = struct {
        /// initial value for accumulator.  one of:
        ///   * comptime initial value of type T
        ///   * fn(Parser)T called to produce initial value (may error)
        init: anytype = {},

        /// optional function to cleanup after init() and fold().  only called
        /// on error or if partially matched pattern fails.  after calling
        /// init(), always either deinit will be called or accumulator will be
        /// returned as capture result (must not error).
        ///     fn(Parser, accumulator: *T) void
        deinit: anytype = {},

        /// function to merge new capture into current accumulator,
        /// signature like (may error):
        ///     fn(accumulator: *T, capture: anytype) void
        fold: anytype,
    };

    /// match `pattern` repeatedly, using supplied function(s) to accumulate
    /// captures.
    ///   * `nmin` to `nmax` — inclusive repitition specification.
    ///      use nmax < 0 for unbounded
    ///   * `pattern` — to fold captures of
    ///   * `folder` — functions to merge new captures
    /// returns new pattern that will produce final value of folded accumulator
    /// as capture result

    pub fn foldRep(
        comptime nmin: comptime_int,
        comptime nmax: comptime_int,
        comptime pattern: anytype,
        comptime folder: Folder,
    ) type {
        comptime const P = pat(pattern);
        comptime const init_info = @typeInfo(@TypeOf(folder.init));
        comptime const deinit_info = @typeInfo(@TypeOf(folder.deinit));
        comptime const fold_info = @typeInfo(@TypeOf(folder.fold)).Fn;
        comptime const acc_info = @typeInfo(fold_info.args[0].arg_type.?);
        comptime const A = acc_info.Pointer.child;
        comptime const T = StripError(A);
        comptime var E = MatchError(P);
        // FIXME more unresolved return type hacks
        E = E || ErrorOf(fold_info.return_type.?);
        if (init_info == .Fn)
            E = E || ErrorOf(init_info.Fn.return_type.?);

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const saved = p.save();
                const aerr =
                    if (comptime (init_info == .Fn)) folder.init(p)
                    else folder.init;
                var acc: T = if (comptime canError(@TypeOf(aerr))) try aerr else aerr;
                errdefer
                    if (comptime(deinit_info != .Fn)) {}
                    else folder.deinit(p, &acc);

                var m: usize = 0;
                while (comptime (nmax < 0) or m < nmax) {
                    const perr = P.parse(p);
                    const opt =
                        if (comptime canError(@TypeOf(perr))) try perr else perr;
                    if (opt) |c| {
                        const ferr = folder.fold(&acc, c);
                        if (comptime canError(@TypeOf(ferr))) try ferr;
                    } else
                        break;
                    m += 1;
                }

                if (m < nmin) {
                    if (comptime (deinit_info == .Fn))
                        folder.deinit(p, &acc);
                    p.restore(saved);
                    return null;
                }
                return acc;
            }
        };
    }

    fn foldVoid(acc: *void, c: void) void { }

    fn optionFolder(comptime T: type) Folder {
        const F = struct {
            fn fold(acc: *?T, c: T) void {
                std.debug.assert(acc.* == null);
                acc.* = c;
            }
        };

        return .{
            .init = @as(?T, null),
            .fold = F.fold,
        };
    }

    fn arrayFolder(comptime T: type) Folder {
        const List = std.ArrayList(T);

        const F = struct {
            fn init(p: *Parser) List { return List.init(p.alloc); }

            fn deinit(p: *Parser, acc: *List) void { acc.deinit(); }

            fn fold(acc: *List, c: T) !void { try acc.append(c); }
        };

        return .{
            .init = F.init,
            .deinit = F.deinit,
            .fold = F.fold,
        };
    }

    /// matches pattern repeatedly
    ///   * `nmin` to `nmax` — inclusive repitition specification.
    ///      use nmax < 0 for unbounded
    ///   * captures nothing if pattern does not capture, otherwise
    ///   * captures optional if nmin < nmax == 1, otherwise
    ///   * allocates and captures slice for variable capture count

    pub fn rep(
        comptime nmin: comptime_int,
        comptime nmax: comptime_int,
        comptime pattern: anytype,
    ) type {
        comptime const P = pat(pattern);
        comptime const M = MatchType(P);

        if (comptime (M == void))
            return foldRep(nmin, nmax, P, .{ .fold = foldVoid });

        return if (comptime (nmin < nmax and nmax == 1))
            foldRep(nmin, nmax, P, optionFolder(M))
        else
            foldRep(nmin, nmax, P, arrayFolder(M));
    }

    /// match concatenation of 2 patterns (special case of .seq() with 2 args).
    /// prefer .seq() for multiple concatenations.
    pub fn cat(comptime pat0: anytype, comptime pat1: anytype) type {
        return seq(.{ pat0, pat1 });
    }

    /// match concatenation of all patterns in `args` tuple
    pub fn seq(comptime args: anytype) type {
        if (@typeInfo(@TypeOf(args)) != .Struct)
            @compileError("Expected tuple of patterns, found '"
                              ++ @typeName(@TypeOf(args)) ++ "'");
        switch (args.len) {
            0 => return pat(true),
            1 => return pat(args[0]),
            else => {}
        }

        // filter match results w/captures
        comptime var E: type = error{};
        comptime var Pats: [args.len]type = undefined;
        comptime var Caps: [args.len]type = undefined;
        comptime var ncaps = 0;
        inline for (args) |arg, i| {
            // within sequence, nested tuple alternates to alt
            Pats[i] = if (comptime @typeInfo(@TypeOf(arg)) == .Struct)
                alt(arg)
            else
                pat(arg);

            E = E || MatchError(Pats[i]);
            comptime const M = MatchType(Pats[i]);
            if (comptime (M != void)) {
                Caps[ncaps] = M;
                ncaps += 1;
            }
        }

        // reduce or gen tuple for mutiple results
        comptime const T = switch (ncaps) {
            0 => void,
            1 => Caps[0],
            else => meta.Tuple(Caps[0..ncaps])
        };

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                var caps: T = undefined;
                const saved = p.save();
                comptime var j = 0;

                inline for (Pats) |P, i| {
                    const err = P.parse(p);
                    const opt = if (comptime canError(@TypeOf(err))) try err else err;
                    if (opt == null) {
                        p.restore(saved);
                        return null;
                    }

                    if (comptime (MatchType(P) != void))
                        switch (ncaps) {
                            0 => unreachable,
                            1 => caps = opt.?,
                            else => {
                                comptime var buf: [128]u8 = undefined;
                                comptime const nm =
                                    std.fmt.bufPrint(&buf, "{d}", .{j})
                                    catch unreachable;
                                @field(caps, nm) = opt.?;
                                j += 1;
                            }
                        };
                }
                return caps;
            }
        };
    }

    /// match *first* of ordered list of choices in `args` tuple
    pub fn alt(comptime args: anytype) type {
        if (@typeInfo(@TypeOf(args)) != .Struct)
            @compileError("Expected tuple of patterns, found '"
                              ++ @typeName(@TypeOf(args)) ++ "'");
        switch (args.len) {
            0 => return pat(true),
            1 => return pat(args[0]),
            else => {}
        }

        comptime var Pats: [args.len]type = undefined;
        Pats[0] = pat(args[0]);
        comptime var E = MatchError(Pats[0]);
        comptime const T = MatchType(Pats[0]);

        inline for (args) |arg, i| {
            if (i > 0) {
                Pats[i] = pat(arg);
                E = E || MatchError(Pats[i]);
                if(MatchType(Pats[i]) != T) {
                    @compileLog(0, T);
                    @compileLog(i, MatchType(Pats[i]));
                    @compileError(
                        "Heterogeneous choice captures unsupported" ++
                            " (try capturing each choice to an enum)");
                }
            }
        }

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const saved = p.save();
                inline for (Pats) |P| {
                    const err = P.parse(p);
                    const opt = if (comptime canError(@TypeOf(err))) try err else err;
                    if (opt) |c|
                        return c;
                    p.restore(saved);
                }
                return null;
            }
        };
    }

    // captures

    /// capture offset from start of matched string to end of pattern.
    /// always matches without consuming input.
    pub fn pos() type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) ?usize {
                return p.pos();
            }
        };
    }

    /// capture string matched by pattern as slice.  if pattern already
    /// captures, slice and capture will be wrapped in new tuple.
    /// returned slice is only valid until parse buffer is updated.
    pub fn cap(comptime pattern: anytype) type {
        comptime const P = pat(pattern);
        comptime const E = MatchError(P);
        comptime const M = MatchType(P);
        comptime const T = if (M == void)
            []const u8
        else
            meta.Tuple(.{ []const u8, M });

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const start = p.save();
                const err = P.parse(p);
                const opt = if (comptime canError(@TypeOf(err))) try err else err;
                if (opt) |sub| {
                    const c = p.buf[start.idx..p.pos()];
                    if (comptime (M == void)) {
                        return c;
                    } else
                        return .{ c, sub };
                }
                return null;
            }
        };
    }

    // FIXME can't make TypeInfo.Fn.return_type resolve reliably.  so, for now,
    // need to specify *full* (including any error) return type of `f` as R

    /// transform result of matching pattern using another function.
    /// if pattern has no captures, matched string will be passed instead.
    /// function that returns optional null will fail to match.
    pub fn grok(
        comptime R: type,
        comptime f: anytype,
        comptime pattern: anytype
    ) type {
        comptime const P = pat(pattern);
        comptime const M = MatchType(P);
        comptime const E = MatchError(P) || ErrorOf(R);
        comptime const T = StripOption(StripError(R));

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const start = if (comptime (M != void)) {} else p.save();
                const perr = P.parse(p);
                const opt = if (comptime canError(@TypeOf(perr))) try perr else perr;
                if (opt) |sub| {
                    const ferr =
                        if (comptime (M != void)) f(sub)
                        else if (comptime (@typeInfo(@TypeOf(f)).Fn.args.len == 0)) f()
                        else f(p.buf[start.idx .. p.pos()]);
                    const v = if (comptime canError(@TypeOf(ferr))) try ferr else ferr;
                    return v;
                }
                return null;
            }
        };
    }
};


/// generate adapters for using builder pattern with Patterns...
fn PatternBuilder(comptime Self_: type) type {
    const P = Pattern;

    return struct {
        const Self = Self_;

        /// concatenate another pattern after this.  see Pattern.cat().
        /// prefer Pattern.seq() for multiple concatenations.
        pub fn _(comptime pattern: anytype) type {
            return P.cat(Self, pattern);
        }

        /// concatenate character set after this.  see Pattern.set().
        /// prefer Pattern.seq() for multiple concatenations.
        pub fn set(comptime chars: []const u8) type {
            return P.cat(Self, P.set(chars));
        }

        /// concatenate character span after this.  see Pattern.span().
        /// prefer Pattern.seq() for multiple concatenations.
        pub fn span(comptime lo: u8, comptime hi: u8) type {
            return P.cat(Self, P.span(lo, hi));
        }

        /// match only if this pattern does not match.
        /// prefer Pattern.not() for clarity.
        pub fn not() type {
            return P.not(Self);
        }

        /// match this pattern without consuming input.
        /// prefer Pattern.if_() for clarity.
        pub fn if_() type {
            return P.if_(Self);
        }

        pub fn foldRep(
            comptime nmin: comptime_int,
            comptime nmax: comptime_int,
            comptime folder: P.Folder
        ) type {
            return P.foldRep(nmin, nmax, Self, folder);
        }

        /// match this pattern repeatedly.
        /// prefer this to using Pattern.rep() directly.
        pub fn rep(comptime nmin: comptime_int, comptime nmax: comptime_int) type {
            return P.rep(nmin, nmax, Self);
        }

        /// try to match another choice if this does not match.
        /// prefer Pattern.alt() for > 2 choices.
        pub fn or_(comptime pattern: anytype) type {
            return P.alt(Self, pattern);
        }

        /// capture position after matching this pattern.  see Pattern.pos()
        pub fn pos() type {
            return P.cat(Self, P.pos());
        }

        /// capture this pattern as string slice.  see Pattern.cap()
        pub fn cap() type {
            return P.cap(Self);
        }

        /// transform string matched by this pattern.  see Pattern.grok()
        pub fn grok(comptime R: type, comptime f: anytype) type {
            return P.grok(R, f, Self);
        }

        /// match this pattern against provided string, returning any captures.
        pub fn match(str: []const u8, alloc: *Allocator)
            MatchReturn(MatchError(Self), MatchType(Self))
        {
            var p = Parser.init(str, alloc);
            return p.match(Self);
        }

        /// match this pattern against provided string, returning any captures.
        /// Allocations are disallowed; any attempt to allocate will panic.
        pub fn matchLean(str: []const u8)
            MatchReturn(MatchError(Self), MatchType(Self))
        {
            var p = Parser.init(str, &noalloc);
            return p.match(Self);
        }
    };
}


// patterns may optionally produce an error and can always fail to match.
// match result is superseded by any error and these need to be swizzled
// during result composition.

/// introspect capture result type from Pattern, *without* error or optional
pub fn MatchType(comptime P: type) type {
    comptime const R = StripError(@typeInfo(@TypeOf(P.parse)).Fn.return_type.?);
    return comptime switch (@typeInfo(R)) {
        .Optional => |opt| opt.child,
        else => @compileError(
            "Expected Optional capture type, not " ++ @typeName(R))
    };
}

/// introspect capture error set from Pattern or default to empty error set
fn MatchError(comptime P: type) type {
    return comptime ErrorOf(@typeInfo(@TypeOf(P.parse)).Fn.return_type.?);
}

/// rewrap new capture result in option and optional error
fn MatchReturn(comptime E: type, comptime T: type) type {
    return comptime if (isError(E)) E!?T else ?T;
}


/// extract payload from outer error union or return type directly
fn StripError(comptime T: type) type {
    return comptime switch (@typeInfo(T)) {
        .ErrorUnion => |eu| eu.payload,
        .ErrorSet => void,
        else => T
    };
}

/// extract payload from optional or return type directly
fn StripOption(comptime T: type) type {
    return comptime switch (@typeInfo(T)) {
        .Optional => |opt| opt.child,
        else => T,
    };
}

/// extract error set from outer error union or return empty error set
fn ErrorOf(comptime T: type) type {
    return comptime switch (@typeInfo(T)) {
        .ErrorUnion => |eu| eu.error_set,
        .ErrorSet => T,
        else => error{}
    };
}

/// detect empty error sets
fn isError(comptime Error: type) bool {
    return comptime switch (@typeInfo(Error)) {
        .ErrorUnion => |eu| isError(eu.error_set),
        .ErrorSet => |errset|
            if (errset) |errs|
                errs.len > 0
            else false,
        else => false
    };
}

fn canError(comptime T: type) bool {
    return comptime (@typeInfo(T) == .ErrorUnion);
}


pub const Parser = struct {
    buf: []const u8,
    alloc: *Allocator,
    state: State,

    const State = struct {
        idx: usize = 0,
    };

    pub fn init(str: []const u8, allocator: *Allocator) @This() {
        return .{
            .buf = str,
            .alloc = allocator,
            .state = .{},
        };
    }

    /// match current string against provided pattern
    pub fn match(self: *@This(), comptime pattern: anytype) blk:{
        comptime const P = Pattern.pat(pattern);
        @setEvalBranchQuota(1<<20);
        break :blk MatchReturn(MatchError(P), MatchType(P));
    } {
        return Pattern.pat(pattern).parse(self);
    }

    pub fn pos(self: *@This()) usize {
        return self.state.idx;
    }

    pub fn get(p: *@This(), n: usize) ?[]const u8 {
        if (p.state.idx + n <= p.buf.len)
            return p.buf[p.state.idx..p.state.idx+n];
        return null;
    }

    fn take(p: *@This(), n: usize) void {
        p.state.idx += n;
        std.debug.assert(p.state.idx <= p.buf.len);
    }

    fn save(self: *@This()) State {
        return self.state;
    }

    fn restore(self: *@This(), prev: State) void {
        self.state = prev;
    }
};


var noalloc = Allocator {
    .allocFn = noAlloc,
    .resizeFn = noResize,
};

fn noAlloc(a: *Allocator, b: usize, c: u29, d: u29, e: usize) ![]u8 {
    std.debug.panic("Unsanctioned allocation during pattern match", .{});
}

fn noResize(a: *Allocator, b: []u8, c: u29, d: usize, e: u29, f: usize) !usize {
    unreachable;
}


//----------------------------------------------------------------------------
const testing = std.testing;
const expectStr = testing.expectEqualStrings;
const panic = std.debug.panic;

pub fn expectOk(ev: anytype) StripError(@TypeOf(ev)) {
    if (comptime !isError(@TypeOf(ev))) {
        return ev;
    } else if(ev) |v| {
        return v;
    } else |e|
        panic("unexpected error.{}", .{ @errorName(e) });
}

// FIXME hacking around testing.expectEqual can't compare strings?!
// FIXME should fix annoying expected type things too...
pub fn expectEqual(exp: anytype, act: @TypeOf(exp)) void {
    switch (@typeInfo(@TypeOf(act))) {
        .Undefined, .Void, .Null => return,

        .Bool, .Int, .Float, .ComptimeFloat, .ComptimeInt,
        .EnumLiteral, .Enum, .Fn, .ErrorSet, .Type =>
            if (act != exp)
                panic("expected {}, found {}", .{ exp, act }),

        .Pointer => |ptr| {
            switch (ptr.size) {
                .One, .Many, .C => if (act != exp)
                    panic("expected {*}, found {*}", .{ exp, act }),
                .Slice => {
                    if (ptr.child == u8) {
                        return expectStr(exp, act);
                    } else
                        return testing.expectEqualSlices(ptr.child, exp, act);
                },
            }
        },

        .Array => |ary| expectEqualSlices(ary.child, &exp, &act),

        .Struct => |s| {
            inline for (s.fields) |f|
                expectEqual(@field(exp, f.name), @field(act, f.name));
        },

        .Union => |u| {
            if (u.tag_type == null)
                @compileError("Unable to compare untagged union values");

            const Tag = @TagType(@TypeOf(exp));
            expectEqual(@as(Tag, exp), @as(Tag, act));

            inline for (std.meta.fields(@TypeOf(act))) |f| {
                if (std.mem.eql(u8, f.name, @tagName(@as(Tag, act)))) {
                    expectEqual(@field(exp, f.name), @field(act, f.name));
                    return;
                }
            }
            unreachable;
        },

        .Optional => {
            if (exp) |expval| {
                if (act) |actval| {
                    expectEqual(expval, actval);
                } else
                    panic("expected {}, found null", .{ expval });
            } else if (act) |actval|
                panic("expected null, found {}", .{ actval });
        },

        else => std.debug.panic("unsupported type: {s}", act)
    }
}

pub fn chkNoM(comptime pat: anytype, str: []const u8) void {
    var p = Parser.init(str, testing.failing_allocator);
    const m = expectOk(p.match(pat));
    expectEqual(@as(@TypeOf(m), null), m);
    expectEqual(@as(usize, 0), p.pos());
}

pub fn chkMatch(comptime pat: anytype, str: []const u8, n: usize) void {
    var p = Parser.init(str, testing.failing_allocator);
    const m = expectOk(p.match(pat));
    expectEqual(@as(?void, {}), m);
    expectEqual(n, p.pos());
}

pub fn chkCap(comptime pat: anytype, str: []const u8, n: usize)
    MatchType(Pattern.pat(pat))
{
    var p = Parser.init(str, testing.allocator);
    const m = expectOk(p.match(pat));
    testing.expect(m != null);
    expectEqual(n, p.pos());
    return m.?;
}

fn injectError(p: *Parser) !?void {
    return error.Injected;
}

fn chkError(comptime pat: anytype, str: []const u8, err: anyerror) void {
    var p = Parser.init(str, testing.allocator);
    testing.expectError(err, p.match(pat));
}

fn parseU32Dec(s: []const u8) !u32 {
    return std.fmt.parseInt(u32, s, 10);
}


test "Pattern.str" {
    const str = Pattern.str;
    chkMatch(str(""), "", 0);
    chkMatch(str(""), "abc", 0);

    chkNoM(str("a"), "b");
    chkNoM(str("abc"), "ab");

    chkMatch(str("a"), "a", 1);
    chkMatch(str("ab"), "abc", 2);
}

test "Pattern.any" {
    const any = Pattern.any;
    chkMatch(any(0), "", 0);

    chkNoM(any(1), "");
    chkMatch(any(1), "a", 1);
    chkMatch(any(1), "b", 1);
    chkMatch(any(1), "abc", 1);

    chkMatch(any(2), "xyz", 2);
}

test "Pattern.set" {
    const set = Pattern.set;

    chkNoM(set(""), "");
    chkNoM(set(""), "a");

    chkMatch(set("a"), "ab", 1);
    chkNoM(set("a"), "b");

    const vowel = set("aeiou");
    chkNoM(vowel, "");
    chkNoM(vowel, "fail");
    chkMatch(vowel, "abcde", 1);
    chkMatch(vowel, "ignore", 1);
    chkMatch(vowel, "oauie", 1);
    chkMatch(vowel, "under", 1);
}

test "Pattern.cls" {
    const cls = Pattern.cls;

    const G = struct {
        fn even(c: u8) bool { return c&1 == 0; }
    };

    chkNoM(cls(G.even).rep(1, -1), "\x01\x03\x05\x09\x11\x21\x41\x81\xff");
    chkMatch(cls(G.even).rep(1, -1), "\x00\x02\x04\x08\x10\x20\x40\x80\xfe", 9);
}

test "Pattern.span" {
    const dig = Pattern.span('0', '9');
    chkNoM(dig, "");
    chkNoM(dig, "fail");
    chkMatch(dig, "0abc", 1);
    chkMatch(dig, "9876", 1);
    chkMatch(dig, "42", 1);
}

test "Pattern.not" {
    const not = Pattern.not;

    chkNoM(not(0), "");
    chkNoM(not(0), "a");

    chkNoM(not(1), "a");
    chkMatch(not(1), "", 0);

    chkNoM(not("a"), "a");
    chkNoM(not("a"), "ab");
    chkMatch(not("a"), "", 0);
    chkMatch(not("a"), "b", 0);
    chkMatch(not("a"), "ba", 0);

    chkNoM(not("ab"), "ab");
    chkNoM(not("ab"), "abc");
    chkMatch(not("ab"), "", 0);
    chkMatch(not("ab"), "a", 0);
    chkMatch(not("ab"), "ba", 0);

    chkError(not(injectError), "", error.Injected);
}

test "Pattern.if_" {
    const if_ = Pattern.if_;

    chkMatch(if_(0), "", 0);
    chkMatch(if_(0), "a", 0);

    chkNoM(if_("a"), "b");
    chkMatch(if_("a"), "a", 0);

    chkError(if_(injectError), "", error.Injected);
}

test "Pattern.if_ capture" {
    const G = struct {
        usingnamespace Pattern;
        const p = seq(.{ "abc", if_(cap("123")) });
    };
    chkNoM(G.p, "abc12xyz");
    expectStr("123", chkCap(G.p, "abc123xyz", 3));
}

test "Pattern.pat" {
    const pat = Pattern.pat;

    // pass thru
    const p = pat(3);
    expectEqual(p, pat(p));
    chkNoM(pat(p), "");
    chkNoM(pat(p), "12");
    chkMatch(pat(p), "123", 3);
    chkMatch(pat(p), "12345", 3);

    // str
    chkNoM(pat("a"), "b");
    chkMatch(pat("a"), "a", 1);
    chkMatch(pat("ab"), "abc", 2);
    chkNoM(pat("abc"), "ab");

    // any(n) n >= 0
    chkMatch(pat(0), "", 0);
    chkNoM(pat(1), "");
    chkMatch(pat(1), "a", 1);
    chkMatch(pat(1), "b", 1);
    chkMatch(pat(1), "abc", 1);
    chkMatch(pat(2), "xyz", 2);

    // any(n) n < 0
    chkMatch(pat(-1), "", 0);
    chkNoM(pat(-1), "a");
    chkMatch(pat(-2), "", 0);
    chkMatch(pat(-2), "a", 0);
    chkNoM(pat(-2), "ab");

    // bool
    chkMatch(pat(true), "", 0);
    chkMatch(pat(true), "abc", 0);
    chkNoM(pat(false), "");
    chkNoM(pat(false), "abc");

    // seq
    const s = pat(.{"a", 1, "c"});
    chkNoM(s, "");
    chkNoM(s, "ab");
    chkNoM(s, "abb");
    chkMatch(s, "abcd", 3);
    chkMatch(s, "aacc", 3);
    chkMatch(s, "acca", 3);
}

test "except" {
    const p = Pattern.seq(.{ Pattern.if_("abc"), "ab" });
    chkNoM(p, "ab");
    chkNoM(p, "aba");
    chkMatch(p, "abc", 2);
    chkMatch(p, "abcde", 2);
}

test "Pattern.rep" {
    const rep = Pattern.rep;

    const a0 = rep(0, -1, "a");
    chkMatch(a0, "", 0);
    chkMatch(a0, "a", 1);
    chkMatch(a0, "aa", 2);
    chkMatch(a0, "aabaa", 2);
    chkMatch(a0, "aaaaaaaaaa", 10);
    chkMatch(a0, "aaaaaaaaaab", 10);

    const a1 = rep(1, -1, "a");
    chkNoM(a1, "");
    chkNoM(a1, "b");
    chkMatch(a1, "a", 1);
    chkMatch(a1, "aaa", 3);
    chkMatch(a1, "aaaaaaiit", 6);

    const ab2 = rep(2, -1, "ab");
    chkNoM(ab2, "");
    chkNoM(ab2, "ab");
    chkMatch(ab2, "abab", 4);
    chkMatch(ab2, "ababab", 6);

    const ab_1 = rep(0, 1, Pattern.span('a', 'b'));
    chkMatch(ab_1, "", 0);
    chkMatch(ab_1, "", 0);
    chkMatch(ab_1, "a", 1);
    chkMatch(ab_1, "b", 1);
    chkMatch(ab_1, "ab", 1);

    const p2_2 = rep(0, 2, 2);
    chkMatch(p2_2, "", 0);
    chkMatch(p2_2, "a", 0);
    chkMatch(p2_2, "aa", 2);
    chkMatch(p2_2, "aaa", 2);
    chkMatch(p2_2, "bbbbb", 4);
    chkMatch(p2_2, "bbbbbbbbbbb", 4);

    chkError(rep(3, -1, injectError), "", error.Injected);
}

test "Pattern.rep capture" {
    const rep = Pattern.rep;
    const cap = Pattern.cap;
    const span = Pattern.span;

    const a_1 = rep(0, 1, cap("a"));
    expectEqual(@as(?[]const u8, null), chkCap(a_1, "", 0));
    expectEqual(@as(?[]const u8, null), chkCap(a_1, "b", 0));
    expectEqual(@as(?[]const u8, "a"), chkCap(a_1, "a", 1));
    expectEqual(@as(?[]const u8, "a"), chkCap(a_1, "aaa", 1));

    const nums = rep(1, -1, .{
        rep(1, -1, .{ rep(0, 1, "-"), span('0', '9') })
            .grok(error{Overflow,InvalidCharacter}!u32, parseU32Dec),
        rep(0, 1, ",")
    });
    chkNoM(nums, "");
    chkNoM(nums, ",");
    {
        var act = chkCap(nums, "42", 2);
        testing.expectEqualSlices(u32, &[_]u32{ 42 }, act.items);
        act.deinit();
    }
    {
        var act = chkCap(nums, "123,456,789 ", 11);
        testing.expectEqualSlices(u32, &[_]u32{ 123, 456, 789 }, act.items);
        act.deinit();
    }

    chkError(nums, "123,-123,456", error.Overflow);
}

test "Pattern.seq" {
    const seq = Pattern.seq;
    {
        const p = seq(.{ "abc", "xyz" });
        chkNoM(p, "");
        chkNoM(p, "abc");
        chkNoM(p, "xyz");
        chkNoM(p, "xyzabc");
        chkMatch(p, "abcxyz123", 6);
    }
    {
        const p = seq(.{ "12", "34", "56" });
        chkNoM(p, "123467");
        chkMatch(p, "123456", 6);
    }
    {
        const p = seq(.{ 1, "b", -1 });  // ".b$"
        chkNoM(p, "abc");
        chkMatch(p, "ab", 2);
        chkNoM(p, "ba");
        chkMatch(p, "zb", 2);
    }

    chkError(seq(.{ "abc", injectError, "xyz" }), "abc123xyz", error.Injected);
}

test "Pattern.seq capture" {
    const seq = Pattern.seq;
    const cap = Pattern.cap;
    {
        const p = seq(.{ "abc", cap("123"), "xyz" });
        expectStr("123", chkCap(p, "abc123xyz ", 9));
    }
    {
        const p = seq(.{ cap("abc"), ",", cap("123"), ",", cap("xyz") });
        const act = chkCap(p, "abc,123,xyz ", 11);
        expectStr("abc", act[0]);
        expectStr("123", act[1]);
        expectStr("xyz", act[2]);
    }
    {
        const p = seq(.{ cap("abc"), ",", cap("123"), ",", injectError });
        chkError(p, "abc,123,xyz", error.Injected);
    }
}

test "Pattern.alt" {
    const alt = Pattern.alt;
    {
        const p = alt(.{ "abc", "xyz" });
        chkNoM(p, "");
        chkNoM(p, "azbycz");
        chkMatch(p, "abc", 3);
        chkMatch(p, "xyz", 3);
        chkMatch(p, "abcxyz", 3);
        chkMatch(p, "abcabc", 3);
    }
}

test "Pattern.pos" {
    const pat = Pattern.pat;
    const pos = Pattern.pos;
    expectEqual(@as(usize, 0), chkCap(pat(0).pos(), "", 0));
    expectEqual(@as(usize, 2), chkCap(pat(.{
        pat("a").rep(0, -1).pos(),
        pat("b").rep(0, -1)
    }), "aabb ", 4));
}

test "Pattern.cap" {
    const G = struct {
        usingnamespace Pattern;

        // [a-c]*([x-z]+)[a-c]*$
        const p = pat(.{
            span('a', 'c').rep(0, -1),
            span('x', 'z').rep(1, -1).cap(),
            span('a', 'c').rep(0, -1),
        });
    };

    chkNoM(G.p, "");
    chkNoM(G.p, "aabb");
    expectStr("x", chkCap(G.p, "x", 1));
    expectStr("xyz", chkCap(G.p, "abcxyzabc", 9));
    expectStr("yy", chkCap(G.p, "aayyccxyz", 6));
}

test "Pattern.grok" {
    const G = struct {
        usingnamespace Pattern;

        const p = pat(.{
            span('a', 'z').rep(1, -1),
            "=",
            rep(0, 1, "-").span('0', '9').rep(1, -1)
                .grok(error{Overflow,InvalidCharacter}!u32, parseU32Dec),
        });
    };

    chkNoM(G.p, "");
    chkNoM(G.p, "=");
    expectEqual(@as(u32, 1), chkCap(G.p, "a=1 ", 3));
    expectEqual(@as(u32, 123), chkCap(G.p, "abc=123xyz", 7));

    chkError(G.p, "xyz=-42", error.Overflow);
}

test "Pattern.grok noargs" {
    const G = struct {
        usingnamespace Pattern;

        fn noargs() bool {
            return true;
        }
        const p = grok(bool, noargs, any(1));
    };

    chkNoM(G.p, "");
    expectEqual(true, chkCap(G.p, ".", 1));
}

test "blind greedy" {
    const G = struct {
        usingnamespace Pattern;
        const p = pat(.{ span('a', 'z').rep(0, -1), "1" });
    };
    chkMatch(G.p, "count123", 6);
    chkNoM(G.p, "count2");
}

test "non-blind greedy" {
    const G = struct {
        usingnamespace Pattern;

        const lastdig = alt(.{
            .{ 1, lastdigRef },
            span('0', '9')
        });
        const lastdigRef = ref(@This(), "lastdig", void);
    };

    chkNoM(G.lastdig, "abcxyz");
    chkMatch(G.lastdig, "abc123xyz", 6);
}

test "non-blind non-greedy c comment grammar" {
    const G = struct {
        usingnamespace Pattern;

        const comment = cat("/*", close);
        const close = alt(.{ "*/", .{ 1, closeRef }, });
        const closeRef = ref(@This(), "close", void);
    };

    chkNoM(G.comment, "");
    chkNoM(G.comment, "/* junk");
    chkNoM(G.comment, "/*/");

    chkMatch(G.close, "*/abc", 2);
    chkMatch(G.close, "aa*/bb", 4);
    chkMatch(G.comment, "/**/", 4);
    chkMatch(G.comment, "/*aa*/bb", 6);
}

test "c comment, negative lookahead" {
    const seq = Pattern.seq;
    const not = Pattern.not;
    const com = seq(.{ "/*", (not("*/")._(1)).rep(0, -1), "*/" });

    chkNoM(com, "");
    chkNoM(com, "a");
    chkNoM(com, "/*");
    chkNoM(com, "/*/");
    chkNoM(com, "*/");

    chkMatch(com, "/**/", 4);
    chkMatch(com, "/* */*/", 5);
}

test "predication" {
    const G = struct {
        usingnamespace Pattern;

        // !((’int’ / ’float’) ![a-z]) [a-z]+
        const p = pat(.{
            not(.{
                .{ "int", "float" },
                not(span('a', 'z'))
            }),
            span('a', 'z').rep(1, -1),
        });
    };

    chkNoM(G.p, "int");
    chkNoM(G.p, "float");
    chkNoM(G.p, "int64");
    chkNoM(G.p, "float32");
    chkMatch(G.p, "in", 2);
    chkMatch(G.p, "floaty", 6);
    chkMatch(G.p, "intu ", 4);
    chkMatch(G.p, "uint123", 4);
}

// FIXME how to detect grammars that are not well formed (left recursive)?
// (currently they just crash (probably stack overflow?))

test "even 0s even 1s" {
    const G = struct {
        usingnamespace Pattern;

        // EE <- ’0’ OE / ’1’ EO / !.
        // OE <- ’0’ EE / ’1’ OO
        // EO <- ’0’ OO / ’1’ EE
        // OO <- ’0’ EO / ’1’ OE

        // add enough manual refs to break cycles
        const eeRef = ref(@This(), "ee", void);
        const ooRef = ref(@This(), "oo", void);

        const ee = alt(.{ .{ "0", oe }, .{ "1", eo }, -1 });
        const oe = alt(.{ .{ "0", eeRef }, .{ "1", ooRef } });
        const eo = alt(.{ .{ "0", ooRef }, .{ "1", eeRef } });
        const oo = alt(.{ .{ "0", eo }, .{ "1", oe } });
    };

    chkMatch(G.ee, "", 0);
    chkNoM(G.ee, "0");
    chkNoM(G.ee, "1");
    chkNoM(G.ee, "01");
    chkNoM(G.ee, "10");
    chkMatch(G.ee, "00", 2);
    chkMatch(G.ee, "11", 2);
    chkNoM(G.ee, "11z");
    chkMatch(G.ee, "0101101101", 10);
    chkNoM(G.ee, "0101100101");
}
