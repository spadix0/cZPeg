const std = @import("std");
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const testing = std.testing;
const expectStr = testing.expectEqualStrings;
const czpeg = @import("../czpeg.zig");


pub var noalloc = Allocator {
    .allocFn = noAlloc,
    .resizeFn = noResize,
};

fn noAlloc(a: *Allocator, b: usize, c: u29, d: u29, e: usize) ![]u8 {
    panic("Unsanctioned allocation during pattern match", .{});
}

fn noResize(a: *Allocator, b: []u8, c: u29, d: usize, e: u29, f: usize) !usize {
    unreachable;
}


pub fn chkNoM(comptime pat: anytype, str: []const u8) void {
    var p = czpeg.Parser.init(str, testing.failing_allocator);
    const m = expectOk(p.match(pat));
    expectEqual(@as(@TypeOf(m), null), m);
    expectEqual(@as(usize, 0), p.pos());
}

pub fn chkMatch(comptime pat: anytype, str: []const u8, n: usize) void {
    var p = czpeg.Parser.init(str, testing.failing_allocator);
    const m = expectOk(p.match(pat));
    expectEqual(@as(?void, {}), m);
    expectEqual(n, p.pos());
}

pub fn chkCap(comptime pat: anytype, str: []const u8, n: usize)
    czpeg.MatchType(czpeg.Pattern.pat(pat))
{
    var p = czpeg.Parser.init(str, testing.allocator);
    const m = expectOk(p.match(pat));
    testing.expect(m != null);
    expectEqual(n, p.pos());
    return m.?;
}

pub fn chkError(comptime pat: anytype, str: []const u8, err: anyerror) void {
    var p = czpeg.Parser.init(str, testing.allocator);
    testing.expectError(err, p.match(pat));
}


pub fn expectOk(ev: anytype) switch (@typeInfo(@TypeOf(ev))) {
    .ErrorUnion => |eu| eu.payload,
    else => @TypeOf(ev)
} {
    if (comptime @typeInfo(@TypeOf(ev)) != .ErrorUnion) {
        return ev;
    } else if (ev) |v| {
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

        else => panic("unsupported type: {s}", act)
    }
}
