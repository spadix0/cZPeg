const std = @import("std");

var tests: *std.build.Step = undefined;

pub fn build(b: *std.build.Builder) void {
    tests = b.step("test", "Run tests");
    addTest(b, "src/czpeg.zig");
    addTest(b, "src/czpeg/re.zig");

    b.installDirectory(.{
        .source_dir = "src",
        .install_dir = .Lib,
        .install_subdir = "zig",
    });
}

fn addTest(b: anytype, src: []const u8) void {
    const t = b.addTest(src);
    t.setBuildMode(b.standardReleaseOptions());
    tests.dependOn(&t.step);
}
