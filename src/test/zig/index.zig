const std = @import("std");

pub fn makeTests(b: *std.build.Builder) *std.build.Step {
    var test_step = b.step("test", "Run tests");
    const test_paths = [_][]const u8 {
        "src/test/zig/test2015.zig"
    };

    for (test_paths) |tp| {
        const t = b.addTest(tp);
        t.addPackagePath("aoc", "src/main/zig/aoc.zig");
        t.addPackagePath("runner", "src/main/zig/runner.zig");
        test_step.dependOn(&t.step);
    }

    return test_step;
}
