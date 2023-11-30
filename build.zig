const Builder = @import("std").build.Builder;
const warn = @import("std").debug.warn;

pub fn build(b: *Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "aoc",
        .root_source_file = .{ .path = "src/main/zig/runner.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addAnonymousModule("aoc_nim", .{ .source_file = .{ .path = "build/aoc_nim.zig" } });
    exe.linkLibC();
    exe.addLibraryPath(.{ .path = "build/bindings.nim" });
    exe.addLibraryPath(.{ .path = "build/ada/lib" });
    exe.linkSystemLibrary("aoc_nim");
    exe.linkSystemLibrary("aoc_ada");
    exe.linkSystemLibrary("gsl");
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var test_step = @import("src/test/zig/index.zig").makeTests(b);
    test_step.dependOn(&exe.step);
}
