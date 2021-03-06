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
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("aoc", "src/main/zig/runner.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();
    exe.linkLibC();
    exe.addIncludeDir("build/bin/linuxX64/releaseShared");
    exe.addLibPath("build/bin/linuxX64/releaseShared");
    exe.linkSystemLibrary("aoc");

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var test_step = @import("src/test/zig/index.zig").makeTests(b);
    test_step.dependOn(&exe.step);
}
