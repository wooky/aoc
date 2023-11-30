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

    const lib = b.addSharedLibrary(.{
        .name = "aoc_zig",
        .root_source_file = .{ .path = "src/main/zig/runner.zig" },
        .target = target,
        .optimize = optimize,
    });
    lib.addAnonymousModule("aoc_nim", .{ .source_file = .{ .path = "build/aoc_nim.zig" } });
    lib.linkLibC();
    lib.addLibraryPath(.{ .path = "build/bindings.nim" });
    lib.addLibraryPath(.{ .path = "build/ada/lib" });
    lib.linkSystemLibrary("aoc_nim");
    lib.linkSystemLibrary("aoc_ada");
    lib.linkSystemLibrary("gsl");

    const install = b.addInstallArtifact(lib, .{ .dest_dir = .{ .override = .{ .custom = "../build" } } });
    b.getInstallStep().dependOn(&install.step);
}
