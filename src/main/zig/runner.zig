const aoc = @import("aoc.zig");
const std = @import("std");

pub const io_mode = .evented;

const ExportSolution = extern struct {
    s1: [*:0]const u8,
    s2: [*:0]const u8,
};

fn run_zig(problem: *aoc.Problem, year: u16, day: u16) !ExportSolution {
    var zig_solution = try switch (year) {
        2015 => @import("2015/runner.zig").run(problem, day),
        2016 => @import("2016/runner.zig").run(problem, day),
        2019 => @import("2019/runner.zig").run(problem, day),
        2020 => @import("2020/runner.zig").run(problem, day),
        2021 => @import("2021/runner.zig").run(problem, day),
        else => @panic("Invalid year"),
    };
    defer zig_solution.deinit(problem.allocator);
    return ExportSolution{
        .s1 = try problem.allocator.dupeZ(u8, zig_solution.s1),
        .s2 = try problem.allocator.dupeZ(u8, zig_solution.s2),
    };
}

export fn run(file: [*:0]const u8, year: u16, day: u16) ExportSolution {
    var problem = aoc.Problem.init(std.heap.page_allocator, file);

    return run_zig(&problem, year, day) catch |err| {
        const trace = @errorReturnTrace();
        std.debug.panicExtra(trace, null, "Caught error {s}", .{@errorName(err)});
    };
}
