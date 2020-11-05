const aoc = @import("aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem, year: u16, day: u16) !aoc.Solution {
    return try switch (year) {
        2015 => @import("2015/runner.zig").run(problem, day),
        2019 => @import("2019/runner.zig").run(problem, day),
        else => unreachable,
    };
}

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);
    if (args.len < 3) {
        usage(args);
    }
    const year = std.fmt.parseInt(u16, args[1], 10) catch usage(args);
    const day = std.fmt.parseInt(u16, args[2], 10) catch usage(args);

    var problem = aoc.Problem.init(year, day, std.heap.page_allocator) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.warn("Invalid year/day provided\n", .{});
            std.process.exit(1);
        },
        else => return err
    };
    defer problem.deinit();

    const solution = try run(&problem, year, day);
    std.debug.warn("{}\n{}\n", .{solution.p1, solution.p2});
}

fn usage(args: [][]const u8) noreturn {
    std.debug.warn("Usage: {} [year] [day]\n", .{args[0]});
    std.process.exit(1);
}
