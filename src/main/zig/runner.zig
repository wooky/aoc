const aoc = @import("aoc.zig");
const std = @import("std");

pub const io_mode = .evented;

pub fn run(problem: *aoc.Problem, year: u16, day: u16) !aoc.Solution {
    return try switch (year) {
        2015 => @import("2015/runner.zig").run(problem, day),
        2016 => @import("2016/runner.zig").run(problem, day),
        2017 => @import("2017/runner.zig").run(problem, day),
        2019 => @import("2019/runner.zig").run(problem, day),
        2020 => @import("2020/runner.zig").run(problem, day),
        2021 => @import("2021/runner.zig").run(problem, day),
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
            std.debug.print("Invalid year/day provided\n", .{});
            std.process.exit(1);
        },
        else => return err
    };
    defer problem.deinit();

    var solution = try run(&problem, year, day);
    defer solution.deinit(std.heap.page_allocator);
    std.debug.print("{s}\n{s}\n", .{solution.s1, solution.s2});
}

fn usage(args: [][]const u8) noreturn {
    std.debug.print("Usage: {s} [year] [day]\n", .{args[0]});
    std.process.exit(1);
}
