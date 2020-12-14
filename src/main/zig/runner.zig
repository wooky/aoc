const aoc = @import("aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem, year: u16, day: u16) !aoc.Solution {
    return try switch (year) {
        2015 => @import("2015/runner.zig").run(problem, day),
        2016 => @import("2016/runner.zig").run(problem, day),
        2019 => @import("2019/runner.zig").run(problem, day),
        2020 => @import("2020/runner.zig").run(problem, day),
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

    const solution = try run(&problem, year, day);
    printPart(solution.p1, solution.s1);
    printPart(solution.p2, solution.s2);
}

fn printPart(p: usize, s: ?[]const u8) void {
    if (s) |ss| {
        std.debug.print("{}\n", .{ss});
        std.heap.page_allocator.free(ss);
    }
    else {
        std.debug.print("{}\n", .{p});
    }
}

fn usage(args: [][]const u8) noreturn {
    std.debug.print("Usage: {} [year] [day]\n", .{args[0]});
    std.process.exit(1);
}
