const aoc = @import("aoc.zig");
const std = @import("std");

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);
    if (args.len < 3) {
        usage(args);
    }
    const year = std.fmt.parseInt(u16, args[1], 10) catch usage(args);
    const day = std.fmt.parseInt(u16, args[2], 10) catch usage(args);

    var problem = try aoc.Problem.init(year, day, std.heap.page_allocator);

    try switch (year) {
        2015 => switch (day) {
            1 => @import("2015/day01.zig").run(&problem),
            2 => @import("2015/day02.zig").run(&problem),
            3 => @import("2015/day03.zig").run(&problem),
            4 => @import("2015/day04.zig").run(&problem),
            5 => @import("2015/day05.zig").run(&problem),
            6 => @import("2015/day06.zig").run(&problem),
            7 => @import("2015/day07.zig").run(&problem),
            8 => @import("2015/day08.zig").run(&problem),
            9 => @import("2015/day09.zig").run(&problem),
            10 => @import("2015/day10.zig").run(&problem),
            12 => @import("2015/day12.zig").run(&problem),
            13 => @import("2015/day13.zig").run(&problem),
            14 => @import("2015/day14.zig").run(&problem),
            15 => @import("2015/day15.zig").run(&problem),
            16 => @import("2015/day16.zig").run(&problem),
            17 => @import("2015/day17.zig").run(&problem),
            else => invalid("day", day),
        },
        else => invalid("year", year),
    };
}

fn usage(args: [][]const u8) noreturn {
    std.debug.warn("Usage: {} [year] [day]\n", .{args[0]});
    std.process.exit(1);
}

fn invalid(what: []const u8, val: u16) noreturn {
    std.debug.warn("Invalid {}: {}\n", .{what, val});
    std.process.exit(1);
}
