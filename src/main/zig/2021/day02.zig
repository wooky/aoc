const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var hor: u16 = 0;
    var depth_naive: u16 = 0;
    var aim: i16 = 0;
    var depth_aimed: i32 = 0;
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(u8, line, " ");
        const command = tokens.next().?;
        const units = try std.fmt.parseInt(u8, tokens.next().?, 10);
        if (std.mem.eql(u8, command, "forward")) {
            hor += units;
            depth_aimed += aim * units;
        } else if (std.mem.eql(u8, command, "down")) {
            depth_naive += units;
            aim += units;
        } else if (std.mem.eql(u8, command, "up")) {
            depth_naive -= units;
            aim -= units;
        }
    }

    return problem.solution(
        @as(u32, hor) * depth_naive,
        @as(u32, hor) * @as(u32, @intCast(depth_aimed)),
    );
}
