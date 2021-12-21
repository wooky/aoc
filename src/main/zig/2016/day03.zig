const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var valid_hor: usize = 0;
    var valid_ver: usize = 0;
    while (problem.line()) |line| {
        const line1 = try lineToSides(line);
        const line2 = try lineToSides(problem.line().?);
        const line3 = try lineToSides(problem.line().?);
        
        valid_hor += isValidTriangle(line1[0], line1[1], line1[2]) +
                     isValidTriangle(line2[0], line2[1], line2[2]) +
                     isValidTriangle(line3[0], line3[1], line3[2]);

        valid_ver += isValidTriangle(line1[0], line2[0], line3[0]) +
                     isValidTriangle(line1[1], line2[1], line3[1]) +
                     isValidTriangle(line1[2], line2[2], line3[2]);
    }

    return problem.solution(valid_hor, valid_ver);
}

fn lineToSides(line: []const u8) ![3]u16 {
    var tokens = std.mem.tokenize(u8, line, " ");
    return [_]u16 {
        try std.fmt.parseInt(u16, tokens.next().?, 10),
        try std.fmt.parseInt(u16, tokens.next().?, 10),
        try std.fmt.parseInt(u16, tokens.next().?, 10),
    };
}

fn isValidTriangle(side1: u16, side2: u16, side3: u16) u8 {
    return if (side1 + side2 > side3 and side1 + side3 > side2 and side2 + side3 > side1)
        1
    else
        0;
}
