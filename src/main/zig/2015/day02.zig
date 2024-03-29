const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var wrapping_paper: u64 = 0;
    var ribbon: u64 = 0;
    while (problem.line()) |line| {
        var dimensions = std.mem.tokenize(u8, line, "x");
        const dim1 = try std.fmt.parseInt(u32, dimensions.next().?, 10);
        const dim2 = try std.fmt.parseInt(u32, dimensions.next().?, 10);
        const dim3 = try std.fmt.parseInt(u32, dimensions.next().?, 10);

        const area1 = dim1 * dim2;
        const area2 = dim1 * dim3;
        const area3 = dim2 * dim3;

        const slack = @min(@min(area1, area2), area3);
        const present_wrap = 2 * (dim1 + dim2 + dim3 - @max(@max(dim1, dim2), dim3));
        const bow = dim1 * dim2 * dim3;

        wrapping_paper += 2 * (area1 + area2 + area3) + slack;
        ribbon += present_wrap + bow;
    }
    return problem.solution(wrapping_paper, ribbon);
}
