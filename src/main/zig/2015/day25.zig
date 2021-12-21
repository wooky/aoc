const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var tokens = std.mem.tokenize(u8, problem.input, " .,\n");
    var idx: u8 = 0;
    while (idx < 15) : (idx += 1) {
        _ = tokens.next().?;
    }
    const target_row = try std.fmt.parseInt(u16, tokens.next().?, 10);
    _ = tokens.next().?;
    const target_col = try std.fmt.parseInt(u16, tokens.next().?, 10);

    var last: usize = 20151125;
    var row: u16 = 1;
    var col: u16 = 1;
    while (row != target_row or col != target_col) {
        last = (last * 252533) % 33554393;
        if (row == 1) {
            row = col + 1;
            col = 1;
        }
        else {
            row -= 1;
            col += 1;
        }
    }
    const res1 = last;

    return problem.solution(res1, 0);
}
