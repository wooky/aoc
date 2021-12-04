const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) aoc.Solution {
    var unprintable: usize = 0;
    var extra: u16 = 0;

    while (problem.line()) |line| {
        var saw_backtick = false;
        var skip: u8 = 0;
        for (line) |c| {
            if (skip > 0) {
                skip -= 1;
            }
            else if (saw_backtick) {
                saw_backtick = false;
                if (c == 'x') {
                    skip = 2;
                    unprintable += 3;
                }
                else {
                    unprintable += 1;
                    extra += 1;
                }
            }
            else if (c == '\\') {
                extra += 1;
                saw_backtick = true;
            }
            else if (c == '"') {
                unprintable += 1;
                extra += 1;
            }
        }
        extra += 2;
    }
    return problem.solution(unprintable, extra);
}
