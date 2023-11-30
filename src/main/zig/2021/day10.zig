const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var syntax_error_score: usize = 0;
    var autocomplete_scores = std.ArrayList(usize).init(problem.allocator);
    defer autocomplete_scores.deinit();
    blk: while (problem.line()) |line| {
        var openers = std.ArrayList(u8).init(problem.allocator);
        defer openers.deinit();
        for (line) |c| {
            switch (c) {
                ')' => if (openers.pop() != '(') {
                    syntax_error_score += 3;
                    continue :blk;
                },
                ']' => if (openers.pop() != '[') {
                    syntax_error_score += 57;
                    continue :blk;
                },
                '}' => if (openers.pop() != '{') {
                    syntax_error_score += 1197;
                    continue :blk;
                },
                '>' => if (openers.pop() != '<') {
                    syntax_error_score += 25137;
                    continue :blk;
                },
                else => try openers.append(c),
            }
        }

        var score: usize = 0;
        while (openers.popOrNull()) |c| {
            score = score * 5 + switch (c) {
                '(' => @as(u8, 1),
                '[' => 2,
                '{' => 3,
                '<' => 4,
                else => unreachable,
            };
        }
        try autocomplete_scores.append(score);
    }
    std.sort.insertion(usize, autocomplete_scores.items, {}, comptime std.sort.asc(usize));

    return problem.solution(syntax_error_score, autocomplete_scores.items[autocomplete_scores.items.len / 2]);
}
