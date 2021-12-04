const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var tree_list = std.ArrayList([]const u8).init(problem.allocator);
    defer tree_list.deinit();
    while (problem.line()) |line| {
        try tree_list.append(line);
    }
    const trees = tree_list.items;

    const res1 = countTrees(trees, 3, 1);
    const res2 = countTrees(trees, 1, 1) * res1 * countTrees(trees, 5, 1) * countTrees(trees, 7, 1) * countTrees(trees, 1, 2);

    return problem.solution(res1, res2);
}

fn countTrees(trees: []const []const u8, right: u8, down: u8) usize {
    var hit: usize = 0;
    var row: usize = down;
    var col: usize = right;

    while (row < trees.len) : (row += down) {
        if (trees[row][col] == '#') {
            hit += 1;
        }
        col = (col + right) % trees[row].len;
    }

    return hit;
}
