const aoc = @import("../aoc.zig");
const std = @import("std");
const TARGET: u16 = 2020;

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var numbers = std.ArrayList(u16).init(problem.allocator);
    defer numbers.deinit();

    while (problem.line()) |line| {
        try numbers.append(try std.fmt.parseInt(u16, line, 10));
    }

    return aoc.Solution {
        .p1 = mult(numbers.items, 0, 1, 1).?,
        .p2 = mult(numbers.items, 0, 1, 2).?
    };
}

fn mult(numbers: []const u16, sum: u16, prod: usize, cnt: u8) ?usize {
    if (sum > TARGET) {
        return null;
    }

    var idx: usize = 0;
    while (idx < numbers.len - cnt) : (idx += 1) {
        const curr = numbers[idx];
        const curr_sum = sum + curr;
        const curr_prod = prod * curr;

        if (cnt == 0) {
            if (curr_sum == TARGET) {
                return curr_prod;
            }
        }
        else if (mult(numbers[cnt+1..], curr_sum, curr_prod, cnt - 1)) |res| {
            return res;
        }
    }
    return null;
}
