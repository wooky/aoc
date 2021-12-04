const aoc = @import("../aoc.zig");
const std = @import("std");
const Joltage = u8;

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var joltages = std.ArrayList(Joltage).init(problem.allocator);
    defer joltages.deinit();
    while (problem.line()) |line| {
        try joltages.append(try std.fmt.parseInt(Joltage, line, 10));
    }
    std.sort.sort(Joltage, joltages.items, {}, comptime std.sort.asc(Joltage));

    const res1 = blk: {
        var last_joltage: Joltage = 0;
        var diff1: usize = 0;
        var diff3: usize = 1;
        for (joltages.items) |joltage| {
            switch (joltage - last_joltage) {
                1 => diff1 += 1,
                3 => diff3 += 1,
                else => unreachable
            }
            last_joltage = joltage;
        }
        break :blk diff1 * diff3;
    };

    const res2 = blk: {
        var diversions = std.AutoHashMap(Joltage, usize).init(problem.allocator);
        defer diversions.deinit();
        try diversions.put(0, 1);
        for (joltages.items) |joltage| {
            var diversion_count: usize = 0;
            var prev_joltage = std.math.max(joltage, 3) - 3;
            while (prev_joltage < joltage) : (prev_joltage += 1) {
                if (diversions.get(prev_joltage)) |prev_diversions| {
                    diversion_count += prev_diversions;
                }
            }
            try diversions.put(joltage, diversion_count);
        }
        break :blk diversions.get(joltages.items[joltages.items.len-1]).?;
    };

    return problem.solution(res1, res2);
}
