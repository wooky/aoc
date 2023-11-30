const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var numbers = std.ArrayList(usize).init(problem.allocator);
    defer numbers.deinit();
    var idx: usize = 0;
    while (idx < 25) : (idx += 1) {
        try numbers.append(try std.fmt.parseInt(usize, problem.line().?, 10));
    }

    const res1 = blk: {
        idx = 0;
        outer: while (true) : (idx += 1) {
            const last_number = try std.fmt.parseInt(usize, problem.line().?, 10);
            for (numbers.items[idx .. numbers.items.len - 1], 0..) |a, i| {
                for (numbers.items[idx + i + 1 ..]) |b| {
                    if (a + b == last_number) {
                        try numbers.append(last_number);
                        continue :outer;
                    }
                }
            }
            break :blk last_number;
        }
    };

    const res2 = blk: {
        const range = blk2: {
            outer: for (numbers.items[0 .. numbers.items.len - 1], 0..) |a, i| {
                var sum = a;
                for (numbers.items[i + 1 ..], 0..) |b, j| {
                    sum += b;
                    if (sum > res1) {
                        continue :outer;
                    }
                    if (sum == res1) {
                        break :blk2 .{ .from = i, .to = i + j + 2 };
                    }
                }
            }
            unreachable;
        };

        const subarray = numbers.items[range.from..range.to];
        std.sort.insertion(usize, subarray, {}, comptime std.sort.asc(usize));
        break :blk subarray[0] + subarray[subarray.len - 1];
    };

    return problem.solution(res1, res2);
}
