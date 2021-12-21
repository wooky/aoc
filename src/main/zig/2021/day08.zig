const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var easy_digs: usize = 0;
    var sum: usize = 0;

    while (problem.line()) |line| {
        var unprocessed_digits = [_]u7 {0} ** 6;
        var definite_digits = [_]u7 {0} ** 10;
        var tokens = std.mem.tokenize(u8, line, " ");

        var unprocessed_digits_idx: u8 = 0;
        while (tokens.next()) |token| {
            if (token[0] == '|') {
                break;
            }
            const bitmask = segmentToBitmask(token);
            switch (token.len) {
                2 => definite_digits[1] = bitmask,
                3 => definite_digits[7] = bitmask,
                4 => definite_digits[4] = bitmask,
                7 => definite_digits[8] = bitmask,
                else => {
                    unprocessed_digits[unprocessed_digits_idx] = bitmask;
                    unprocessed_digits_idx += 1;
                }
            }
        }

        blk: while (true) {
            unprocessed_digits_idx = 0;
            while (unprocessed_digits_idx < unprocessed_digits.len) : (unprocessed_digits_idx += 1) {
                const dig = unprocessed_digits[unprocessed_digits_idx];
                if (dig == 0) {
                    continue;
                }

                if (definite_digits[9] != 0) {
                    if (@popCount(u7, dig) == 5) {
                        if (dig & definite_digits[9] != dig) {
                            definite_digits[2] = dig;
                        }
                        else if (dig & definite_digits[1] == definite_digits[1]) {
                            definite_digits[3] = dig;
                        }
                        else {
                            definite_digits[5] = dig;
                        }
                        unprocessed_digits[unprocessed_digits_idx] = 0;
                        continue :blk;
                    }
                    if (definite_digits[5] != 0) {
                        if (dig & definite_digits[5] == definite_digits[5]) {
                            definite_digits[6] = dig;
                        }
                        else {
                            definite_digits[0] = dig;
                        }
                        unprocessed_digits[unprocessed_digits_idx] = 0;
                        continue :blk;
                    }
                }
                else {
                    const four_seven = definite_digits[4] | definite_digits[7];
                    if (four_seven & dig == four_seven) {
                        definite_digits[9] = dig;
                        unprocessed_digits[unprocessed_digits_idx] = 0;
                        continue :blk;
                    }
                }
            }
            break;
        }

        var output: usize = 0;
        while (tokens.next()) |token| {
            const bitmask = segmentToBitmask(token);
            const n = blk: {
                for (definite_digits) |dd, idx| {
                    if (bitmask == dd) {
                        break :blk idx;
                    }
                }
                unreachable;
            };
            output = output * 10 + n;

            switch (token.len) {
                2, 3, 4, 7 => easy_digs += 1,
                else => {}
            }
        }
        sum += output;
    }

    return problem.solution(easy_digs, sum);
}

fn segmentToBitmask(segment: []const u8) u7 {
    var bitmask: u7 = 0;
    for (segment) |s| {
        bitmask |= @intCast(u7, 1) << @intCast(u3, s - 'a');
    }
    return bitmask;
}
