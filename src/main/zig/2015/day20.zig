const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    const input = try std.fmt.parseInt(usize, std.mem.trim(u8, problem.input, &std.ascii.whitespace), 10);
    var res1: ?usize = null;
    var res2: ?usize = null;

    var house: usize = 1;
    while (res1 == null or res2 == null) : (house += 1) {
        var factor1_sum: usize = 0;
        var factor2_sum: usize = 0;

        var i: usize = 1;
        const max = std.math.sqrt(house);
        while (i <= max) : (i += 1) {
            if (house % i == 0) {
                factor1_sum += i;
                if (i * 50 >= house) {
                    factor2_sum += i;
                }

                const other_factor = house / i;
                if (other_factor != i) {
                    factor1_sum += other_factor;
                    if (other_factor * 50 >= house) {
                        factor2_sum += other_factor;
                    }
                }
            }
        }
        if (res1 == null and factor1_sum * 10 >= input) {
            res1 = house;
        } else if (res2 == null and factor2_sum * 11 >= input) {
            res2 = house;
        }
    }

    return problem.solution(res1.?, res2.?);
}
