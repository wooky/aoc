const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var tokens = std.mem.tokenize(u8, problem.input, "targe: xy=.,");
    const x_min = try std.fmt.parseInt(isize, tokens.next().?, 10);
    const x_max = try std.fmt.parseInt(isize, tokens.next().?, 10);
    const y_low = try std.fmt.parseInt(isize, tokens.next().?, 10);
    const y_high = try std.fmt.parseInt(isize, tokens.next().?, 10);

    var max_y: isize = undefined;
    var choices = std.AutoHashMap(aoc.Coord2D, void).init(problem.allocator);
    defer choices.deinit();
    var vy: isize = -200;
    while (vy < 200) : (vy += 1) {
        var t: isize = 1;
        while (true) : (t += 1) {
            const y = calcY(t, vy);
            if (y > y_high) {
                continue;
            }
            if (y < y_low) {
                break;
            }

            var vx: isize = 1;
            while (true) : (vx += 1) {
                const x = calcX(t, vx);
                if (x < x_min) {
                    continue;
                }
                if (x > x_max) {
                    break;
                }
                max_y = cumsum(vy);
                try choices.put(aoc.Coord2D.init(.{ vx, vy }), {});
            }
        }
    }

    return problem.solution(max_y, choices.count());
}

inline fn calcX(t: isize, vx: isize) isize {
    const t_capped = @min(vx, t);
    return vx * t_capped - cumsum(t_capped - 1);
}

inline fn calcY(t: isize, vy: isize) isize {
    if (vy <= 0) {
        return vy * t - cumsum(t - 1);
    }
    return cumsum(vy) - cumsum(vy - t);
}

inline fn cumsum(n: isize) isize {
    return @divExact(n * (n + 1), 2);
}
