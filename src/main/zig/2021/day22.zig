const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var cubes_on = std.AutoHashMap(aoc.Coord3D, void).init(problem.allocator);
    defer cubes_on.deinit();
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(u8, line, " xyz=.,");
        const on = std.mem.eql(u8, "on", tokens.next().?);
        const x_from = std.math.max(try std.fmt.parseInt(isize, tokens.next().?, 10), -50);
        const x_to = std.math.min(try std.fmt.parseInt(isize, tokens.next().?, 10), 50);
        const y_from = std.math.max(try std.fmt.parseInt(isize, tokens.next().?, 10), -50);
        const y_to = std.math.min(try std.fmt.parseInt(isize, tokens.next().?, 10), 50);
        const z_from = std.math.max(try std.fmt.parseInt(isize, tokens.next().?, 10), -50);
        const z_to = std.math.min(try std.fmt.parseInt(isize, tokens.next().?, 10), 50);
        if (x_from > x_to or y_from > y_to or z_from > z_to) {
            continue;
        }

        var iter = aoc.CoordRangeIterator3D.init(
            aoc.Coord3D.init(.{x_from, y_from, z_from}),
            aoc.Coord3D.init(.{x_to, y_to, z_to})
        );
        while (iter.next()) |coord| {
            if (on) {
                try cubes_on.put(coord, {});
            }
            else {
                _ = cubes_on.remove(coord);
            }
        }
    }

    return problem.solution(cubes_on.count(), 0);
}
