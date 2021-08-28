const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var visited = std.AutoHashMap(aoc.Coord, void).init(problem.allocator);
    defer visited.deinit();
    var already_visited: ?aoc.Coord = null;
    var curr_pos = aoc.PredefinedCoord.ORIGIN;
    var delta = aoc.PredefinedCoord.UP;
    var tokens = std.mem.tokenize(problem.input, ", \n");
    while (tokens.next()) |token| {
        var direction = token[0];
        var distance = try std.fmt.parseInt(i16, token[1..], 10);
        switch (direction) {
            'L' => delta.mutRotate90DegreesCounterclockwise(),
            'R' => delta.mutRotate90DegreesClockwise(),
            else => unreachable
        }

        var step_pos = curr_pos;
        curr_pos.mutAdd(delta.multiply(distance));

        while (!step_pos.equals(curr_pos)) : (step_pos.mutAdd(delta)) {
            const res = try visited.getOrPut(step_pos);
            if (res.found_existing and already_visited == null) {
                already_visited = step_pos;
            }
        }
    }

    return aoc.Solution {
        .p1 = curr_pos.distanceFromOrigin(),
        .p2 = already_visited.?.distanceFromOrigin()
    };
}
