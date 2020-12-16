const aoc = @import("../aoc.zig");
const std = @import("std");

const Coord = struct {
    x: i16,
    y: i16,

    fn distance(self: *const Coord) usize {
        return std.math.absCast(self.x) + std.math.absCast(self.y);
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var visited = std.AutoHashMap(Coord, void).init(problem.allocator);
    defer visited.deinit();
    var already_visited: ?Coord = null;
    var curr_pos = Coord { .x = 0, .y = 0 };
    var delta = Coord { .x = 0, .y = -1 };
    var tokens = std.mem.tokenize(problem.input, ", \n");
    while (tokens.next()) |token| {
        var direction = token[0];
        var distance = try std.fmt.parseInt(i16, token[1..], 10);
        const new_delta: Coord = switch (direction) {
            'L' => .{ .x = delta.y, .y = -delta.x },
            'R' => .{ .x = -delta.y, .y = delta.x },
            else => unreachable
        };
        delta = new_delta;

        var step_pos = curr_pos;
        curr_pos.x += distance * delta.x;
        curr_pos.y += distance * delta.y;

        while (step_pos.x != curr_pos.x or step_pos.y != curr_pos.y) : ({step_pos.x += delta.x; step_pos.y += delta.y;}) {
            const res = try visited.getOrPut(step_pos);
            if (res.found_existing and already_visited == null) {
                already_visited = step_pos;
            }
        }
    }

    return aoc.Solution { .p1 = curr_pos.distance(), .p2 = already_visited.?.distance() };
}
