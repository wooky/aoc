const aoc = @import("../aoc.zig");

const Coord4D = struct {
    x: i8,
    y: i8,
    z: i8,
    w: i8,
    pub usingnamespace aoc.GenericCoord(Coord4D);
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var conway_3d = aoc.Conway(aoc.Coord3D).init(problem.allocator);
    defer conway_3d.deinit();
    var conway_4d = aoc.Conway(Coord4D).init(problem.allocator);
    defer conway_4d.deinit();
    var y: i8 = 0;
    while (problem.line()) |line| : (y += 1) {
        for (line, 0..) |state, x| {
            if (state == '#') {
                try conway_3d.active_spots.put(aoc.Coord3D.init(.{ @as(i8, @intCast(x)), y, 0 }), {});
                try conway_4d.active_spots.put(Coord4D.init(.{ @as(i8, @intCast(x)), y, 0, 0 }), {});
            }
        }
    }

    return problem.solution(
        try boot(&conway_3d),
        try boot(&conway_4d),
    );
}

fn boot(conway: anytype) !usize {
    var cycles: u8 = 0;
    while (cycles < 6) : (cycles += 1) {
        var iter = conway.stepIterator();
        defer iter.deinit();
        while (try iter.next()) {
            try iter.setActive(iter.active_neighbors == 3 or (iter.active and iter.active_neighbors == 2));
        }
    }
    return conway.active_spots.count();
}
