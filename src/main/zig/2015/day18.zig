const aoc = @import("../aoc.zig");
const std = @import("std");

const LightMap = struct {
    const Conway = aoc.Conway(aoc.Coord);
    const bounds = aoc.CoordRange.initWithBounds(aoc.Coord.init(.{0, 0}), aoc.Coord.init(.{99, 99}));

    conway: Conway,

    fn init(allocator: *std.mem.Allocator) LightMap {
        return .{ .conway = Conway.init(allocator) };
    }

    fn deinit(self: *LightMap) void {
        self.conway.deinit();
    }

    fn animate(self: *LightMap) !void {
        var iter = self.conway.stepIterator();
        while (try iter.next()) {
            if (bounds.coordInRange(iter.coord)) {
                try iter.setActive(iter.active_neighbors == 3 or (iter.active_neighbors == 2 and iter.active));
            }
        }
    }

    fn activate(self: *LightMap, coord: aoc.Coord) !void {
        try self.conway.active_spots.put(coord, {});
    }

    fn makeDefective(self: *LightMap) !void {
        try self.activate(aoc.Coord.init(.{0, 0}));
        try self.activate(aoc.Coord.init(.{0, 99}));
        try self.activate(aoc.Coord.init(.{99, 0}));
        try self.activate(aoc.Coord.init(.{99, 99}));
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var map_good = LightMap.init(problem.allocator); defer map_good.deinit();
    var map_bad = LightMap.init(problem.allocator); defer map_bad.deinit();
    var coord = aoc.PredefinedCoord.ORIGIN;
    for (problem.input) |c| {
        switch (c) {
            '#' => {
                try map_good.activate(coord);
                try map_bad.activate(coord);
            },
            '.' => {},
            '\n' => {
                coord.row += 1;
                coord.col = -1;
            },
            else => unreachable,
        }
        coord.col += 1;
    }
    try map_bad.makeDefective();

    var loop: u8 = 0;
    while (loop < 100) : (loop += 1) {
        try map_good.animate();
        try map_bad.animate();
        try map_bad.makeDefective();
    }

    return problem.solution(
        map_good.conway.active_spots.count(),
        map_bad.conway.active_spots.count(),
    );
}
