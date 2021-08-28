const aoc = @import("../aoc.zig");
const std = @import("std");

const BlackTiles = std.AutoHashMap(aoc.Coord, void);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var black_tiles = BlackTiles.init(problem.allocator);
    defer black_tiles.deinit();
    while (problem.line()) |line| {
        var coord = aoc.PredefinedCoord.ORIGIN;
        var idx: usize = 0;
        while (idx < line.len) : (idx += 1) {
            const delta = switch (line[idx]) {
                'n' => blk: {
                    idx += 1;
                    break :blk switch (line[idx]) {
                        'w' => aoc.Coord.init(.{-1, -1}),
                        'e' => aoc.Coord.init(.{-1, 1}),
                        else => unreachable
                    };
                },
                's' => blk: {
                    idx += 1;
                    break :blk switch (line[idx]) {
                        'w' => aoc.Coord.init(.{1, -1}),
                        'e' => aoc.Coord.init(.{1, 1}),
                        else => unreachable
                    };
                },
                'w' => aoc.Coord.init(.{0, -2}),
                'e' => aoc.Coord.init(.{0, 2}),
                else => unreachable
            };
            coord.mutAdd(delta);
        }

        if (!black_tiles.remove(coord)) {
            try black_tiles.put(coord, {});
        }
    }

    return aoc.Solution { .p1 = black_tiles.count(), .p2 = 0 };
}
