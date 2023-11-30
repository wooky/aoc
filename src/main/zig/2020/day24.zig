const aoc = @import("../aoc.zig");
const std = @import("std");

const BlackTiles = std.AutoHashMap(aoc.Coord, void);

const HexCoord = struct {
    const NW = aoc.Coord.init(.{ -1, -1 });
    const NE = aoc.Coord.init(.{ -1, 1 });
    const SW = aoc.Coord.init(.{ 1, -1 });
    const SE = aoc.Coord.init(.{ 1, 1 });
    const W = aoc.Coord.init(.{ 0, -2 });
    const E = aoc.Coord.init(.{ 0, 2 });

    underlying: aoc.Coord = aoc.PredefinedCoord.ORIGIN,

    fn add(self: *const HexCoord, other: aoc.Coord) HexCoord {
        return .{ .underlying = self.underlying.add(other) };
    }

    fn mutAdd(self: *HexCoord, other: aoc.Coord) void {
        self.underlying.mutAdd(other);
    }

    pub fn neighbors(self: *const HexCoord, _: bool) NeighborIterator {
        return NeighborIterator.init(self.*);
    }

    const NeighborIterator = struct {
        idx: u8 = 0,
        neighbors: [6]HexCoord,

        fn init(center: HexCoord) NeighborIterator {
            return .{ .neighbors = [_]HexCoord{
                center.add(NW),
                center.add(NE),
                center.add(SW),
                center.add(SE),
                center.add(W),
                center.add(E),
            } };
        }

        pub fn next(self: *NeighborIterator) ?HexCoord {
            if (self.idx == self.neighbors.len) {
                return null;
            }
            const res = self.neighbors[self.idx];
            self.idx += 1;
            return res;
        }
    };
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var conway = aoc.Conway(HexCoord).init(problem.allocator);
    defer conway.deinit();
    while (problem.line()) |line| {
        var coord = HexCoord{};
        var idx: usize = 0;
        while (idx < line.len) : (idx += 1) {
            const delta = switch (line[idx]) {
                'n' => blk: {
                    idx += 1;
                    break :blk switch (line[idx]) {
                        'w' => HexCoord.NW,
                        'e' => HexCoord.NE,
                        else => unreachable,
                    };
                },
                's' => blk: {
                    idx += 1;
                    break :blk switch (line[idx]) {
                        'w' => HexCoord.SW,
                        'e' => HexCoord.SE,
                        else => unreachable,
                    };
                },
                'w' => HexCoord.W,
                'e' => HexCoord.E,
                else => unreachable,
            };
            coord.mutAdd(delta);
        }

        if (!conway.active_spots.remove(coord)) {
            try conway.active_spots.put(coord, {});
        }
    }
    const solution1 = conway.active_spots.count();

    var cycle: u8 = 0;
    while (cycle < 100) : (cycle += 1) {
        var iter = conway.stepIterator();
        defer iter.deinit();
        while (try iter.next()) {
            try iter.setActive((iter.active and !(iter.active_neighbors == 0 or iter.active_neighbors > 2)) or
                (!iter.active and iter.active_neighbors == 2));
        }
    }
    const solution2 = conway.active_spots.count();

    return problem.solution(solution1, solution2);
}
