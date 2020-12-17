const aoc = @import("../aoc.zig");
const std = @import("std");

const Coord = struct {
    x: i8,
    y: i8,
    z: i8,

    fn getAllNeighbors(self: *const Coord) [26]Coord {
        return [_]Coord {
            .{ .x = self.x - 1, .y = self.y - 1, .z = self.z - 1 },
            .{ .x = self.x    , .y = self.y - 1, .z = self.z - 1 },
            .{ .x = self.x + 1, .y = self.y - 1, .z = self.z - 1 },
            .{ .x = self.x - 1, .y = self.y    , .z = self.z - 1 },
            .{ .x = self.x    , .y = self.y    , .z = self.z - 1 },
            .{ .x = self.x + 1, .y = self.y    , .z = self.z - 1 },
            .{ .x = self.x - 1, .y = self.y + 1, .z = self.z - 1 },
            .{ .x = self.x    , .y = self.y + 1, .z = self.z - 1 },
            .{ .x = self.x + 1, .y = self.y + 1, .z = self.z - 1 },
            .{ .x = self.x - 1, .y = self.y - 1, .z = self.z     },
            .{ .x = self.x    , .y = self.y - 1, .z = self.z     },
            .{ .x = self.x + 1, .y = self.y - 1, .z = self.z     },
            .{ .x = self.x - 1, .y = self.y    , .z = self.z     },
            .{ .x = self.x + 1, .y = self.y    , .z = self.z     },
            .{ .x = self.x - 1, .y = self.y + 1, .z = self.z     },
            .{ .x = self.x    , .y = self.y + 1, .z = self.z     },
            .{ .x = self.x + 1, .y = self.y + 1, .z = self.z     },
            .{ .x = self.x - 1, .y = self.y - 1, .z = self.z + 1 },
            .{ .x = self.x    , .y = self.y - 1, .z = self.z + 1 },
            .{ .x = self.x + 1, .y = self.y - 1, .z = self.z + 1 },
            .{ .x = self.x - 1, .y = self.y    , .z = self.z + 1 },
            .{ .x = self.x    , .y = self.y    , .z = self.z + 1 },
            .{ .x = self.x + 1, .y = self.y    , .z = self.z + 1 },
            .{ .x = self.x - 1, .y = self.y + 1, .z = self.z + 1 },
            .{ .x = self.x    , .y = self.y + 1, .z = self.z + 1 },
            .{ .x = self.x + 1, .y = self.y + 1, .z = self.z + 1 },
        };
    }
};
const CoordList = std.AutoHashMap(Coord, void);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var active_cubes = CoordList.init(problem.allocator);
    defer active_cubes.deinit();
    var y: i8 = 0;
    while (problem.line()) |line| : (y += 1) {
        for (line) |state, x| {
            if (state == '#') {
                try active_cubes.put(.{ .x = @intCast(i8, x), .y = y, .z = 0 }, {});
            }
        }
    }

    const res1 = blk: {
        var cycles: u8 = 0;
        while (cycles < 6) : (cycles += 1) {
            var inactive_cubes_to_process = CoordList.init(problem.allocator);
            defer inactive_cubes_to_process.deinit();
            var new_active_cubes = CoordList.init(problem.allocator);

            var iter = active_cubes.iterator();
            while (iter.next()) |kv| {
                const cube = kv.key;
                var active_neighbors: u8 = 0;
                for (cube.getAllNeighbors()) |neighbor| {
                    if (active_cubes.contains(neighbor)) {
                        active_neighbors += 1;
                    }
                    else {
                        try inactive_cubes_to_process.put(neighbor, {});
                    }
                }
                if (active_neighbors == 2 or active_neighbors == 3) {
                    try new_active_cubes.put(cube, {});
                }
            }

            iter = inactive_cubes_to_process.iterator();
            while (iter.next()) |kv| {
                const cube = kv.key;
                var active_neighbors: u8 = 0;
                for (cube.getAllNeighbors()) |neighbor| {
                    if (active_cubes.contains(neighbor)) {
                        active_neighbors += 1;
                    }
                }
                if (active_neighbors == 3) {
                    try new_active_cubes.put(cube, {});
                }
            }

            active_cubes.deinit();
            active_cubes = new_active_cubes;
        }
        break :blk active_cubes.count();
    };

    return aoc.Solution { .p1 = res1, .p2 = 0 };
}
