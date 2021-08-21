const aoc = @import("../aoc.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

fn DimensionPoint(comptime T: type) type {
    comptime const fields = @typeInfo(T).Struct.fields;
    return struct {
        const Self = @This();
        const CoordList = std.AutoHashMap(Self, void);

        point: T,
        neighbor_offset: ?T = undefined,

        fn init(point: T) Self {
            var dim_point = Self { .point = point };
            dim_point.resetOffset();
            return dim_point;
        }

        fn resetOffset(self: *Self) void {
            self.neighbor_offset = .{};
            inline for (fields) |field| {
                @field(self.neighbor_offset.?, field.name) = -1;
            }
        }

        fn nextNeighbor(self: *Self) ?Self {
            if (self.neighbor_offset) |*offset| {
                var next_point = init(self.point);
                inline for (fields) |field| {
                    @field(next_point.point, field.name) += @field(offset, field.name);
                }

                var all_zeroes = true;
                var is_last: bool = undefined;
                while (all_zeroes) {
                    is_last = true;
                    inline for (fields) |field| {
                        if (is_last) {
                            @field(offset, field.name) = switch (@field(offset, field.name)) {
                                -1 => blk: { is_last = false; break :blk 0; },
                                0 => blk: { is_last = false; break :blk 1; },
                                1 => blk: { break :blk -1; },
                                else => unreachable
                            };
                        }
                        all_zeroes = all_zeroes and (@field(offset, field.name) == 0);
                    }
                }
                if (is_last) {
                    self.neighbor_offset = null;
                }

                return next_point;
            }
            
            self.resetOffset();
            return null;
        }
    };
}

const DimensionPoint3 = DimensionPoint(struct { x: i8 = 0, y: i8 = 0, z: i8 = 0 });
const DimensionPoint4 = DimensionPoint(struct { x: i8 = 0, y: i8 = 0, z: i8 = 0, w: i8 = 0 });

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var active_cubes_3d = DimensionPoint3.CoordList.init(problem.allocator);
    // defer active_cubes_3d.deinit();
    var active_cubes_4d = DimensionPoint4.CoordList.init(problem.allocator);
    var y: i8 = 0;
    while (problem.line()) |line| : (y += 1) {
        for (line) |state, x| {
            if (state == '#') {
                try active_cubes_3d.put(DimensionPoint3.init(.{ .x = @intCast(i8, x), .y = y }), {});
                try active_cubes_4d.put(DimensionPoint4.init(.{ .x = @intCast(i8, x), .y = y }), {});
            }
        }
    }

    return aoc.Solution {
        .p1 = try boot(problem.allocator, DimensionPoint3.CoordList, active_cubes_3d),
        .p2 = try boot(problem.allocator, DimensionPoint4.CoordList, active_cubes_4d),
    };
}

fn boot(allocator: *Allocator, comptime CoordList: type, active_cubes_copy: anytype) !usize {
    var active_cubes = active_cubes_copy;
    defer active_cubes.deinit();
    var cycles: u8 = 0;
    while (cycles < 6) : (cycles += 1) {
        var inactive_cubes_to_process = CoordList.init(allocator);
        defer inactive_cubes_to_process.deinit();
        var new_active_cubes = CoordList.init(allocator);

        var iter = active_cubes.iterator();
        while (iter.next()) |kv| {
            var cube = kv.key_ptr.*;
            var active_neighbors: u8 = 0;
            while (cube.nextNeighbor()) |neighbor| {
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
            var cube = kv.key_ptr.*;
            var active_neighbors: u8 = 0;
            while (cube.nextNeighbor()) |neighbor| {
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
    return active_cubes.count();
}
