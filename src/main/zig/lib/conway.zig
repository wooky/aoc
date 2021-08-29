const std = @import("std");
const Allocator = std.mem.Allocator;

fn ActiveSpots(comptime C: type) type { return std.AutoHashMap(C, void); }

pub fn Conway(comptime C: type) type {
    return struct {
        const Self = @This();

        allocator: *Allocator,
        active_spots: ActiveSpots(C),

        pub fn init(allocator: *Allocator) Self {
            return .{
                .allocator = allocator,
                .active_spots = ActiveSpots(C).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.active_spots.deinit();
        }

        pub fn stepIterator(self: *Self) ConwayIterator(C) {
            var prev_active_spots = self.active_spots;
            self.active_spots = ActiveSpots(C).init(self.allocator);
            return ConwayIterator(C).init(self.allocator, prev_active_spots, &self.active_spots);
        }
    };
}

pub fn ConwayIterator(comptime C: type) type {
    return struct {
        const Stage = union (enum) {
            not_started: void,
            iterate_active: ActiveSpots(C).KeyIterator,
            iterate_inactive: ActiveSpots(C).KeyIterator,
            done: void,
        };

        const Self = @This();

        prev_active_spots: ActiveSpots(C),
        next_active_spots: *ActiveSpots(C),
        inactive_spots: ActiveSpots(C),
        stage: Stage = .{ .not_started = {} },

        coord: C = undefined,
        active: bool = undefined,
        active_neighbors: u8 = undefined,

        fn init(allocator: *Allocator, prev_active_spots: ActiveSpots(C), next_active_spots: *ActiveSpots(C)) Self {
            return .{
                .prev_active_spots = prev_active_spots,
                .next_active_spots = next_active_spots,
                .inactive_spots = ActiveSpots(C).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.prev_active_spots.deinit();
            self.inactive_spots.deinit();
        }

        pub fn next(self: *Self) !bool {
            while (true) {
                switch (self.stage) {
                    .not_started => {
                        self.stage = .{ .iterate_active = self.prev_active_spots.keyIterator() };
                        continue;
                    },
                    .iterate_active => |*iter| {
                        if (iter.next()) |coord| {
                            return self.iterateActive(coord.*);
                        }
                        self.stage = .{ .iterate_inactive = self.inactive_spots.keyIterator() };
                        continue;
                    },
                    .iterate_inactive => |*iter| {
                        if (iter.next()) |coord| {
                            return self.iterateInactive(coord.*);
                        }
                        self.stage = .{ .done = {} };
                        continue;
                    },
                    .done => return false,
                }
            }
        }

        pub fn setActive(self: *Self, active: bool) !void {
            if (active) {
                try self.next_active_spots.put(self.coord, {});
            }
            else if (self.stage == .iterate_inactive) {
                try self.inactive_spots.put(self.coord, {});
            }
        }

        fn iterateActive(self: *Self, coord: C) !bool {
            self.coord = coord;
            self.active = true;
            self.active_neighbors = 0;
            var neighbors = coord.neighbors();
            while (neighbors.next()) |neighbor| {
                if (self.prev_active_spots.contains(neighbor)) {
                    self.active_neighbors += 1;
                }
                else {
                    try self.inactive_spots.put(neighbor, {});
                }
            }
            return true;
        }

        fn iterateInactive(self: *Self, coord: C) bool {
            self.coord = coord;
            self.active = false;
            self.active_neighbors = 0;
            var neighbors = coord.neighbors();
            while (neighbors.next()) |neighbor| {
                if (self.prev_active_spots.contains(neighbor)) {
                    self.active_neighbors += 1;
                }
            }
            return true;
        }
    };
}
