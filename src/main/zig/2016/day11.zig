const aoc = @import("../aoc.zig");
const std = @import("std");

const Object = union(enum) {
    Generator: []const u8,
    Microchip: []const u8,
};

const Situation = struct {
    const ObjectArray = std.ArrayList(Object);

    floors: [4]ObjectArray,
    elevator: u8,
    steps: usize,

    fn init(allocator: *std.mem.Allocator, elevator: u8, steps: usize) Situation {
        var situation = Situation {
            .floors = undefined,
            .elevator = elevator,
            .steps = steps,
        };
        for (situation.floors) |*floor| {
            floor.* = ObjectArray.init(allocator);
        }
        return situation;
    }

    fn copyToNextFloor(self: *const Situation, delta: i2) !?Situation {
        if ((delta == 1 and self.elevator == 3) or (delta == -1 and self.elevator == 0)) {
            return null;
        }
        var situation = Situation {
            .floors = undefined,
            .elevator = @intCast(u8, @intCast(i8, self.elevator) + delta),
            .steps = self.steps + 1,
        };
        for (situation.floors) |*floor, idx| {
            floor.* = ObjectArray.init(self.floors[0].allocator);
            try floor.appendSlice(self.floors[idx].items);
        }
        return situation;
    }

    fn deinit(self: *Situation) void {
        for (self.floors) |*floor| {
            floor.deinit();
        }
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var initial_situation = Situation.init(problem.allocator, 0, 0);
    {
        var floor: u8 = 0;
        while (floor < 3) : (floor += 1) {
            var tokens = std.mem.tokenize(problem.line().?, " ,.");
            var last_token: []const u8 = undefined;
            while (tokens.next()) |token| {
                if (std.mem.startsWith(u8, token, "generator")) {
                    try initial_situation.floors[floor].append(.{ .Generator = last_token });
                }
                else if (std.mem.endsWith(u8, token, "-compatible")) {
                    try initial_situation.floors[floor].append(.{ .Microchip = token[0..token.len - "-compatible".len] });
                }
                last_token = token;
            }
        }
    }

    var min_steps: usize = std.math.maxInt(usize);
    var queue = std.ArrayList(Situation).init(problem.allocator);
    defer queue.deinit();
    try queue.append(initial_situation);
    blk: while (queue.items.len != 0) {
        const situation = queue.orderedRemove(0);
        if (situation.steps >= min_steps) {
            continue;
        }

        // Validate that all but the top floor are valid
        var all_floors_empty = true;
        for (situation.floors[0..3]) |floor| {
            for (floor.items) |object| {
                if (std.meta.activeTag(object) == .Microchip) {
                    var any_generator = false;
                    var corresponding_generator = false;
                    for (floor.items) |object2| {
                        if (std.meta.activeTag(object2) == .Generator) {
                            any_generator = true;
                            if (std.mem.eql(u8, object2.Generator, object.Microchip)) {
                                corresponding_generator = true;
                            }
                        }
                    }
                    if (any_generator and !corresponding_generator) {
                        continue :blk;
                    }
                }
                all_floors_empty = false;
            }
        }

        // Return successfully if all but the top floor are empty
        if (all_floors_empty) {
            min_steps = situation.steps;
            continue;
        }

        // Gather all the item combinations that can be taken to the elevator
        for (situation.floors[situation.elevator].items) |obj1, i| {
            if (try situation.copyToNextFloor(1)) |situation_up| {
                _ = situation_up.floors[situation.elevator].swapRemove(i);
                try situation_up.floors[situation_up.elevator].append(obj1);
                try queue.append(situation_up);
            }

            for (situation.floors[situation.elevator].items[i+1..]) |obj2, j| {
                if (try situation.copyToNextFloor(1)) |situation_up| {
                    _ = situation_up.floors[situation.elevator].swapRemove(j);
                    _ = situation_up.floors[situation.elevator].swapRemove(i);
                    try situation_up.floors[situation_up.elevator].append(obj1);
                    try situation_up.floors[situation_up.elevator].append(obj2);
                    try queue.append(situation_up);
                }
            }
        }
    }

    return problem.solution(0, 0);
}
