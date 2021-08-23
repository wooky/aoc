const aoc = @import("../aoc.zig");
const std = @import("std");

const Ship = struct {
    const Instruction = struct {
        action: u8,
        value: isize,
    };

    ship_coord: aoc.Coord = aoc.Coord.Predefined.ORIGIN,
    waypoint_coord: aoc.Coord,
    target_ship: bool,

    fn init(waypoint_x: isize, waypoint_y: isize, target_ship: bool) Ship {
        return Ship {
            .waypoint_coord = aoc.Coord.fromXY(waypoint_x, waypoint_y),
            .target_ship = target_ship,
        };
    }

    fn go(self: *Ship, instruction: Instruction) void {
        const target_movement = if (self.target_ship) &self.ship_coord else &self.waypoint_coord;
        switch (instruction.action) {
            'N' => target_movement.row -= instruction.value,
            'S' => target_movement.row += instruction.value,
            'E' => target_movement.col += instruction.value,
            'W' => target_movement.col -= instruction.value,
            'L' => self.rotateWaypoint(instruction.value, aoc.Coord.mutRotate90DegreesCounterclockwise),
            'R' => self.rotateWaypoint(instruction.value, aoc.Coord.mutRotate90DegreesClockwise),
            'F' => self.ship_coord.mutAdd(self.waypoint_coord.multiply(instruction.value)),
            else => unreachable
        }
    }

    fn rotateWaypoint(self: *Ship, degrees: isize, call: anytype) void {
        var i: usize = 0;
        while (i < @intCast(usize, degrees) / 90) : (i += 1) {
            @call(.{}, call, .{&self.waypoint_coord});
        }
    }

    fn getShipDistance(self: *const Ship) usize {
        return self.ship_coord.distanceFromOrigin();
    }

    fn parseInstruction(line: []const u8) !Instruction {
        return Instruction {
            .action = line[0],
            .value = try std.fmt.parseInt(isize, line[1..], 10),
        };
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var ship1 = Ship.init(1, 0, true);
    var ship2 = Ship.init(10, -1, false);
    while (problem.line()) |line| {
        const instruction = try Ship.parseInstruction(line);
        ship1.go(instruction);
        ship2.go(instruction);
    }

    return aoc.Solution { .p1 = ship1.getShipDistance(), .p2 = ship2.getShipDistance() };
}
