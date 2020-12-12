const aoc = @import("../aoc.zig");
const std = @import("std");

const Ship = struct {
    const Instruction = struct {
        action: u8,
        value: isize,
    };

    const Coord = struct {
        x: isize = 0,
        y: isize = 0,

        fn rotate(self: *Coord, deg: isize) void {
            const cos: isize = switch (deg) {
                0 => 1,
                180, -180 => -1,
                else => 0
            };
            const sin: isize = switch (deg) {
                90, -270 => 1,
                270, -90 => -1,
                else => 0
            };
            const x = self.x;
            const y = self.y;

            self.x = x*cos - y*sin;
            self.y = x*sin + y*cos;
        }
    };

    ship_coord: Coord = Coord {},
    waypoint_coord: Coord,
    target_ship: bool,

    fn init(waypoint_x: isize, waypoint_y: isize, target_ship: bool) Ship {
        return Ship {
            .waypoint_coord = .{.x = waypoint_x, .y = waypoint_y},
            .target_ship = target_ship,
        };
    }

    fn go(self: *Ship, instruction: Instruction) void {
        const target_movement = if (self.target_ship) &self.ship_coord else &self.waypoint_coord;
        switch (instruction.action) {
            'N' => target_movement.y -= instruction.value,
            'S' => target_movement.y += instruction.value,
            'E' => target_movement.x += instruction.value,
            'W' => target_movement.x -= instruction.value,
            'L' => self.waypoint_coord.rotate(-instruction.value),
            'R' => self.waypoint_coord.rotate(instruction.value),
            'F' => {
                self.ship_coord.x += self.waypoint_coord.x * instruction.value;
                self.ship_coord.y += self.waypoint_coord.y * instruction.value;
            },
            else => unreachable
        }
    }

    fn getShipDistance(self: *const Ship) usize {
        return @intCast(usize, std.math.absCast(self.ship_coord.x) + std.math.absCast(self.ship_coord.y));
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
