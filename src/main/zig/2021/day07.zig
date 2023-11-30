const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var positions = std.AutoHashMap(u16, u8).init(problem.allocator);
    defer positions.deinit();
    var pos_left: u16 = std.math.maxInt(u16);
    var pos_right: u16 = 0;

    var tokens = std.mem.tokenize(u8, problem.input, ",");
    while (tokens.next()) |token| {
        const pos = try std.fmt.parseInt(u16, token, 10);
        var res = try positions.getOrPut(pos);
        if (res.found_existing) {
            res.value_ptr.* += 1;
        } else {
            res.value_ptr.* = 1;
            if (pos < pos_left) {
                pos_left = pos;
            }
            if (pos > pos_right) {
                pos_right = pos;
            }
        }
    }

    const constant_fuel = calcFuel(&positions, pos_left, pos_right, constantFuel);
    const pyramidal_fuel = calcFuel(&positions, pos_left, pos_right, pyramidalFuel);

    return problem.solution(constant_fuel, pyramidal_fuel);
}

fn calcFuel(positions: *const std.AutoHashMap(u16, u8), pos_left: u16, pos_right: u16, fuelModifier: *const fn (usize) usize) usize {
    var min_fuel: usize = std.math.maxInt(usize);
    var target: isize = pos_left;
    while (target <= pos_right) : (target += 1) {
        var curr_fuel: usize = 0;
        var iter = positions.iterator();
        while (iter.next()) |kv| {
            curr_fuel += fuelModifier(std.math.absCast(target - kv.key_ptr.*)) * kv.value_ptr.*;
        }
        if (curr_fuel < min_fuel) {
            min_fuel = curr_fuel;
        }
    }
    return min_fuel;
}

fn constantFuel(steps: usize) usize {
    return steps;
}

fn pyramidalFuel(steps: usize) usize {
    return steps * (steps + 1) / 2;
}
