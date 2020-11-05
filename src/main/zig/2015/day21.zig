const aoc = @import("../aoc.zig");
const std = @import("std");

const Item = struct { cost: u16, atk: i8 = 0, def: i8 = 0 };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    const player_hp: u8 = 100;
    const weapons = [_]Item {
        .{ .cost = 8, .atk = 4 },
        .{ .cost = 10, .atk = 5 },
        .{ .cost = 25, .atk = 6 },
        .{ .cost = 40, .atk = 7 },
        .{ .cost = 74, .atk = 8 },
    };
    const armor = [_]Item {
        .{ .cost = 0 },
        .{ .cost = 13, .def = 1 },
        .{ .cost = 31, .def = 2 },
        .{ .cost = 53, .def = 3 },
        .{ .cost = 75, .def = 4 },
        .{ .cost = 102, .def = 5 },
    };
    const rings = [_]Item {
        .{ .cost = 0 },
        .{ .cost = 0 },
        .{ .cost = 25, .atk = 1 },
        .{ .cost = 50, .atk = 2 },
        .{ .cost = 100, .atk = 3 },
        .{ .cost = 20, .def = 1 },
        .{ .cost = 40, .def = 2 },
        .{ .cost = 80, .def = 3 },
    };

    var tokens = std.mem.tokenize(problem.input, ": \n");
    _ = tokens.next().?; _ = tokens.next().?;
    const boss_hp = try std.fmt.parseInt(u8, tokens.next().?, 10);
    _ = tokens.next().?;
    const boss_atk = try std.fmt.parseInt(i8, tokens.next().?, 10);
    _ = tokens.next().?;
    const boss_def = try std.fmt.parseInt(i8, tokens.next().?, 10);

    var cheapest_win: u16 = std.math.maxInt(u16);
    var expensive_lose: u16 = 0;
    for (weapons) |w| {
        for (armor) |a| {
            for (rings) |r1, r1i| {
                for (rings[r1i+1..]) |r2| {
                    const total_cost = w.cost + a.cost + r1.cost + r2.cost;

                    const player_atk = w.atk + r1.atk + r2.atk;
                    const player_def = a.def + r1.def + r2.def;

                    const player_hp_delta = @intCast(u8, std.math.max(boss_atk-player_def, 1));
                    const player_survives = player_hp / player_hp_delta - if (player_hp % player_hp_delta == 0) @as(u16, 1) else 0;

                    const boss_hp_delta = @intCast(u8, std.math.max(player_atk-boss_def, 1));
                    const boss_survives = boss_hp / boss_hp_delta - if (boss_hp % boss_hp_delta == 0) @as(u16, 1) else 0;

                    if (player_survives >= boss_survives) {
                        cheapest_win = std.math.min(cheapest_win, total_cost);
                    }
                    else {
                        expensive_lose = std.math.max(expensive_lose, total_cost);
                    }
                }
            }
        }
    }
    return aoc.Solution { .p1 = cheapest_win, .p2 = expensive_lose };
}
