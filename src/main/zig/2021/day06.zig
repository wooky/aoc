const aoc = @import("../aoc.zig");
const std = @import("std");
const RESPAWN_TIMER = 6;
const NEW_SPAWN_TIMER = 8;

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var fish = [_]usize { 0 } ** (NEW_SPAWN_TIMER + 1);
    var tokens = std.mem.tokenize(problem.input, ",");
    while (tokens.next()) |token| {
        const timer = try std.fmt.parseInt(u8, token, 10);
        fish[timer] += 1;
    }

    var fish80: usize = undefined;
    const fish256 = blk: {
        var day: u16 = 0;
        while (day < 256) : (day += 1) {
            const fish_respawn_count = fish[0];
            var cnt: u8 = 1;
            while (cnt <= NEW_SPAWN_TIMER) : (cnt += 1) {
                fish[cnt - 1] = fish[cnt];
            }
            fish[RESPAWN_TIMER] += fish_respawn_count;
            fish[NEW_SPAWN_TIMER] = fish_respawn_count;
            if (day == 79) {
                fish80 = countFish(&fish);
            }
        }
        break :blk countFish(&fish);
    };

    return problem.solution(fish80, fish256);
}

fn countFish(fish: []const usize) usize {
    var all_fish: usize = 0;
    for (fish) |f| {
        all_fish += f;
    }
    return all_fish;
}
