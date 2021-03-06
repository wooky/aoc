const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var any_yeses: usize = 0;
    var all_yeses: usize = 0;
    while (problem.group()) |group| {
        // In Zig 0.7.0, clearRetainingCapacity() eats a lot of RAM
        // Hence why the hashmap is getting recreated on each iteration :(
        var yeses = std.AutoHashMap(u8, u8).init(problem.allocator);
        defer yeses.deinit();

        var people: u8 = 1;
        for (std.mem.trimRight(u8, group, "\n")) |c| {
            if (c == '\n') {
                people += 1;
            }
            else {
                const e = try yeses.getOrPutValue(c, 0);
                e.value += 1;
            }
        }
        
        var iter = yeses.iterator();
        while (iter.next()) |kv| {
            any_yeses += 1;
            if (kv.value == people) {
                all_yeses += 1;
            }
        }
    }

    return aoc.Solution { .p1 = any_yeses, .p2 = all_yeses };
}