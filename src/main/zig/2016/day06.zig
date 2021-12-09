const aoc = @import("../aoc.zig");
const std = @import("std");
const LINE_SIZE = 8;

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var lines = std.ArrayList([]const u8).init(problem.allocator);
    defer lines.deinit();
    while (problem.line()) |line| {
        try lines.append(line);
    }

    var loop: std.event.Loop = undefined;
    try loop.init();
    defer loop.deinit();
    var most_common: [LINE_SIZE]u8 = undefined;
    var least_common: [LINE_SIZE]u8 = undefined;
    var idx: u8 = 0;
    while (idx < LINE_SIZE) : (idx += 1) {
        try loop.runDetached(problem.allocator, doCodes, .{problem.allocator, lines.items, &most_common, &least_common, idx});
    }
    loop.run();

    return problem.solution(&most_common, &least_common);
}

// lmao multhreading is marginally faster
fn doCodes(allocator: *std.mem.Allocator, lines: [][]const u8, most_common: []u8, least_common: []u8, idx: u8) void {
    var frequency = std.AutoHashMap(u8, u8).init(allocator);
    defer frequency.deinit();
    for (lines) |line| {
        (frequency.getOrPutValue(line[idx], 0) catch unreachable).value_ptr.* += 1;
    }

    var iter = frequency.iterator();
    var most_common_entry = iter.next().?;
    var least_common_entry = most_common_entry;
    while (iter.next()) |entry| {
        if (entry.value_ptr.* > most_common_entry.value_ptr.*) {
            most_common_entry = entry;
        }
        else if (entry.value_ptr.* < least_common_entry.value_ptr.*) {
            least_common_entry = entry;
        }
    }
    most_common[idx] = most_common_entry.key_ptr.*;
    least_common[idx] = least_common_entry.key_ptr.*;
}
