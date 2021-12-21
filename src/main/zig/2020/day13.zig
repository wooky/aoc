const aoc = @import("../aoc.zig");
const std = @import("std");

const Bus = struct { id: u16, offset: u8 };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var buses = std.ArrayList(Bus).init(problem.allocator);
    defer buses.deinit();

    const next_timestamp = try std.fmt.parseInt(u32, problem.line().?, 10);
    const ids = problem.line().?;
    var offset: u8 = 0;
    var tokens = std.mem.tokenize(u8, ids, ",");
    while (tokens.next()) |token| : (offset += 1) {
        if (token[0] == 'x') {
            continue;
        }
        const id = try std.fmt.parseInt(u16, token, 10);
        try buses.append(.{ .id = id, .offset = offset });
    }

    const res1 = blk: {
        var min_delay: u32 = std.math.maxInt(u32);
        var route: u16 = undefined;
        for (buses.items) |bus| {
            const delay = bus.id - (next_timestamp % bus.id);
            if (delay < min_delay) {
                min_delay = delay;
                route = bus.id;
            }
        }
        break :blk min_delay * route;
    };

    const res2 = blk: {
        var timestamp: usize = 0;
        var skip: usize = buses.items[0].id;
        var idx: usize = 1;
        while (idx < buses.items.len) : (idx += 1) {
            const bus = buses.items[idx];
            while ((timestamp + bus.offset) % bus.id != 0) : (timestamp += skip) {}
            skip *= bus.id;
        }
        break :blk timestamp;
    };

    return problem.solution(res1, res2);
}
