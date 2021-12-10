const aoc = @import("../aoc.zig");
const std = @import("std");

const Filesystem = struct {
    x: u8,
    y: u8,
    used: u16,
    avail: u16,
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var filesystems = std.ArrayList(Filesystem).init(problem.allocator);
    defer filesystems.deinit();

    _ = problem.line().?;
    _ = problem.line().?;
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, "/devgrino-xy T");
        const x = try std.fmt.parseInt(u8, tokens.next().?, 10);
        const y = try std.fmt.parseInt(u8, tokens.next().?, 10);
        _ = tokens.next().?;
        const used = try std.fmt.parseInt(u16, tokens.next().?, 10);
        const avail = try std.fmt.parseInt(u16, tokens.next().?, 10);
        try filesystems.append(.{ .x = x, .y = y, .used = used, .avail = avail });
    }

    var viable_pairs: usize = 0;
    for (filesystems.items) |*a| {
        if (a.used == 0) {
            continue;
        }
        for (filesystems.items) |*b| {
            if (a != b and a.used <= b.avail) {
                viable_pairs += 1;
            }
        }
    }

    return problem.solution(viable_pairs, 0);
}
