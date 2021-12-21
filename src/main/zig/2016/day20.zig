const aoc = @import("../aoc.zig");
const std = @import("std");

const IpRange = struct {
    from: u32,
    to: u32,
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var ranges = std.ArrayList(IpRange).init(problem.allocator);
    defer ranges.deinit();
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(u8, line, "-");
        try ranges.append(.{
            .from = try std.fmt.parseInt(u32, tokens.next().?, 10),
            .to = try std.fmt.parseInt(u32, tokens.next().?, 10),
        });
    }

    var ip: u32 = 0;
    var lowest_allowed_ip: ?u32 = null;
    var allowed_ip_count: u32 = 0;
    blk: while (true) {
        for (ranges.items) |range| {
            if (ip >= range.from and ip <= range.to) {
                if (range.to == std.math.maxInt(u32)) {
                    break :blk;
                }
                ip = range.to + 1;
                continue :blk;
            }
        }
        if (lowest_allowed_ip == null) {
            lowest_allowed_ip = ip;
        }
        allowed_ip_count += 1;
        ip += 1;
    }

    return problem.solution(lowest_allowed_ip.?, allowed_ip_count);
}
