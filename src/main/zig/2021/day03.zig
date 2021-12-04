const aoc = @import("../aoc.zig");
const std = @import("std");
const CODE_TYPE = u12;
const CODE_TYPE_SHIFT = u4;

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var arena = std.heap.ArenaAllocator.init(problem.allocator);
    defer arena.deinit();
    var codes = std.ArrayList(CODE_TYPE).init(&arena.allocator);
    while (problem.line()) |line| {
        try codes.append(try std.fmt.parseInt(CODE_TYPE, line, 2));
    }

    const s1 = blk: {
        var gamma: usize = 0;
        var i: CODE_TYPE_SHIFT = 0;
        while (i < std.meta.bitCount(CODE_TYPE)) : (i += 1) {
            gamma |= @as(usize, commonBit(codes.items, i)) << i;
        }
        const epsilon = std.math.maxInt(CODE_TYPE) & (~gamma);
        break :blk gamma * epsilon;
    };
    
    const s2 = blk: {
        const o2Rating = try calcRating(&arena.allocator, codes.items, false);
        const co2Rating = try calcRating(&arena.allocator, codes.items, true);
        break :blk o2Rating * co2Rating;
    };

    return problem.solution(s1, s2);
}

fn commonBit(codes: []const CODE_TYPE, pos: CODE_TYPE_SHIFT) u1 {
    var diff: i8 = 0;
    for (codes) |code| {
        const d: i8 = if (code & (@as(CODE_TYPE, 1) << pos) == 0) -1 else 1;
        diff += d;
    }
    return if (diff < 0) 0 else 1;
}

fn calcRating(allocator: *std.mem.Allocator, codes: []const CODE_TYPE, least_common: bool) !usize {
    var filteredCodes = codes;
    var i: CODE_TYPE_SHIFT = std.meta.bitCount(CODE_TYPE) - 1;
    while (true) : (i -= 1) {
        const bit = commonBit(filteredCodes, i) ^ @boolToInt(least_common);
        var newFilteredCodes = std.ArrayList(CODE_TYPE).init(allocator);
        for (filteredCodes) |code| {
            if ((code >> i) & 1 == bit) {
                try newFilteredCodes.append(code);
            }
        }
        filteredCodes = newFilteredCodes.items;
        if (filteredCodes.len == 1) {
            return filteredCodes[0];
        }
    }
}
