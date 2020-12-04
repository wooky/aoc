const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var res1: usize = 0;
    var res2: usize = 0;
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, "- :");
        const first = try std.fmt.parseInt(u8, tokens.next().?, 10);
        const last = try std.fmt.parseInt(u8, tokens.next().?, 10);
        const needle = tokens.next().?;
        const haystack = tokens.next().?;

        const count = std.mem.count(u8, haystack, needle);
        if (count >= first and count <= last) {
            res1 += 1;
        }

        const has_first = (first - 1 < haystack.len) and (haystack[first - 1] == needle[0]);
        const has_last = (last - 1 < haystack.len) and (haystack[last - 1] == needle[0]);
        if (has_first != has_last) {
            res2 += 1;
        }
    }

    return aoc.Solution { .p1 = res1, .p2 = res2 };
}
