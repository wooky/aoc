const aoc = @import("../aoc.zig");
const std = @import("std");

const SueComparison = enum { eq, gt, lt };
const SueComparer = struct {
    value: u8 = 0, real_comparer: SueComparison = SueComparison.eq,

    fn real_compare(self: SueComparer, other: u8) bool {
        return switch (self.real_comparer) {
            SueComparison.eq => other == self.value,
            SueComparison.gt => other > self.value,
            SueComparison.lt => other < self.value,
        };
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var target = std.StringHashMap(SueComparer).init(problem.allocator);
    defer target.deinit();
    try target.putNoClobber("children", SueComparer { .value = 3 });
    try target.putNoClobber("cats", SueComparer { .value = 7, .real_comparer = SueComparison.gt });
    try target.putNoClobber("samoyeds", SueComparer { .value = 2 });
    try target.putNoClobber("pomeranians", SueComparer { .value = 3, .real_comparer = SueComparison.lt });
    try target.putNoClobber("akitas", SueComparer {});
    try target.putNoClobber("vizslas", SueComparer {});
    try target.putNoClobber("goldfish", SueComparer { .value = 5, .real_comparer = SueComparison.lt });
    try target.putNoClobber("trees", SueComparer { .value = 3, .real_comparer = SueComparison.gt });
    try target.putNoClobber("cars", SueComparer { .value = 2 });
    try target.putNoClobber("perfumes", SueComparer { .value = 1 });

    var fake: []const u8 = undefined;
    var real: []const u8 = undefined;
    while (problem.line()) |line| {
        var is_fake = true;
        var is_real = true;
        var tokens = std.mem.tokenize(line, " :,");
        _ = tokens.next().?;
        const sue = tokens.next().?;
        while (tokens.next()) |key| {
            const comparer = target.get(key).?;
            const actual_value = try std.fmt.parseInt(u8, tokens.next().?, 10);
            if (comparer.value != actual_value) {
                is_fake = false;
            }
            if (!comparer.real_compare(actual_value)) {
                is_real = false;
            }
        }
        if (is_fake) {
            fake = sue;
        }
        else if (is_real) {
            real = sue;
        }
    }
    return aoc.Solution{ .p1 = try std.fmt.parseInt(usize, fake, 10), .p2 = try std.fmt.parseInt(usize, real, 10) };
}
