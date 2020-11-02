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

pub fn run(problem: *aoc.Problem) !void {
    var target = std.StringHashMap(SueComparer).init(problem.allocator);
    defer target.deinit();
    _ = try target.put("children", SueComparer { .value = 3 });
    _ = try target.put("cats", SueComparer { .value = 7, .real_comparer = SueComparison.gt });
    _ = try target.put("samoyeds", SueComparer { .value = 2 });
    _ = try target.put("pomeranians", SueComparer { .value = 3, .real_comparer = SueComparison.lt });
    _ = try target.put("akitas", SueComparer {});
    _ = try target.put("vizslas", SueComparer {});
    _ = try target.put("goldfish", SueComparer { .value = 5, .real_comparer = SueComparison.lt });
    _ = try target.put("trees", SueComparer { .value = 3, .real_comparer = SueComparison.gt });
    _ = try target.put("cars", SueComparer { .value = 2 });
    _ = try target.put("perfumes", SueComparer { .value = 1 });

    var fake: []const u8 = undefined;
    var real: []const u8 = undefined;
    while (problem.line()) |line| {
        var is_fake = true;
        var is_real = true;
        var tokens = std.mem.tokenize(line, " :,");
        _ = tokens.next().?;
        const sue = tokens.next().?;
        while (tokens.next()) |key| {
            const comparer = target.getValue(key).?;
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
    std.debug.warn("{}\n{}\n", .{fake, real});
}
