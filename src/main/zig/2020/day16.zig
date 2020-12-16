const aoc = @import("../aoc.zig");
const std = @import("std");

const Range = struct {
    from: u16,
    to: u16,

    fn contains(self: *const Range, what: u16) bool {
        return what >= self.from and what <= self.to;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    // var fields = aoc.StringMultimap(Range);
    // defer fields.deinit();
    var ranges = std.AutoHashMap(Range, void).init(problem.allocator);
    defer ranges.deinit();

    {
        const rules = problem.group().?;
        var lines = std.mem.tokenize(rules, "\n");
        while (lines.next()) |line| {
            const rule = line[0..std.mem.indexOf(u8, line, ":").?];
            var range_tokens = std.mem.tokenize(line[rule.len..], ": or");
            while (range_tokens.next()) |range_str| {
                const divider = std.mem.indexOf(u8, range_str, "-").?;
                try ranges.put(
                    Range {
                        .from = try std.fmt.parseInt(u16, range_str[0..divider], 10),
                        .to = try std.fmt.parseInt(u16, range_str[divider+1..], 10)
                    },
                    {}
                );
            }
        }
    }

    _ = problem.group().?;

    const res1 = blk: {
        var error_rate: usize = 0;
        const nearby_tix = problem.group().?;
        var all_tix = std.mem.tokenize(nearby_tix, "\n,");
        _ = all_tix.next().?;
        outer: while (all_tix.next()) |ticket_str| {
            const ticket = try std.fmt.parseInt(u16, ticket_str, 10);
            var iter = ranges.iterator();
            while (iter.next()) |kv| {
                if (kv.key.contains(ticket)) {
                    continue :outer;
                }
            }
            error_rate += ticket;
        }
        break :blk error_rate;
    };

    return aoc.Solution { .p1 = res1, .p2 = 0 };
}
