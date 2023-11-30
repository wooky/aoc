const aoc = @import("../aoc.zig");
const std = @import("std");

const Intersector = struct {
    coords_encountered: std.AutoHashMap(aoc.Coord2D, void),
    coords_crossed: std.AutoHashMap(aoc.Coord2D, void),

    fn init(allocator: std.mem.Allocator) Intersector {
        return .{
            .coords_encountered = std.AutoHashMap(aoc.Coord2D, void).init(allocator),
            .coords_crossed = std.AutoHashMap(aoc.Coord2D, void).init(allocator),
        };
    }

    fn deinit(self: *Intersector) void {
        self.coords_encountered.deinit();
        self.coords_crossed.deinit();
    }

    fn process(self: *Intersector, from: aoc.Coord2D, to: aoc.Coord2D) !void {
        const delta = aoc.Coord2D.init(.{ cmp(from.x, to.x), cmp(from.y, to.y) });
        var curr = from;
        while (true) {
            if ((try self.coords_encountered.fetchPut(curr, {})) != null) {
                try self.coords_crossed.put(curr, {});
            }
            if (curr.equals(to)) {
                break;
            }
            curr.mutAdd(delta);
        }
    }

    inline fn cmp(b: isize, a: isize) i8 {
        return if (a < b) -1 else if (a > b) @as(i8, 1) else 0;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var straight_intersector = Intersector.init(problem.allocator);
    defer straight_intersector.deinit();
    var diag_intersector = Intersector.init(problem.allocator);
    defer diag_intersector.deinit();

    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(u8, line, " ,->");
        const from = aoc.Coord2D.init(.{
            try std.fmt.parseInt(isize, tokens.next().?, 10),
            try std.fmt.parseInt(isize, tokens.next().?, 10),
        });
        const to = aoc.Coord2D.init(.{
            try std.fmt.parseInt(isize, tokens.next().?, 10),
            try std.fmt.parseInt(isize, tokens.next().?, 10),
        });

        if (from.x == to.x or from.y == to.y) {
            try straight_intersector.process(from, to);
        }
        try diag_intersector.process(from, to);
    }

    return problem.solution(straight_intersector.coords_crossed.count(), diag_intersector.coords_crossed.count());
}
