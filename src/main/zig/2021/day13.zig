const aoc = @import("../aoc.zig");
const std = @import("std");

const Fold = union(enum) { x: u16, y: u16 };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var dots = blk: {
        var dots = std.AutoHashMap(aoc.Coord2D, void).init(problem.allocator);
        const group = problem.group().?;
        var tokens = std.mem.tokenize(group, "\n,");
        while (tokens.next()) |x_str| {
            const y_str = tokens.next().?;
            try dots.put(aoc.Coord2D.init(.{
                try std.fmt.parseInt(u16, x_str, 10),
                try std.fmt.parseInt(u16, y_str, 10),
            }), {});
        }
        break :blk dots;
    };
    defer dots.deinit();

    var folds = blk: {
        var folds = std.ArrayList(Fold).init(problem.allocator);
        const group = problem.group().?;
        var tokens = std.mem.tokenize(group, "fold ang=\n");
        while (tokens.next()) |dir| {
            const line = try std.fmt.parseInt(u16, tokens.next().?, 10);
            try folds.append(switch (dir[0]) {
                'x' => Fold{ .x = line },
                'y' => Fold{ .y = line },
                else => unreachable
            });
        }
        break :blk folds;
    };
    defer folds.deinit();

    var one_fold_dot_count: usize = undefined;
    var screen_size: aoc.Coord2D = undefined;
    for (folds.items) |fold, idx| {
        switch (fold) {
            .x => |x| screen_size.x = x,
            .y => |y| screen_size.y = y,
        }
        var next_dots = std.AutoHashMap(aoc.Coord2D, void).init(problem.allocator);
        var iter = dots.keyIterator();
        while (iter.next()) |coord| {
            const x = if (std.meta.activeTag(fold) == .x and coord.x > fold.x) 2*fold.x - coord.x else coord.x;
            const y = if (std.meta.activeTag(fold) == .y and coord.y > fold.y) 2*fold.y - coord.y else coord.y;
            try next_dots.put(aoc.Coord2D.init(.{x, y}), {});
        }
        dots.deinit();
        dots = next_dots;
        
        if (idx == 0) {
            one_fold_dot_count = dots.count();
        }
    }

    var code = std.ArrayList(u8).init(problem.allocator);
    defer code.deinit();
    var coord = aoc.Coord2D.init(.{0, 0});
    while (coord.y < screen_size.y) : ({ coord.x = 0; coord.y += 1; }) {
        while (coord.x < screen_size.x) : (coord.x += 1) {
            try code.append(if (dots.contains(coord)) '#' else '.');
        }
        try code.append('\n');
    }

    return problem.solution(one_fold_dot_count, code.items);
}
