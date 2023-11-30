const aoc = @import("../aoc.zig");
const std = @import("std");
const HeightMap = std.AutoHashMap(aoc.Coord, u8);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var heights = HeightMap.init(problem.allocator);
    defer heights.deinit();
    var row: isize = 0;
    while (problem.line()) |line| {
        var col: isize = 0;
        for (line) |h| {
            try heights.putNoClobber(aoc.Coord.init(.{ row, col }), h - '0');
            col += 1;
        }
        row += 1;
    }

    var risk_level: usize = 0;
    var basin_sizes = std.ArrayList(usize).init(problem.allocator);
    defer basin_sizes.deinit();
    var iter = heights.iterator();
    blk: while (iter.next()) |kv| {
        for (getNeighbors(kv.key_ptr.*)) |neighbor| {
            if (heights.get(neighbor)) |h| {
                if (h <= kv.value_ptr.*) {
                    continue :blk;
                }
            }
        }
        risk_level += kv.value_ptr.* + 1;
        try basin_sizes.append(try calcBasinSize(problem.allocator, &heights, kv.key_ptr.*));
    }

    std.sort.insertion(usize, basin_sizes.items, {}, comptime std.sort.desc(usize));
    const basin_product = basin_sizes.items[0] * basin_sizes.items[1] * basin_sizes.items[2];

    return problem.solution(risk_level, basin_product);
}

fn getNeighbors(coord: aoc.Coord) [4]aoc.Coord {
    return [_]aoc.Coord{
        coord.add(aoc.PredefinedCoord.UP),
        coord.add(aoc.PredefinedCoord.DOWN),
        coord.add(aoc.PredefinedCoord.LEFT),
        coord.add(aoc.PredefinedCoord.RIGHT),
    };
}

fn calcBasinSize(allocator: std.mem.Allocator, heights: *const HeightMap, low_point: aoc.Coord) !usize {
    var basin_points = std.AutoHashMap(aoc.Coord, void).init(allocator);
    defer basin_points.deinit();
    var unprocessed_points = std.ArrayList(aoc.Coord).init(allocator);
    defer unprocessed_points.deinit();
    try unprocessed_points.append(low_point);

    while (unprocessed_points.items.len != 0) {
        const coord = unprocessed_points.pop();
        if ((try basin_points.fetchPut(coord, {})) != null) {
            continue;
        }
        for (getNeighbors(coord)) |neighbor| {
            if (heights.get(neighbor)) |h| {
                if (h < 9) {
                    try unprocessed_points.append(neighbor);
                }
            }
        }
    }

    return basin_points.count();
}
