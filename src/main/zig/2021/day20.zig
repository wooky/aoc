const aoc = @import("../aoc.zig");
const std = @import("std");
const Map = std.AutoHashMap(aoc.Coord, void);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var image_enhancement_algorithm = problem.line().?;

    var map = Map.init(problem.allocator);
    defer map.deinit();
    var last_coord = aoc.Coord.init(.{0, 0});
    while (problem.line()) |line| {
        last_coord.col = 0;
        for (line) |c| {
            if (c == '#') {
                try map.putNoClobber(last_coord, {});
            }
            last_coord.col += 1;
        }
        last_coord.row += 1;
    }

    var two_iters: usize = undefined;
    const fifty_iters = blk: {
        var first_coord = aoc.Coord.init(.{-1, -1});
        var out_of_bounds_pixel: u1 = 0;
        var iterations: u8 = 0;
        while (iterations < 50) : ({
            iterations += 1;
            first_coord.row -= 1; first_coord.col -= 1;
            last_coord.row += 1; last_coord.col += 1;
            out_of_bounds_pixel ^= @boolToInt(image_enhancement_algorithm[0] == '#');
        }) {
            if (iterations == 2) {
                two_iters = map.count();
            }

            var new_map = Map.init(problem.allocator);
            var iter = aoc.CoordRangeIterator.init(first_coord, last_coord);
            while (iter.next()) |coord| {
                var iea_idx: u9 = 0;
                var neighbors = coord.neighbors(true);
                while (neighbors.next()) |neighbor| {
                    const pixel =
                        if (map.contains(neighbor))
                            1
                        else if (neighbor.row <= first_coord.row or neighbor.col <= first_coord.col or neighbor.row >= last_coord.row or neighbor.col >= last_coord.col)
                            out_of_bounds_pixel
                        else
                            0
                        ;
                    iea_idx = (iea_idx << 1) | pixel;
                }
                if (image_enhancement_algorithm[iea_idx] == '#') {
                    try new_map.putNoClobber(coord, {});
                }
            }
            map.deinit();
            map = new_map;
        }
        break :blk map.count();
    };

    return problem.solution(two_iters, fifty_iters);
}
