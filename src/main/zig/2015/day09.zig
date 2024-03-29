const aoc = @import("../aoc.zig");
const std = @import("std");

const PathMap = aoc.StringTable(u8);
const Datum = struct { locations: [][]const u8, paths: *PathMap, min_dist: u16, max_dist: u16 };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var paths = PathMap.init(problem.allocator);
    defer paths.deinit();

    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(u8, line, " ");
        const loc1 = tokens.next().?;
        _ = tokens.next().?;
        const loc2 = tokens.next().?;
        _ = tokens.next().?;
        const distance = try std.fmt.parseInt(u8, tokens.next().?, 10);

        _ = try paths.put(loc1, loc2, distance);
        _ = try paths.put(loc2, loc1, distance);
    }

    var min_dist: u16 = std.math.maxInt(u16);
    var max_dist: u16 = std.math.minInt(u16);
    var permutator = try aoc.Permutator([]const u8).fromHashMapKeys(problem.allocator, PathMap, paths);
    defer permutator.deinit();
    while (permutator.next()) |locations| {
        var dist: u16 = 0;
        for (locations[1..], 0..) |_, idx| {
            dist += paths.get(locations[idx], locations[idx + 1]).?;
        }
        min_dist = @min(min_dist, dist);
        max_dist = @max(max_dist, dist);
    }
    return problem.solution(min_dist, max_dist);
}
