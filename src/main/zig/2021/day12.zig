const aoc = @import("../aoc.zig");
const std = @import("std");
const Paths = aoc.StringMultimap([]const u8);
const Visits = std.StringHashMap(void);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var paths = Paths.init(problem.allocator);
    defer paths.deinit();
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(u8, line, "-");
        const cave1 = tokens.next().?;
        const cave2 = tokens.next().?;
        try paths.put(cave1, cave2);
        try paths.put(cave2, cave1);
    }

    const total_paths_1 = countPaths("start", &Visits.init(problem.allocator), &paths, false, true);
    const total_paths_2 = countPaths("start", &Visits.init(problem.allocator), &paths, false, false);

    return problem.solution(total_paths_1, total_paths_2);
}

fn countPaths(current_cave: []const u8, current_visits: *const Visits, paths: *const Paths, start_visited: bool, some_small_cave_already_visited_twice: bool) anyerror!usize {
    if (std.mem.eql(u8, current_cave, "end")) {
        return 1;
    }
    if (std.mem.eql(u8, current_cave, "start") and start_visited) {
        return 0;
    }

    var next_some_small_cave_already_visited_twice = some_small_cave_already_visited_twice;
    if (current_cave[0] >= 'a' and current_visits.contains(current_cave)) {
        if (some_small_cave_already_visited_twice) {
            return 0;
        }
        next_some_small_cave_already_visited_twice = true;
    }
    var path_count: usize = 0;
    var next_visits = try current_visits.clone();
    defer next_visits.deinit();
    try next_visits.put(current_cave, {});
    for (paths.get(current_cave).?) |next_cave| {
        path_count += try countPaths(next_cave, &next_visits, paths, true, next_some_small_cave_already_visited_twice);
    }
    return path_count;
}
