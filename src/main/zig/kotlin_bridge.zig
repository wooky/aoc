const aoc = @import("aoc.zig");
const std = @import("std");

const kotlin = @cImport(
    @cInclude("libaoc_api.h")
);

pub fn run(comptime year: []const u8, comptime day: []const u8, problem: *aoc.Problem) !aoc.Solution {
    comptime const year_field = "aoc" ++ year;
    comptime const day_field = "run" ++ day;

    const symbols = kotlin.libaoc_symbols();
    const kotlin_aoc = symbols.*.kotlin.root.ca.yakov.aoc;
    const path = @field(@field(kotlin_aoc, year_field), day_field);
    const kotlin_solution = path.?(problem.input);
    var solution = aoc.Solution { .s1 = undefined, .s2 = undefined };
    try populateSolution(problem.allocator, &solution, kotlin_solution, kotlin_aoc, "1");
    try populateSolution(problem.allocator, &solution, kotlin_solution, kotlin_aoc, "2");
    symbols.*.DisposeStablePointer.?(kotlin_solution.pinned);
    return solution;
}

// TODO purge the get_p functions
fn populateSolution(allocator: *std.mem.Allocator, solution: *aoc.Solution, kotlin_solution: anytype, kotlin_aoc: anytype, comptime field: []const u8) !void {
    comptime const get_s = "get_s" ++ field;
    const s =
        if (@field(kotlin_aoc.Solution, get_s).?(kotlin_solution)) |s|
            try allocator.dupe(u8, s[0..std.mem.len(s)])
        else
            try std.fmt.allocPrint(allocator, "{}", .{@field(kotlin_aoc.Solution, "get_p" ++ field).?(kotlin_solution)})
        ;
    @field(solution, "s" ++ field) = s;
}
