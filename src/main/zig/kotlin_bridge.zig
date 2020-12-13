const aoc = @import("aoc.zig");

const kotlin = @cImport(
    @cInclude("libaoc_api.h")
);

pub fn run(comptime year: []const u8, comptime day: []const u8, problem: *aoc.Problem) aoc.Solution {
    comptime const year_field = "aoc" ++ year;
    comptime const day_field = "run" ++ day;

    const symbols = kotlin.libaoc_symbols();
    const kotlin_aoc = symbols.*.kotlin.root.ca.yakov.aoc;
    const path = @field(@field(kotlin_aoc, year_field), day_field);
    const kotlin_solution = path.?(problem.input);
    const solution = aoc.Solution {
        .p1 = kotlin_aoc.Solution.get_p1.?(kotlin_solution),
        .p2 = kotlin_aoc.Solution.get_p2.?(kotlin_solution),
    };
    symbols.*.DisposeStablePointer.?(kotlin_solution.pinned);
    return solution;
}
