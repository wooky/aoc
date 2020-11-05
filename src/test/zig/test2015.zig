const aoc = @import("aoc.zig");
const runner = @import("runner.zig");
const testing = @import("std").testing;

fn assertSolution(day: u16, p1: usize, p2: usize) !void {
    const actual_solution = try runner.run(day);
    testing.expectEqual(expected_solution, .{ .p1 = p1, .p2 = p2 });
}

test "2015 day 1" {
    assertSolution(1, 280, 1797);
}
