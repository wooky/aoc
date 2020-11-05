const aoc = @import("aoc");
const runner = @import("runner");
const testing = @import("std").testing;

fn assertSolution(day: u16, p1: usize, p2: usize) !void {
    var problem = try aoc.Problem.init(2015, day, testing.allocator);
    const actual_solution = try runner.run(&problem, 2015, day);
    testing.expectEqual(aoc.Solution{ .p1 = p1, .p2 = p2 }, actual_solution);
    problem.deinit();
    try testing.allocator_instance.validate();
}

test "2015 day 1" { try assertSolution(1, 280, 1797); }
test "2015 day 2" {try assertSolution(2, 1598415, 3812909); }
test "2015 day 3" { try assertSolution(3, 2565, 2639); }
test "2015 day 4" {    try assertSolution(4, 117946, 3938038); }
test "2015 day 5" { try assertSolution(5, 255, 55); }
test "2015 day 6" { try assertSolution(6, 400410, 15343601); }
test "2015 day 7" { try assertSolution(7, 3176, 14710); }
test "2015 day 8" { try assertSolution(8, 1350, 2085); }
test "2015 day 9" { try assertSolution(9, 117, 909); }
test "2015 day 10" { try assertSolution(10, 492982, 6989950); }
test "2015 day 12" { try assertSolution(12, 156366, 96852); }
test "2015 day 13" { try assertSolution(13, 709, 668); }
test "2015 day 14" { try assertSolution(14, 2696, 1084); }
test "2015 day 15" { try assertSolution(15, 222870, 117936); }
test "2015 day 16" { try assertSolution(16, 40, 241); }
test "2015 day 17" { try assertSolution(17, 654, 57); }
test "2015 day 18" { try assertSolution(18, 814, 924); }
test "2015 day 19" { try assertSolution(19, 535, 0); }
test "2015 day 21" { try assertSolution(21, 111, 88); }
