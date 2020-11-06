const aoc = @import("../aoc.zig");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();

    try intcode.setMemory(1, 12);
    try intcode.setMemory(2, 2);
    _ = try intcode.run();
    const ans1 = intcode.getMemory(0);

    var noun: Intcode.TapeElement = 0;
    var verb: Intcode.TapeElement = undefined;
    outer: while (noun <= 99) : (noun += 1) {
        verb = 0;
        while (verb <= 99) : (verb += 1) {
            intcode.reset();
            try intcode.setMemory(1, noun);
            try intcode.setMemory(2, verb);
            _ = try intcode.run();
            if (intcode.getMemory(0) == 19690720) {
                break :outer;
            }
        }
    }
    const ans2 = 100 * noun + verb;

    return aoc.Solution{ .p1 = @intCast(usize, ans1), .p2 = @intCast(usize, ans2) };
}
