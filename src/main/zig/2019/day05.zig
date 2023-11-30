const aoc = @import("../aoc.zig");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();

    const res1 = blk: {
        var state = intcode.newState();
        defer state.deinit();
        try state.inputs.append(1);
        while (true) {
            const diagnostic = (try intcode.run(&state)).?;
            if (diagnostic != 0) {
                break :blk @as(usize, @intCast(diagnostic));
            }
        }
    };

    const res2 = blk: {
        var state = intcode.newState();
        defer state.deinit();
        try state.inputs.append(5);
        break :blk @as(usize, @intCast((try intcode.run(&state)).?));
    };

    return problem.solution(res1, res2);
}
