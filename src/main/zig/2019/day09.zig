const aoc = @import("../aoc.zig");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();

    const res1 = blk: {
        var state = intcode.newState();
        defer state.deinit();
        try state.inputs.append(1);

        var last_output: Intcode.TapeElement = undefined;
        while (true) {
            const output = try intcode.run(&state);
            if (output) |o| {
                last_output = o;
            } else {
                break :blk @as(usize, @intCast(last_output));
            }
        }
    };

    const res2 = blk: {
        var state = intcode.newState();
        defer state.deinit();
        try state.inputs.append(2);
        break :blk @as(usize, @intCast((try intcode.run(&state)).?));
    };

    return problem.solution(res1, res2);
}
