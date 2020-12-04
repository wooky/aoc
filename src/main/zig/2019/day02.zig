const aoc = @import("../aoc.zig");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();
    
    const res1 = blk: {
        var state = intcode.newState();
        defer state.deinit();
        try state.memory.putNoClobber(1, 12);
        try state.memory.putNoClobber(2, 2);
        _ = try intcode.run(&state);
        break :blk @intCast(usize, state.memory.get(0).?);
    };

    const res2 = blk: {
        var noun: Intcode.TapeElement = 0;
        var verb: Intcode.TapeElement = undefined;
        while (noun <= 99) : (noun += 1) {
            verb = 0;
            while (verb <= 99) : (verb += 1) {
                var state = intcode.newState();
                try state.memory.putNoClobber(1, noun);
                try state.memory.putNoClobber(2, verb);
                _ = try intcode.run(&state);
                if (state.memory.get(0).? == 19690720) {
                    break :blk @intCast(usize, 100 * noun + verb);
                }
            }
        }
        unreachable;
    };

    return aoc.Solution{ .p1 = res1, .p2 = res2 };
}
