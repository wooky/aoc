const aoc = @import("../aoc.zig");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();

    var blocks: usize = 0;
    var state = intcode.newState();
    defer state.deinit();
    while (true) {
        _ = (try intcode.run(&state)) orelse break;
        _ = (try intcode.run(&state)).?;
        const tile_id = (try intcode.run(&state)).?;
        if (tile_id == 2) {
            blocks += 1;
        }
    }

    return aoc.Solution{ .p1 = blocks, .p2 = 0 };
}
