const aoc = @import("../aoc.zig");
const ada_bridge = @import("../ada_bridge.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
  return ada_bridge.run(problem, 2022, day);
}
