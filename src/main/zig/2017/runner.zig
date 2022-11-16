const aoc = @import("../aoc.zig");
const nim_bridge = @import("../nim_bridge.zig");

pub fn run(problem: *aoc.Problem, day: u16) !aoc.Solution {
  return nim_bridge.run(problem, 2017, day);
}
