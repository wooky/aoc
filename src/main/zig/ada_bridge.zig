const aoc = @import("aoc.zig");
const std = @import("std");

extern fn aoc_adainit() void;
extern fn aoc_adafinal() void;
extern fn run_ada(year: u16, day: u16) void;

pub fn run(problem: *aoc.Problem, year: u16, day: u16) !aoc.Solution {
  // TODO
  aoc_adainit();
  run_ada(year, day);
  aoc_adafinal();
  return aoc.Solution {
    .s1 = try problem.allocator.alloc(u8, 0),
    .s2 = try problem.allocator.alloc(u8, 0),
  };
}
