const aoc = @import("aoc.zig");
const std = @import("std");

const AdaSolution = extern struct {
  s1: [*c]const u8,
  s2: [*c]const u8,
};
extern fn aoc_adainit() void;
extern fn aoc_adafinal() void;
extern fn run_ada(input: [*c]const u8, year: u16, day: u16) AdaSolution;

pub fn run(problem: *aoc.Problem, year: u16, day: u16) !aoc.Solution {
  aoc_adainit();
  defer aoc_adafinal();
  const ada_solution = run_ada(problem.input.ptr, year, day);
  return aoc.Solution {
    .s1 = try problem.allocator.dupe(u8, std.mem.span(ada_solution.s1)),
    .s2 = try problem.allocator.dupe(u8, std.mem.span(ada_solution.s2)),
  };
}
