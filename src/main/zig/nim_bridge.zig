const aoc = @import("aoc.zig");
const nim = @import("aoc_nim");
const std = @import("std");

pub fn run(problem: *aoc.Problem, year: u16, day: u16) !aoc.Solution {
  const nim_solution = nim.run(problem.input, year, day);
  return aoc.Solution {
    .s1 = try problem.allocator.dupe(u8, std.mem.span(nim_solution.s1)),
    .s2 = try problem.allocator.dupe(u8, std.mem.span(nim_solution.s2))
  };
}
