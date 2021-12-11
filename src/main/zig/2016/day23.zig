const aoc = @import("../aoc.zig");
const std = @import("std");
const Assembunny = @import("assembunny.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var computer = Assembunny.init(problem.allocator);
    defer computer.deinit();
    while (problem.line()) |line| {
        try computer.feed(line);
    }

    const s1 = try computer.setRegister("a", 7).runAndFetchRegisterA();
    const s2 = try computer.reset().setRegister("a", 12).runAndFetchRegisterA();
    return problem.solution(s1, s2);
}
