const aoc = @import("../aoc.zig");
const std = @import("std");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var amplifiers: [5]Intcode = undefined;
    for (amplifiers) |*amplifier, idx| {
        amplifier.* = try Intcode.init(problem.allocator, problem.input);
    }

    const solution = aoc.Solution {
        .p1 = try getHighestOutput(problem.allocator, &amplifiers, &[_]i8{0,1,2,3,4}),
        .p2 = try getHighestOutput(problem.allocator, &amplifiers, &[_]i8{5,6,7,8,9}),
    };

    for (amplifiers) |*amplifier| {
        amplifier.deinit();
    }
    return solution;
}

fn getHighestOutput(allocator: *std.mem.Allocator, amplifiers: []Intcode, inputs: []const i8) !usize {
    var permutator = try aoc.Permutator(i8).init(allocator);
    defer permutator.deinit();
    try permutator.elements.appendSlice(inputs);

    var highest_output: Intcode.TapeElement = 0;
    while (permutator.next()) |phase_settings| {
        highest_output = std.math.max(highest_output, inner: {
            for (phase_settings) |phase_setting, idx| {
                var amplifier = &amplifiers[idx];
                amplifier.reset();
                try amplifier.inputs.append(phase_setting);
            }

            var last_output: Intcode.TapeElement = 0;
            while (true) {
                for (amplifiers) |*amplifier| {
                    try amplifier.inputs.append(last_output);
                    if (try amplifier.run()) |o| {
                        last_output = o;
                    }
                    else {
                        break :inner last_output;
                    }
                }
            }
        });
    }
    return @intCast(usize, highest_output);
}
