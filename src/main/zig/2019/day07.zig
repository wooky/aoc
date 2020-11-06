const aoc = @import("../aoc.zig");
const std = @import("std");
const Intcode = @import("intcode.zig");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();

    var amplifiers: [5]Intcode = undefined;
    for (amplifiers) |*amplifier, idx| {
        amplifier.* = try Intcode.init(problem.allocator, problem.input);
    }

    return aoc.Solution {
        .p1 = try getHighestOutput(&intcode, &[_]i8{0,1,2,3,4}),
        .p2 = try getHighestOutput(&intcode, &[_]i8{5,6,7,8,9}),
    };
}

fn getHighestOutput(intcode: *const Intcode, inputs: []const i8) !usize {
    var permutator = try aoc.Permutator(i8).init(intcode.allocator);
    defer permutator.deinit();
    try permutator.elements.appendSlice(inputs);

    var highest_output: Intcode.TapeElement = 0;
    while (permutator.next()) |phase_settings| {
        var amplifiers: [5]Intcode.State = undefined;
        for (amplifiers) |*amplifier| {
            amplifier.* = intcode.newState();
        }
        defer {
            for (amplifiers) |*amplifier| {
                amplifier.deinit();
            }
        }

        highest_output = std.math.max(highest_output, inner: {
            for (phase_settings) |phase_setting, idx| {
                try amplifiers[idx].inputs.append(phase_setting);
            }

            var last_output: Intcode.TapeElement = 0;
            while (true) {
                for (amplifiers) |*amplifier| {
                    try amplifier.inputs.append(last_output);
                    if (try intcode.run(amplifier)) |o| {
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
