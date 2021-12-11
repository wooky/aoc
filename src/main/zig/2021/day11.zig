const aoc = @import("../aoc.zig");
const std = @import("std");

const Octopus = union(enum) {
    Normal: u8,     // power level
    Flashing: usize,   // step number
};
const Octopuses = std.AutoHashMap(aoc.Coord, Octopus);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var octopuses = Octopuses.init(problem.allocator);
    defer octopuses.deinit();
    var coord = aoc.Coord.init(.{0, 0});
    for (problem.input) |c| {
        if (c == '\n') {
            coord.row += 1;
            coord.col = 0;
        }
        else {
            try octopuses.putNoClobber(coord, Octopus{ .Normal = c - '0' });
            coord.col += 1;
        }
    }

    var total_flashes: usize = 0;
    var step: usize = 0;
    const first_step_simultaneous_flashes = blk: {
        while (true) : (step += 1) {
            var flashes: usize = 0;
            var iter = octopuses.iterator();
            while (iter.next()) |kv| {
                tickOctopus(kv, step, &octopuses, &flashes);
            }
            if (step < 100) {
                total_flashes += flashes;
            }
            if (flashes == 100) {
                break :blk step + 1;
            }
        }
        unreachable;
    };

    return problem.solution(total_flashes, first_step_simultaneous_flashes);
}

fn tickOctopus(kv_opt: ?Octopuses.Entry, step: usize, octopuses: *const Octopuses, flashes: *usize) void {
    if (kv_opt == null) {
        return;
    }
    var kv = kv_opt.?;

    switch (kv.value_ptr.*) {
        .Normal => |*power| {
            if (power.* == 9) {
                kv.value_ptr.* = Octopus{ .Flashing = step };
                flashes.* += 1;
                var neighbors = kv.key_ptr.neighbors();
                while (neighbors.next()) |neighbor| {
                    tickOctopus(octopuses.getEntry(neighbor), step, octopuses, flashes);
                }
            }
            else {
                power.* += 1;
            }
        },
        .Flashing => |flashing_step| {
            if (flashing_step != step) {
                kv.value_ptr.* = Octopus{ .Normal = 1 };
            }
        },
    }
}
