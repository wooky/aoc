const aoc = @import("../aoc.zig");
const std = @import("std");

const target_volume = 150;

const Params = struct {
    containers: []u8, combos: u16 = 0, smallest_container_qty: u8 = 255, smallest_container_combo: u8 = 255
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var containers: [21]u8 = undefined;
    containers[0] = 0;
    var container_idx: usize = 1;
    while (problem.line()) |line| {
        containers[container_idx] = try std.fmt.parseInt(u8, line, 10);
        container_idx += 1;
    }

    var params = Params { .containers = &containers };
    calcCombos(&params, 0, 0, 1);
    return problem.solution(params.combos, params.smallest_container_combo);
}

fn calcCombos(params: *Params, idx: usize, total_volume: u8, container_qty: u8) void {
    var new_volume: u8 = total_volume + params.containers[idx];
    if (new_volume == target_volume) {
        params.combos += 1;
        if (container_qty < params.smallest_container_qty) {
            params.smallest_container_qty = container_qty;
            params.smallest_container_combo = 1;
        }
        else if (container_qty == params.smallest_container_qty) {
            params.smallest_container_combo += 1;
        }
    }
    else if (new_volume < target_volume) {
        const new_qty = container_qty + 1;
        var new_idx = idx + 1;
        while (new_idx < params.containers.len) : (new_idx += 1) {
            calcCombos(params, new_idx, new_volume, new_qty);
        }
    }
}
