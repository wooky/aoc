const aoc = @import("../aoc.zig");
const std = @import("std");

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var unoccupied_seats = blk: {
        var set = std.AutoHashMap(u16, void).init(problem.allocator);
        var seat_id: u16 = 128;
        while (seat_id < 128 * 127) : (seat_id += 1) {
            try set.putNoClobber(seat_id, {});
        }
        break :blk set;
    };
    defer unoccupied_seats.deinit();

    var highest_seatid: u16 = 0;
    while (problem.line()) |line| {
        const row = decode(line[0..7], 'F');
        const col = decode(line[7..10], 'L');
        const seat_id = row * 8 + col;
        highest_seatid = std.math.max(highest_seatid, seat_id);
        _ = unoccupied_seats.remove(seat_id);
    }

    var iterator = unoccupied_seats.iterator();
    const target_id = blk: {
        while (iterator.next()) |kv| {
            const k = kv.key;
            if (!unoccupied_seats.contains(k - 1) and !unoccupied_seats.contains(k + 1)) {
                break :blk k;
            }
        }
        unreachable;
    };

    return aoc.Solution { .p1 = highest_seatid, .p2 = target_id };
}

fn decode(str: []const u8, upper_half_char: u8) u16 {
    var low: u8 = 0;
    var high = (@intCast(u8, 1) << @intCast(u3, str.len)) - 1;
    var delta = @intCast(u8, 1) << (@intCast(u3, str.len) - 1);
    for (str) |c| {
        if (c == upper_half_char) {
            high -= delta;
        }
        else {
            low += delta;
        }
        delta /= 2;
    }
    return low;
}
