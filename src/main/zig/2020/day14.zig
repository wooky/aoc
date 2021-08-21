const aoc = @import("../aoc.zig");
const std = @import("std");
const Memory = std.AutoHashMap(usize, usize);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var mem1 = Memory.init(problem.allocator); defer mem1.deinit();
    var mem2 = Memory.init(problem.allocator); defer mem2.deinit();
    var mask: []const u8 = undefined;
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, "= ");
        const lhs = tokens.next().?;
        const rhs = tokens.next().?;

        if (std.mem.eql(u8, lhs, "mask")) {
            mask = rhs;
        }
        else {
            const address = try std.fmt.parseInt(usize, lhs[4..std.mem.indexOf(u8, lhs, "]").?], 10);
            const value = try std.fmt.parseInt(usize, rhs, 10);

            var value1: usize = 0;
            var address2: usize = 0;
            var address_masks = std.ArrayList(usize).init(problem.allocator);
            defer address_masks.deinit();
            for (mask) |bit, idx| {
                const res_mask = @intCast(usize, 1) << @intCast(u6, mask.len - idx - 1);
                switch (bit) {
                    '0' => address2 |= address & res_mask,
                    '1' => { value1 |= res_mask; address2 |= res_mask; },
                    'X' => { value1 |= value & res_mask; try address_masks.append(res_mask); },
                    else => unreachable
                }
            }

            try mem1.put(address, value1);
            try putFloatingAddresses(address2, value, &mem2, address_masks.items);
        }
    }

    return aoc.Solution { .p1 = sumMemory(&mem1), .p2 = sumMemory(&mem2) };
}

fn putFloatingAddresses(address: usize, value: usize, mem: *Memory, address_masks: []const usize) anyerror!void {
    if (address_masks.len == 0) {
        try mem.put(address, value);
    }
    else {
        const mask = address_masks[0];
        try putFloatingAddresses(address, value, mem, address_masks[1..]);
        try putFloatingAddresses(address | mask, value, mem, address_masks[1..]);
    }
}

fn sumMemory(mem: *const Memory) usize {
    var res: usize = 0;
    var iter = mem.iterator();
    while (iter.next()) |kv| {
        res += kv.value_ptr.*;
    }
    return res;
}
