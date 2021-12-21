const aoc = @import("../aoc.zig");
const std = @import("std");
const Arena = std.heap.ArenaAllocator;
const Defab = std.StringHashMap([]const u8);
const DefabOrder = std.ArrayList([]const u8);
const Fab = aoc.StringMultimap([]const u8);
const MoleculeSet = std.StringHashMap(void);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var fab = Fab.init(problem.allocator);
    defer fab.deinit();
    var defab = Defab.init(problem.allocator);
    defer defab.deinit();
    var defab_order = DefabOrder.init(problem.allocator);
    defer defab_order.deinit();
    var medicine: []const u8 = undefined;

    while (problem.line()) |line| {
        if (line.len > 20) {
            medicine = line;
            break;
        }
        var tokens = std.mem.tokenize(u8, line, " ");
        const key = tokens.next().?;
        _ = tokens.next().?;
        const value = tokens.next().?;

        try fab.put(key, value);
        try defab.putNoClobber(value, key);
        try defab_order.append(value);
    }
    std.sort.sort([]const u8, defab_order.items, {}, compareLengths);

    const uniquePossibilities = blk: {
        var arena = Arena.init(problem.allocator);
        defer arena.deinit();
        var possibilities = MoleculeSet.init(problem.allocator);
        defer possibilities.deinit();
        var idx: usize = 0;
        var end_idx: usize = undefined;
        while (idx < medicine.len) : (idx = end_idx) {
            end_idx = nextAtom(medicine, idx) orelse unreachable;
            const key = medicine[idx..end_idx];
            const values = fab.get(key) orelse continue;
            for (values) |replacement| {
                _ = try possibilities.put(try replaceString(&arena, medicine, idx, end_idx, replacement), {});
            }
        }
        break :blk possibilities.count();
    };

    const fastestFabrication = blk: {
        var replacements: usize = 0;
        var buf: [512]u8 = undefined;
        std.mem.copy(u8, &buf, medicine);
        var buf_slice = buf[0..medicine.len];
        outer: while (!std.mem.eql(u8, buf_slice, "e")) {
            for (defab_order.items) |key| {
                const count = std.mem.count(u8, buf_slice, key);
                if (count != 0) {
                    replacements += count;
                    const value = defab.get(key).?;
                    _ = std.mem.replace(u8, buf_slice, key, value, &buf);
                    buf_slice.len -= (key.len - value.len) * count;
                    continue :outer;
                }
            }
            unreachable;
        }
        break :blk replacements;
    };

    return problem.solution(uniquePossibilities, fastestFabrication);
}

fn nextAtom(molecule: []const u8, idx: usize) ?usize {
    if (idx == molecule.len) {
        return null;
    }
    const range: usize = if (idx == molecule.len - 1 or molecule[idx + 1] < 'a') 1 else 2;
    return idx + range;
}

fn replaceString(arena: *Arena, molecule: []const u8, idx: usize, end_idx: usize, replacement: []const u8) ![]u8 {
    return std.fmt.allocPrint(arena.allocator(), "{s}{s}{s}", .{
        molecule[0..idx], replacement, molecule[end_idx..]
    });
}

fn compareLengths(_: void, a: []const u8, b: []const u8) bool {
    return a.len > b.len;
}
