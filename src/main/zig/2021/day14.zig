const aoc = @import("../aoc.zig");
const std = @import("std");
const PairCounts = std.StringHashMap(usize);
const AtomCounts = std.AutoHashMap(u8, usize);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    const polymer_template = problem.line().?;
    var pair_insertions = std.StringHashMap(u8).init(problem.allocator);
    defer pair_insertions.deinit();
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, " ->");
        try pair_insertions.put(tokens.next().?, tokens.next().?[0]);
    }

    var pair_counts = PairCounts.init(problem.allocator);
    defer pair_counts.deinit();
    var atom_counts = AtomCounts.init(problem.allocator);
    defer atom_counts.deinit();
    for (polymer_template[0..polymer_template.len - 1]) |atom, idx| {
        (try pair_counts.getOrPutValue(polymer_template[idx..idx + 2], 0)).value_ptr.* += 1;
        (try atom_counts.getOrPutValue(atom, 0)).value_ptr.* += 1;
    }
    (try atom_counts.getOrPutValue(polymer_template[polymer_template.len - 1], 0)).value_ptr.* += 1;

    var atom_diff_10: usize = undefined;
    const atom_diff_40 = blk: {
        var step: u8 = 0;
        while (step < 40) : (step += 1) {
            var new_pair_counts = PairCounts.init(problem.allocator);
            var iter = pair_counts.iterator();
            while (iter.next()) |old_pair_count| {
                const rule = pair_insertions.getEntry(old_pair_count.key_ptr.*).?;
                var buf: [2]u8 = undefined;
                buf[0] = old_pair_count.key_ptr.*[0]; buf[1] = rule.value_ptr.*;
                (try new_pair_counts.getOrPutValue(pair_insertions.getEntry(&buf).?.key_ptr.*, 0)).value_ptr.* += old_pair_count.value_ptr.*;
                buf[0] = rule.value_ptr.*; buf[1] = old_pair_count.key_ptr.*[1];
                (try new_pair_counts.getOrPutValue(pair_insertions.getEntry(&buf).?.key_ptr.*, 0)).value_ptr.* += old_pair_count.value_ptr.*;
                (try atom_counts.getOrPutValue(rule.value_ptr.*, 0)).value_ptr.* += old_pair_count.value_ptr.*;
            }
            pair_counts.deinit();
            pair_counts = new_pair_counts;

            if (step == 9) {
                atom_diff_10 = atomDiff(&atom_counts);
            }
        }
        break :blk atomDiff(&atom_counts);
    };

    return problem.solution(atom_diff_10, atom_diff_40);
}

fn atomDiff(atom_counts: *const AtomCounts) usize {
    var most_common: usize = 0;
    var least_common: usize = std.math.maxInt(usize);
    var iter = atom_counts.iterator();
    while (iter.next()) |atom_count| {
        if (atom_count.value_ptr.* > most_common) {
            most_common = atom_count.value_ptr.*;
        }
        if (atom_count.value_ptr.* < least_common) {
            least_common = atom_count.value_ptr.*;
        }
    }
    return most_common - least_common;
}
