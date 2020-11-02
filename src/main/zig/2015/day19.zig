const aoc = @import("../aoc.zig");
const std = @import("std");
const Arena = std.heap.ArenaAllocator;
const Defab = std.StringHashMap([] const u8);
const Fab = aoc.StringMultimap([]const u8);
const MoleculeList = std.ArrayList([]const u8);
const MoleculeSet = std.StringHashMap(void);

pub fn run(problem: *aoc.Problem) !void {
    var fab = Fab.init(problem.allocator);
    defer fab.deinit();
    var defab = Defab.init(problem.allocator);
    defer defab.deinit();
    var medicine: []const u8 = undefined;
    while (problem.line()) |line| {
        if (line.len > 20) {
            medicine = line;
            break;
        }
        var tokens = std.mem.tokenize(line, " ");
        const key = tokens.next().?;
        _ = tokens.next().?;
        const value = tokens.next().?;

        try fab.put(key, value);
        try defab.putNoClobber(value, key);
    }

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

    // const fastestFabrication = blk : {
    //     var arena = Arena.init(problem.allocator); defer arena.deinit();
    //     var buf1 = MoleculeList.init(problem.allocator); defer buf1.deinit();
    //     var buf2 = MoleculeList.init(problem.allocator); defer buf2.deinit();
    //     var already_processed = MoleculeSet.init(problem.allocator); defer already_processed.deinit();

    //     var src = &buf1; var dest = &buf2;
    //     try src.append(medicine);

    //     var steps: usize = 1;
    //     while (true) : (steps += 1) {
    //         dest.items.len = 0;
    //         for (src.items) |molecule| {
    //             std.debug.warn("Process {}\n", .{molecule});
    //             if (std.mem.eql(u8, molecule, "e")) {
    //                 break :blk steps;
    //             }

    //             var idx: usize = 0;
    //             var end_idx: usize = undefined;
    //             while (idx < molecule.len) : (idx = end_idx) {
    //                 end_idx = nextAtom(molecule, idx) orelse unreachable;
    //                 const end_idx_2 = nextAtom(molecule, end_idx) orelse break;
    //                 if (std.mem.eql(u8, molecule[end_idx..end_idx_2], "Rn")) {
    //                     var this_end_idx = end_idx_2;
    //                     var attempts: u8 = 0;
    //                     while (attempts < 6) : (attempts += 1) {
    //                         const next_end_idx = nextAtom(molecule, this_end_idx) orelse break;
    //                         if (std.mem.eql(u8, molecule[this_end_idx..next_end_idx], "Ar")) {
    //                             if (defab.getValue(molecule[idx..next_end_idx])) |replacement| {
    //                                 const next_molecule = try replaceString(&arena, molecule, idx, next_end_idx, replacement);
    //                                 if (!already_processed.contains(next_molecule)) {
    //                                     std.debug.warn("\tWOW {}\n", .{next_molecule});
    //                                     try already_processed.putNoClobber(next_molecule, {});
    //                                     _ = try dest.append(next_molecule);
    //                                     break;
    //                                 }
    //                             }
    //                         }
    //                         this_end_idx = next_end_idx;
    //                     }
    //                 }
    //                 else if (molecule.len < 20) {
    //                     if (defab.getValue(molecule[idx..end_idx_2])) |replacement| {
    //                         const next_molecule = try replaceString(&arena, molecule, idx, end_idx_2, replacement);
    //                         if (!already_processed.contains(next_molecule)) {
    //                             std.debug.warn("\tPut {}\n", .{next_molecule});
    //                             try already_processed.putNoClobber(next_molecule, {});
    //                             _ = try dest.append(next_molecule);
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //         var tmp = src;
    //         src = dest;
    //         dest = tmp;
    //     }
    // };
    const fastestFabrication = "TODO";

    std.debug.warn("{}\n{}\n", .{uniquePossibilities, fastestFabrication});
}

fn nextAtom(molecule: []const u8, idx: usize) ?usize {
    if (idx == molecule.len) {
        return null;
    }
    const range: usize = if (idx == molecule.len - 1 or molecule[idx + 1] < 'a') 1 else 2;
    return idx + range;
}

fn replaceString(arena: *Arena, molecule: []const u8, idx: usize, end_idx: usize, replacement: []const u8) ![]u8 {
    return std.fmt.allocPrint(&arena.allocator, "{}{}{}", .{
        molecule[0..idx], replacement, molecule[end_idx..]
    });
}
