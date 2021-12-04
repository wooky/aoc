const aoc = @import("../aoc.zig");
const std = @import("std");

const HappinessTable = aoc.StringTable(i16);
const HappinessPermutator = aoc.Permutator([]const u8);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var happiness = HappinessTable.init(problem.allocator);
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, " ");
        const p1 = tokens.next().?;
        _ = tokens.next().?;
        const factor: i8 = if (std.mem.eql(u8, tokens.next().?, "gain")) 1 else -1;
        const amount = try std.fmt.parseInt(i8, tokens.next().?, 10);
        _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?;
        const p2_dot = tokens.next().?;
        try happiness.put(p1, p2_dot[0..p2_dot.len-1], factor * amount);
    }

    var permutator = try HappinessPermutator.fromHashMapKeys(problem.allocator, HappinessTable, happiness);
    defer permutator.deinit();
    const excluded = get_max_happiness(&permutator, happiness);

    const me = "__ME__";
    permutator.reset();
    for (permutator.elements.items) |person| {
        try happiness.put(person, me, 0);
        try happiness.put(me, person, 0);
    }
    try permutator.elements.append(me);
    const included = get_max_happiness(&permutator, happiness);
    
    return problem.solution(excluded, included);
}

fn get_max_happiness(permutator: *HappinessPermutator, happiness: HappinessTable) u16 {
    var max_happiness: i16 = 0;
    while (permutator.next()) |seating| {
        var this_happiness: i16 = happiness.get(seating[0], seating[seating.len - 1]).? + happiness.get(seating[seating.len - 1], seating[0]).?;
        for (seating[0..seating.len - 1]) |p1, idx| {
            const p2 = seating[idx + 1];
            this_happiness += happiness.get(p1, p2).? + happiness.get(p2, p1).?;
        }
        max_happiness = std.math.max(max_happiness, this_happiness);
    }
    return @intCast(u16, max_happiness);
}
