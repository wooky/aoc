const aoc = @import("../aoc.zig");
const std = @import("std");

const BagsHeld = struct { name: []const u8, qty: u8 };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var contains_in = aoc.StringMultimap([]const u8).init(problem.allocator);
    defer contains_in.deinit();
    var containing = aoc.StringMultimap(BagsHeld).init(problem.allocator);
    defer containing.deinit();

    while (problem.line()) |line| {
        var statements = std.mem.split(line, " bag");
        const contains = statements.next().?;
        while (statements.next()) |statement| {
            if (extractBag(statement)) |bag| {
                try contains_in.put(bag.name, contains);
                try containing.put(contains, bag);
            }
        }
    }

    const contains_shiny_gold = blk: {
        var to_process = std.ArrayList([]const u8).init(problem.allocator);
        defer to_process.deinit();
        var already_processed = std.StringHashMap(void).init(problem.allocator);
        defer already_processed.deinit();

        try to_process.append("shiny gold");
        while (to_process.popOrNull()) |p| {
            if (!already_processed.contains(p)) {
                try already_processed.putNoClobber(p, {});
                try to_process.appendSlice(contains_in.get(p) orelse continue);
            }
        }

        break :blk already_processed.count() - 1;
    };

    const shiny_gold_contents = countBags(containing, "shiny gold");

    return problem.solution(contains_shiny_gold, shiny_gold_contents);
}

fn extractBag(statement: []const u8) ?BagsHeld {
    if (statement[statement.len - 1] == '.' or std.mem.eql(u8, statement, "s contain no other")) {
        return null;
    }
    var idx = statement.len - 1;
    var saw_space = false;
    while (true) : (idx -= 1) {
        if (statement[idx] == ' ') {
            if (saw_space) {
                return BagsHeld { .name = statement[idx+1..], .qty = statement[idx-1] - '0' };
            }
            saw_space = true;
        }
    }
    unreachable;
}

fn countBags(containing: aoc.StringMultimap(BagsHeld), name: []const u8) usize {
    if (containing.get(name)) |bags| {
        var sum: usize = 0;
        for (bags) |bag| {
            sum += bag.qty + bag.qty * countBags(containing, bag.name);
        }
        return sum;
    }
    else {
        return 0;
    }
}
