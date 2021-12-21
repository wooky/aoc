const aoc = @import("../aoc.zig");
const std = @import("std");
const Risks = std.AutoHashMap(aoc.Coord, u8);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var risks1 = Risks.init(problem.allocator);
    defer risks1.deinit();
    var end_coord1 = aoc.Coord.init(.{0, 0});
    while (problem.line()) |line| {
        end_coord1.col = 0;
        for (line) |risk| {
            try risks1.put(end_coord1, risk - '0');
            end_coord1.col += 1;
        }
        end_coord1.row += 1;
    }
    
    var risks2 = Risks.init(problem.allocator);
    defer risks2.deinit();
    {
        var iter = risks1.iterator();
        while (iter.next()) |risk| {
            var row: u8 = 0;
            while (row < 5) : (row += 1) {
                var col: u8 = 0;
                while (col < 5) : (col += 1) {
                    try risks2.put(
                        risk.key_ptr.add(aoc.Coord.init(.{end_coord1.row * row, end_coord1.col * col})),
                        wrapRisk(risk.value_ptr.* + row + col)
                    );
                }
            }
        }
    }

    const end_coord2 = aoc.Coord.init(.{end_coord1.row * 5 - 1, end_coord1.col * 5 - 1});
    end_coord1.row -= 1; end_coord1.col -= 1;

    const min_risk1 = try aStar(&risks1, end_coord1);
    const min_risk2 = try aStar(&risks2, end_coord2);

    return problem.solution(min_risk1, min_risk2);
}

fn aStar(risks: *const Risks, end_coord: aoc.Coord) !usize {
    const Node = struct {
        const Self = @This();

        coord: aoc.Coord, g_score: usize,

        fn compare(_: void, a: Self, b: Self) std.math.Order {
            return std.math.order(a.g_score, b.g_score);
        }
    };

    var open_set = std.PriorityQueue(Node, void, Node.compare).init(risks.allocator, {});
    defer open_set.deinit();
    try open_set.add(.{ .coord = aoc.Coord.init(.{0, 0}), .g_score = 0 });

    var g_score = std.AutoHashMap(aoc.Coord, usize).init(risks.allocator);
    defer g_score.deinit();
    try g_score.put(aoc.Coord.init(.{0, 0}), 0);

    while (open_set.removeOrNull()) |current| {
        if (current.coord.equals(end_coord)) {
            return current.g_score;
        }

        for (&[_]aoc.Coord {
            current.coord.add(aoc.PredefinedCoord.UP),
            current.coord.add(aoc.PredefinedCoord.LEFT),
            current.coord.add(aoc.PredefinedCoord.RIGHT),
            current.coord.add(aoc.PredefinedCoord.DOWN),
        }) |neighbor| {
            const neighbor_score = risks.get(neighbor) orelse continue;
            const tentative_g_score = current.g_score + neighbor_score;
            if (tentative_g_score < g_score.get(neighbor) orelse std.math.maxInt(usize)) {
                try g_score.put(neighbor, tentative_g_score);
                try open_set.add(.{ .coord = neighbor, .g_score = tentative_g_score }); // not optimal
            }
        }
    }

    unreachable;
}

fn wrapRisk(risk: u8) u8 {
    return if (risk >= 10) risk - 9 else risk;
}
