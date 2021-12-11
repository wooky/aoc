const aoc = @import("../aoc.zig");
const std = @import("std");
const Visitations = std.bit_set.StaticBitSet(7);

const Node = struct {
    coord: aoc.Coord,
    g_score: usize,

    fn compare(a: *Node, b: *Node) std.math.Order {
        return std.math.order(a.g_score, b.g_score);
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {

}

fn dijkstraThread(allocator: *std.mem.Allocator, map: [][]const u8, start_node: Node, const_visitations: Visitations) void {

}

fn dijkstra(allocator: *std.mem.Allocator, map: [][]const u8, start_node: Node, const_visitations: Visitations) !usize {
    var open_set = std.PriorityQueue(Node).init(allocator, Node.compare);
    defer open_set.deinit();
    try open_set.add(start_node);

    var g_scores = std.AutoHashMap(aoc.Coord, usize).init(allocator);
    defer g_scores.deinit();
    try g_scores.putNoClobber(start_node.coord, 0);

    var visitations = const_visitations;
    while (open_set.removeOrNull()) |node| {
        const point = map[node.coord.row][node.coord.col];
        if (point != '.' and point != '0') {
            const target: u3 = point - '0' - 1;
            if (visitations.isSet(target)) {
                if (visitations.count() == 1) {
                    return node.g_score;
                }
            }

            var new_visitations = visitations;
            new_visitations.unset(target);
            // TODO spawn dijkstra(allocator, map, node, new_visitations)
        }

        for (.{
            node.coord.add(aoc.PredefinedCoord.UP),
            node.coord.add(aoc.PredefinedCoord.LEFT),
            node.coord.add(aoc.PredefinedCoord.RIGHT),
            node.coord.add(aoc.PredefinedCoord.DOWN),
        }) |neighbor| {
            if (map[neighbor.row][neighbor.col] == '#') {
                continue;
            }
            const tentative_g_score = node.g_score + 1;
            if (tentative_g_score < g_scores.get(neighbor) orelse std.math.maxInt(usize)) {
                try g_scores.put(neighbor, tentative_g_score);
                try open_set.add(.{ .coord = neighbor, .g_score = tentative_g_score });
            }
        }
    }

    unreachable;
}
