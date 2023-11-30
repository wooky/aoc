const aoc = @import("../aoc.zig");
const std = @import("std");
const Intcode = @import("intcode.zig");

const Wall = std.AutoHashMap(aoc.Coord, u1);
const WallResult = struct { wall: Wall, range: aoc.CoordRange };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var intcode = try Intcode.init(problem.allocator, problem.input);
    defer intcode.deinit();

    const panels_painted = blk: {
        var res = try paintPanels(&intcode, 0);
        defer res.wall.deinit();
        break :blk res.wall.count();
    };

    const id = blk: {
        var res = try paintPanels(&intcode, 1);
        defer res.wall.deinit();

        var reg = std.ArrayList(u8).init(problem.allocator);

        var iter = res.range.iterator();
        while (iter.next()) |coord| {
            const chr = res.wall.get(coord) orelse 0;
            try reg.append(if (chr == 0) '.' else '#');
            if (coord.col == res.range.last.col) {
                try reg.append('\n');
            }
        }

        break :blk reg.toOwnedSlice();
    };

    return problem.solution(panels_painted, id);
}

fn paintPanels(intcode: *const Intcode, starting_tile: u1) !WallResult {
    var state = intcode.newState();
    defer state.deinit();
    var pos = aoc.PredefinedCoord.ORIGIN;
    var delta = aoc.PredefinedCoord.UP;
    var wall = Wall.init(intcode.allocator);
    try wall.putNoClobber(pos, starting_tile);
    var range = aoc.CoordRange.init();

    while (true) {
        const input = wall.get(pos) orelse 0;
        try state.inputs.append(input);
        const color = (try intcode.run(&state)) orelse break;
        _ = try wall.put(pos, @as(u1, @intCast(color)));
        range.amend(pos);

        switch ((try intcode.run(&state)).?) {
            0 => delta.mutRotate90DegreesCounterclockwise(),
            1 => delta.mutRotate90DegreesClockwise(),
            else => unreachable,
        }
        pos.mutAdd(delta);
    }

    return WallResult{ .wall = wall, .range = range };
}
