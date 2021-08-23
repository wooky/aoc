const aoc = @import("../aoc.zig");
const std = @import("std");
const Intcode = @import("intcode.zig");

const Wall = std.AutoHashMap(aoc.Coord, u1);
const WallResult = struct {
    wall: Wall, topleft: aoc.Coord, bottomright: aoc.Coord
};

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
        const dimensions = res.bottomright.subtract(res.topleft);

        var reg = try problem.allocator.alloc(u8, @intCast(usize, (dimensions.col + 2) * (dimensions.row + 1)));
        
        var idx: usize = 0;
        var row = res.topleft.row;
        while (row <= res.bottomright.row) : ({row += 1; idx += 1;}) {
            var col = res.topleft.col;
            while (col <= res.bottomright.col) : ({col += 1; idx += 1;}) {
                const chr = res.wall.get(aoc.Coord.fromRowCol(row, col)) orelse 0;
                reg[idx] = if (chr == 0) '.' else '#';
            }
            reg[idx] = '\n';
        }

        break :blk reg;
    };

    return aoc.Solution{ .p1 = panels_painted, .p2 = undefined, .s2 = id };
}

fn paintPanels(intcode: *const Intcode, starting_tile: u1) !WallResult {
    var state = intcode.newState();
    defer state.deinit();
    var pos = aoc.Coord.Predefined.ORIGIN;
    var delta = aoc.Coord.Predefined.UP;
    var wall = Wall.init(intcode.allocator);
    try wall.putNoClobber(pos, starting_tile);
    var topleft = aoc.Coord.Predefined.ORIGIN;
    var bottomright = aoc.Coord.Predefined.ORIGIN;

    while (true) {
        const input = wall.get(pos) orelse 0;
        try state.inputs.append(input);
        const color = (try intcode.run(&state)) orelse break;
        _ = try wall.put(pos, @intCast(u1, color));

        topleft.row = std.math.min(topleft.row, pos.row);
        topleft.col = std.math.min(topleft.col, pos.col);
        bottomright.row = std.math.max(bottomright.row, pos.row);
        bottomright.col = std.math.max(bottomright.col, pos.col);

        switch ((try intcode.run(&state)).?) {
            0 => delta.mutRotate90DegreesCounterclockwise(),
            1 => delta.mutRotate90DegreesClockwise(),
            else => unreachable
        }
        pos.mutAdd(delta);
    }

    return WallResult{ .wall = wall, .topleft = topleft, .bottomright = bottomright };
}
