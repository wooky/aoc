const aoc = @import("../aoc.zig");
const std = @import("std");
const Intcode = @import("intcode.zig");

const Coord = struct { x: i16 = 0, y: i16 = 0 };
const Wall = std.AutoHashMap(Coord, u1);
const WallResult = struct {
    wall: Wall, topleft: Coord, bottomright: Coord
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
        const width = res.bottomright.x - res.topleft.x + 1;
        const height = res.bottomright.y - res.topleft.y + 1;

        var reg = try problem.allocator.alloc(u8, @intCast(usize, (width + 1) * height));
        
        var idx: usize = 0;
        var y = res.topleft.y;
        while (y <= res.bottomright.y) : ({y += 1; idx += 1;}) {
            var x = res.topleft.x;
            while (x <= res.bottomright.x) : ({x += 1; idx += 1;}) {
                const chr = res.wall.get(.{ .x = x, .y = y }) orelse 0;
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
    var pos = Coord {};
    var delta = Coord { .x = 0, .y = -1 };
    var wall = std.AutoHashMap(Coord, u1).init(intcode.allocator);
    try wall.putNoClobber(pos, starting_tile);
    var topleft = Coord {};
    var bottomright = Coord {};

    while (true) {
        const input = wall.get(pos) orelse 0;
        try state.inputs.append(input);
        const color = (try intcode.run(&state)) orelse break;
        _ = try wall.put(pos, @intCast(u1, color));

        topleft.x = std.math.min(topleft.x, pos.x);
        topleft.y = std.math.min(topleft.y, pos.y);
        bottomright.x = std.math.max(bottomright.x, pos.x);
        bottomright.y = std.math.max(bottomright.y, pos.y);

        const rot_factor: i16 = switch ((try intcode.run(&state)).?) {
            0 => -1,
            1 => 1,
            else => unreachable
        };
        const old_delta = delta;
        delta = .{
            .x = switch (old_delta.x) {
                0 => old_delta.y * -rot_factor,
                else => 0
            },
            .y = switch (old_delta.y) {
                0 => old_delta.x * rot_factor,
                else => 0
            }
        };
        pos.x += delta.x; pos.y += delta.y;
    }

    return WallResult{ .wall = wall, .topleft = topleft, .bottomright = bottomright };
}
