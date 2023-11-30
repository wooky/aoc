const aoc = @import("../aoc.zig");
const std = @import("std");

const TokenState = enum { turn_toggle, on_off, start_coord, through, end_coord };
const Command = enum { turn_on, turn_off, toggle };

const Light = struct {
    p1: u1 = 0,
    p2: u8 = 0,
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var lights = [_]Light{.{}} ** 1_000_000;

    while (problem.line()) |line| {
        var range = aoc.CoordRange2D.init();
        var state = TokenState.turn_toggle;
        var command: Command = undefined;
        var tokens = std.mem.tokenize(u8, line, " ");
        while (tokens.next()) |token| {
            switch (state) {
                .turn_toggle => {
                    if (std.mem.eql(u8, token, "turn")) {
                        state = .on_off;
                    } else {
                        state = .start_coord;
                        command = .toggle;
                    }
                },
                .on_off => {
                    state = .start_coord;
                    command = if (std.mem.eql(u8, token, "on")) .turn_on else .turn_off;
                },
                .start_coord => {
                    range.amend(try parseCoord(token));
                    state = .through;
                },
                .through => {
                    state = .end_coord;
                },
                .end_coord => range.amend(try parseCoord(token)),
            }
        }

        var iter = range.iterator();
        while (iter.next()) |coord| {
            var light = &lights[@as(usize, @intCast(coord.y * 1000 + coord.x))];
            switch (command) {
                .turn_on => {
                    light.p1 = 1;
                    light.p2 += 1;
                },
                .turn_off => {
                    light.p1 = 0;
                    if (light.p2 > 0) light.p2 -= 1;
                },
                .toggle => {
                    light.p1 ^= 1;
                    light.p2 += 2;
                },
            }
        }
    }
    var lights_on: u32 = 0;
    var brightness: u32 = 0;
    for (lights) |light| {
        lights_on += light.p1;
        brightness += light.p2;
    }
    return problem.solution(lights_on, brightness);
}

fn parseCoord(token: []const u8) !aoc.Coord2D {
    var vals = std.mem.tokenize(u8, token, ",");
    return aoc.Coord2D.init(.{ try std.fmt.parseInt(u16, vals.next().?, 10), try std.fmt.parseInt(u16, vals.next().?, 10) });
}
