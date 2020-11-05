const aoc = @import("../aoc.zig");
const std = @import("std");

const TokenState = enum {
    turn_toggle, on_off, start_coord, through, end_coord
};
const Command = enum {
    turn_on, turn_off, toggle
};

const Light = struct {
    p1: bool, p2: u8,
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var lights = [_]Light{Light { .p1 = false, .p2 = 0 }} ** 1_000_000;

    while (problem.line()) |line| {
        var left: u16 = undefined;
        var top: u16 = undefined;
        var right: u16 = undefined;
        var bottom: u16 = undefined;
        var state = TokenState.turn_toggle;
        var command: Command = undefined;
        var tokens = std.mem.tokenize(line, " ");
        while (tokens.next()) |token| {
            switch (state) {
                .turn_toggle => {
                    if (std.mem.eql(u8, token, "turn")) {
                        state = .on_off;
                    }
                    else {
                        state = .start_coord;
                        command = .toggle;
                    }
                },
                .on_off => {
                    state = .start_coord;
                    command = if (std.mem.eql(u8, token, "on")) .turn_on else .turn_off;
                },
                .start_coord => {
                    try parse_coords(token, &left, &top);
                    state = .through;
                },
                .through => { state = .end_coord; },
                .end_coord => try parse_coords(token, &right, &bottom),
            }
        }
        var row = top;
        while (row <= bottom) : (row += 1) {
            var col = left;
            while (col <= right) : (col += 1) {
                var light = &lights[@intCast(usize, row) * 1000 + col];
                switch (command) {
                    .turn_on => {light.p1 = true; light.p2 += 1;},
                    .turn_off => {light.p1 = false; if (light.p2 > 0) light.p2 -= 1;},
                    .toggle => {light.p1 = !light.p1; light.p2 += 2;},
                }
            }
        }
    }
    var lights_on: u32 = 0;
    var brightness: u32 = 0;
    for (lights) |light| {
        if (light.p1) {
            lights_on += 1;
        }
        brightness += light.p2;
    }
    return aoc.Solution{ .p1 = lights_on, .p2 = brightness };
}

fn parse_coords(token: []const u8, x: *u16, y: *u16) !void {
    var vals = std.mem.tokenize(token, ",");
    x.* = try std.fmt.parseInt(u16, vals.next().?, 10);
    y.* = try std.fmt.parseInt(u16, vals.next().?, 10);
}
