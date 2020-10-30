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

pub fn main() !void {
    var lights = [_]Light{Light { .p1 = false, .p2 = 0 }} ** 1_000_000;

    var buf: [16384]u8 = undefined;
    var file = try std.fs.cwd().openFile("input/2015/day06.txt", .{});
    defer file.close();
    var size = try file.read(&buf);
    var lines = std.mem.tokenize(buf[0..size], "\n");
    while (lines.next()) |line| {
        var left: u16 = undefined;
        var top: u16 = undefined;
        var right: u16 = undefined;
        var bottom: u16 = undefined;
        var state = TokenState.turn_toggle;
        var command: Command = undefined;
        var tokens = std.mem.tokenize(line, " ");
        while (tokens.next()) |token| {
            switch (state) {
                TokenState.turn_toggle => {
                    if (std.mem.eql(u8, token, "turn")) {
                        state = TokenState.on_off;
                    }
                    else {
                        state = TokenState.start_coord;
                        command = Command.toggle;
                    }
                },
                TokenState.on_off => {
                    state = TokenState.start_coord;
                    command = if (std.mem.eql(u8, token, "on")) Command.turn_on else Command.turn_off;
                },
                TokenState.start_coord => {
                    try parse_coords(token, &left, &top);
                    state = TokenState.through;
                },
                TokenState.through => { state = TokenState.end_coord; },
                TokenState.end_coord => try parse_coords(token, &right, &bottom),
            }
        }
        var row = top;
        while (row <= bottom) : (row += 1) {
            var col = left;
            while (col <= right) : (col += 1) {
                var light = &lights[@intCast(usize, row) * 1000 + col];
                switch (command) {
                    Command.turn_on => {light.p1 = true; light.p2 += 1;},
                    Command.turn_off => {light.p1 = false; if (light.p2 > 0) light.p2 -= 1;},
                    Command.toggle => {light.p1 = !light.p1; light.p2 += 2;},
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
    std.debug.warn("{}\n{}\n", .{lights_on, brightness});
}

fn parse_coords(token: []const u8, x: *u16, y: *u16) !void {
    var vals = std.mem.tokenize(token, ",");
    x.* = try std.fmt.parseInt(u16, vals.next().?, 10);
    y.* = try std.fmt.parseInt(u16, vals.next().?, 10);
}
