const aoc = @import("../aoc.zig");
const std = @import("std");

const SCREEN_WIDTH = 50;
const SCREEN_HEIGHT = 6;

const ScreenRowType = std.meta.Int(.unsigned, SCREEN_WIDTH);
const ScreenColumnType = std.meta.Int(.unsigned, SCREEN_HEIGHT);
const Screen = std.bit_set.StaticBitSet(SCREEN_WIDTH * SCREEN_HEIGHT);

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var screen = Screen.initEmpty();
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(u8, line, " xy=");
        if (std.mem.eql(u8, tokens.next().?, "rect")) {
            rect(&screen, try std.fmt.parseInt(u8, tokens.next().?, 10), try std.fmt.parseInt(u8, tokens.next().?, 10));
        }
        else {
            const rotateFn = if (std.mem.eql(u8, tokens.next().?, "row")) rotateRow else rotateColumn;
            const a = try std.fmt.parseInt(u8, tokens.next().?, 10);
            _ = tokens.next().?;
            const b = try std.fmt.parseInt(u8, tokens.next().?, 10);
            rotateFn(&screen, a, b);
        }
    }

    var code: [(SCREEN_WIDTH + 1) * SCREEN_HEIGHT]u8 = undefined;
    std.mem.copy(u8, &code, ("." ** SCREEN_WIDTH ++ "\n") ** 6);
    var iter = screen.iterator(.{});
    while (iter.next()) |idx| {
        code[idx + idx / SCREEN_WIDTH] = '#';
    }

    return problem.solution(screen.count(), &code);
}

fn rect(screen: *Screen, width: u8, height: u8) void {
    var row_offset: usize = 0;
    while (row_offset < SCREEN_WIDTH * height) : (row_offset += SCREEN_WIDTH) {
        var col_offset: u8 = 0;
        while (col_offset < width) : (col_offset += 1) {
            screen.set(row_offset + col_offset);
        }
    }
}

fn rotateRow(screen: *Screen, row: u8, amount: u8) void {
    var row_mask: ScreenRowType = 0;
    var offset: usize = SCREEN_WIDTH * row;
    while (offset < @as(usize, SCREEN_WIDTH) * (row + 1)) : (offset += 1) {
        row_mask = (row_mask << 1) | @boolToInt(screen.isSet(offset));
    }
    row_mask = std.math.rotr(ScreenRowType, row_mask, amount);
    offset = SCREEN_WIDTH * row;
    while (offset < @as(usize, SCREEN_WIDTH) * (row + 1)) : (offset += 1) {
        screen.setValue(offset, row_mask & (1 << (SCREEN_WIDTH - 1)) != 0);
        row_mask <<= 1;
    }
}

fn rotateColumn(screen: *Screen, col: u8, amount: u8) void {
    var column_mask: ScreenColumnType = 0;
    var row_offset: usize = 0;
    while (row_offset < SCREEN_WIDTH * SCREEN_HEIGHT) : (row_offset += SCREEN_WIDTH) {
        column_mask = (column_mask << 1) | @boolToInt(screen.isSet(row_offset + col));
    }
    column_mask = std.math.rotr(ScreenColumnType, column_mask, amount);
    row_offset = 0;
    while (row_offset < SCREEN_WIDTH * SCREEN_HEIGHT) : (row_offset += SCREEN_WIDTH) {
        screen.setValue(row_offset + col, column_mask & (1 << (SCREEN_HEIGHT - 1)) != 0);
        column_mask <<= 1;
    }
}
