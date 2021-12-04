const aoc = @import("../aoc.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const BOARD_SIZE = 5;

const Board = struct {
    numbers: std.AutoHashMap(u8, aoc.Coord),
    markings: std.AutoHashMap(aoc.Coord, void),

    fn init(allocator: *Allocator) Board {
        return .{
            .numbers = std.AutoHashMap(u8, aoc.Coord).init(allocator),
            .markings = std.AutoHashMap(aoc.Coord, void).init(allocator),
        };
    }

    fn loadLine(self: *Board, line: []const u8, row: u8) !void {
        var tokens = std.mem.tokenize(line, " ");
        var col: u8 = 0;
        while (tokens.next()) |token| {
            try self.numbers.put(try std.fmt.parseInt(u8, token, 10), .{ .row = row, .col = col });
            col += 1;
        }
    }

    fn markAndCheckWin(self: *Board, number: u8) !bool {
        if (self.numbers.get(number)) |coord| {
            try self.markings.putNoClobber(coord, {});
            blk: {
                var idx: u8 = 0;
                while (idx < BOARD_SIZE) : (idx += 1) {
                    if (!self.markings.contains(.{ .row = coord.row, .col = idx })) {
                        break :blk;
                    }
                }
                return true;
            }
            blk: {
                var idx: u8 = 0;
                while (idx < BOARD_SIZE) : (idx += 1) {
                    if (!self.markings.contains(.{ .row = idx, .col = coord.col })) {
                        break :blk;
                    }
                }
                return true;
            }
        }
        return false;
    }

    fn sumUnmarked(self: *const Board) usize {
        var sum: usize = 0;
        var iter = self.numbers.iterator();
        while (iter.next()) |kv| {
            if (!self.markings.contains(kv.value_ptr.*)) {
                sum += kv.key_ptr.*;
            }
        }
        return sum;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var arena = std.heap.ArenaAllocator.init(problem.allocator);
    defer arena.deinit();
    const numbers = problem.line().?;

    var boards = std.AutoHashMap(*Board, void).init(&arena.allocator);
    while (problem.line()) |line| {
        var kv = try boards.getOrPutValue(try arena.allocator.create(Board), {});
        kv.key_ptr.*.* = Board.init(&arena.allocator);
        try kv.key_ptr.*.loadLine(line, 0);

        var line_num: u8 = 1;
        while (line_num < BOARD_SIZE) : (line_num += 1) {
            try kv.key_ptr.*.loadLine(problem.line().?, line_num);
        }
    }

    var s1: ?usize = null;
    var s2 = blk: {
        var tokens = std.mem.tokenize(numbers, ",");
        while (tokens.next()) |token| {
            const last_called = try std.fmt.parseInt(u8, token, 10);
            const winning_boards = try getWinningBoards(&arena.allocator, boards.keyIterator(), last_called);
            for (winning_boards) |wb| {
                _ = boards.remove(wb);
            }
            if (winning_boards.len == 1 and s1 == null) {
                s1 = winning_boards[0].sumUnmarked() * last_called;
            }
            else if (boards.count() == 0) {
                break :blk winning_boards[0].sumUnmarked() * last_called;
            }
        }
        std.debug.print("s1 = {}, boards remain = {}\n", .{s1, boards.count()});
        unreachable;
    };

    return problem.solution(s1.?, s2);
}

fn getWinningBoards(allocator: *Allocator, board_iter_const: std.AutoHashMap(*Board, void).KeyIterator, last_called: u8) ![]*Board {
    var winning_boards = std.ArrayList(*Board).init(allocator);
    var board_iter = board_iter_const;
    while (board_iter.next()) |board| {
        if (try board.*.markAndCheckWin(last_called)) {
            try winning_boards.append(board.*);
        }
    }
    return winning_boards.items;
}
