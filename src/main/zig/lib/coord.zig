const std = @import("std");

pub const Coord = struct {
    pub const Predefined = struct {
        pub const ORIGIN = Coord.fromRowCol(0, 0);
        pub const UP = Coord.fromRowCol(-1, 0);
        pub const RIGHT = Coord.fromRowCol(0, 1);
        pub const DOWN = Coord.fromRowCol(1, 0);
        pub const LEFT = Coord.fromRowCol(0, -1);
    };

    row: isize,
    col: isize,

    pub fn fromRowCol(row: isize, col: isize) Coord {
        return .{ .row = row, .col = col };
    }

    pub fn fromXY(x: isize, y: isize) Coord {
        return .{ .row = y, .col = x };
    }

    pub fn add(self: *const Coord, other: Coord) Coord {
        var result = self.*;
        result.mutAdd(other);
        return result;
    }

    pub fn mutAdd(self: *Coord, other: Coord) void {
        self.row += other.row;
        self.col += other.col;
    }

    pub fn multiply(self: *const Coord, factor: isize) Coord {
        var result = self.*;
        result.mutMultiply(factor);
        return result;
    }

    pub fn subtract(self: *const Coord, other: Coord) Coord {
        return .{ .row = self.row - other.row, .col = self.col - other.col };
    }

    pub fn mutMultiply(self: *Coord, factor: isize) void {
        self.row *= factor;
        self.col *= factor;
    }

    pub fn mutRotate90DegreesClockwise(self: *Coord) void {
        const old_coord = self.*;
        self.row = old_coord.col;
        self.col = -old_coord.row;
    }

    pub fn mutRotate90DegreesCounterclockwise(self: *Coord) void {
        const old_coord = self.*;
        self.row = -old_coord.col;
        self.col = old_coord.row;
    }

    pub fn distanceFromOrigin(self: *const Coord) usize {
        return std.math.absCast(self.row) + std.math.absCast(self.col);
    }

    pub fn equals(self: *const Coord, other: Coord) bool {
        return self.row == other.row and self.col == other.col;
    }
};
