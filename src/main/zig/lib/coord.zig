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

    pub fn equals(self: *const Coord, other: ?Coord) bool {
        return other != null and self.row == other.?.row and self.col == other.?.col;
    }

    pub fn neighbors(self: *const Coord) CoordNeighborIterator {
        return CoordNeighborIterator.init(self.*);
    }
};

pub const CoordRange = struct {
    top_left: Coord = undefined,
    bottom_right: Coord = undefined,
    never_touched: bool = true,

    pub fn init() CoordRange {
        return .{};
    }

    pub fn initWithBounds(top_left: Coord, bottom_right: Coord) CoordRange {
        return .{ .top_left = top_left, .bottom_right = bottom_right };
    }

    pub fn amend(self: *CoordRange, coord: Coord) void {
        if (self.never_touched) {
            self.top_left = coord;
            self.bottom_right = coord;
            self.never_touched = false;
        }
        else {
            self.top_left.row = std.math.min(self.top_left.row, coord.row);
            self.top_left.col = std.math.min(self.top_left.col, coord.col);
            self.bottom_right.row = std.math.max(self.bottom_right.row, coord.row);
            self.bottom_right.col = std.math.max(self.bottom_right.col, coord.col);
        }
    }

    pub fn coordInRange(self: *const CoordRange, coord: Coord) bool {
        return
            coord.row >= self.top_left.row and coord.row <= self.bottom_right.row and
            coord.col >= self.top_left.col and coord.col <= self.bottom_right.col;
    }

    pub fn iterator(self: *const CoordRange) CoordRangeIterator {
        return CoordRangeIterator.init(self.top_left, self.bottom_right);
    }
};

pub const CoordRangeIterator = struct {
    first: Coord,
    last: Coord,
    curr: Coord,

    pub fn init(first: Coord, last: Coord) CoordRangeIterator {
        return .{ .first = first, .last = last, .curr = first };
    }

    pub fn next(self: *CoordRangeIterator) ?Coord {
        if (self.curr.row > self.last.row) {
            return null;
        }

        var res = self.curr;
        if (self.curr.col == self.last.col) {
            self.curr.row += 1;
            self.curr.col = self.first.col;
        }
        else {
            self.curr.col += 1;
        }

        return res;
    }
};

pub const CoordNeighborIterator = struct {
    center: Coord,
    range_iter: CoordRangeIterator,

    pub fn init(center: Coord) CoordNeighborIterator {
        return .{
            .center = center,
            .range_iter = CoordRangeIterator.init(
                center.subtract(Coord.fromRowCol(1, 1)),
                center.add(Coord.fromRowCol(1, 1))
            )
        };
    }

    pub fn next(self: *CoordNeighborIterator) ?Coord {
        const curr = self.range_iter.next();
        if (self.center.equals(curr)) {
            return self.range_iter.next();
        }
        return curr;
    }
};
