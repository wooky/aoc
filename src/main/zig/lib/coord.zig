const std = @import("std");

pub fn GenericCoord(comptime Self: type) type {
    return struct {
        pub fn init(coord: anytype) Self {
            var self: Self = undefined;
            inline for (@typeInfo(Self).Struct.fields) |field, i| {
                @field(self, field.name) = coord[i];
            }
            return self;
        }

        pub fn add(self: *const Self, other: Self) Self {
            var result = self.*;
            result.mutAdd(other);
            return result;
        }

        pub fn subtract(self: *const Self, other: Self) Self {
            var result = self.*;
            inline for (@typeInfo(Self).Struct.fields) |field| {
                @field(result, field.name) -= @field(other, field.name);
            }
            return result;
        }

        pub fn multiply(self: *const Self, factor: isize) Self {
            var result = self.*;
            result.mutMultiply(factor);
            return result;
        }

        pub fn mutAdd(self: *Self, other: Self) void {
            inline for (@typeInfo(Self).Struct.fields) |field| {
                @field(self, field.name) += @field(other, field.name);
            }
        }

        pub fn mutMultiply(self: *Self, factor: isize) void {
            inline for (@typeInfo(Self).Struct.fields) |field| {
                @field(self, field.name) *= factor;
            }
        }

        pub fn mutRotate90DegreesClockwise(self: *Self) void {
            if (@typeInfo(Self).Struct.fields.len != 2) {
                @compileError("Cannot rotate coord which doesn't have 2 points");
            }
            const old_coord = self.*;
            @field(self, @typeInfo(Self).Struct.fields[0].name) = @field(old_coord, @typeInfo(Self).Struct.fields[1].name);
            @field(self, @typeInfo(Self).Struct.fields[1].name) = -@field(old_coord, @typeInfo(Self).Struct.fields[0].name);
        }

        pub fn mutRotate90DegreesCounterclockwise(self: *Self) void {
            if (@typeInfo(Self).Struct.fields.len != 2) {
                @compileError("Cannot rotate coord which doesn't have 2 points");
            }
            const old_coord = self.*;
            @field(self, @typeInfo(Self).Struct.fields[0].name) = -@field(old_coord, @typeInfo(Self).Struct.fields[1].name);
            @field(self, @typeInfo(Self).Struct.fields[1].name) = @field(old_coord, @typeInfo(Self).Struct.fields[0].name);
        }

        pub fn distanceFromOrigin(self: *const Self) usize {
            var dist: usize = 0;
            inline for (@typeInfo(Self).Struct.fields) |field| {
                dist += std.math.absCast(@field(self, field.name));
            }
            return dist;
        }

        pub fn equals(self: *const Self, other: ?Self) bool {
            if (other == null) {
                return false;
            }
            inline for (@typeInfo(Self).Struct.fields) |field| {
                if (@field(self, field.name) != @field(other.?, field.name)) {
                    return false;
                }
            }
            return true;
        }

        pub fn neighbors(self: *const Self) GenericCoordNeighborIterator(Self) {
            return GenericCoordNeighborIterator(Self).init(self.*);
        }
    };
}
pub const Coord = struct {
    row: isize,
    col: isize,

    pub usingnamespace GenericCoord(Coord);
};
pub const Coord2D = struct {
    x: isize,
    y: isize,

    pub usingnamespace GenericCoord(Coord2D);
};

pub const PredefinedCoord = struct {
    pub const ORIGIN = Coord.init(.{0, 0});
    pub const UP = Coord.init(.{-1, 0});
    pub const RIGHT = Coord.init(.{0, 1});
    pub const DOWN = Coord.init(.{1, 0});
    pub const LEFT = Coord.init(.{0, -1});
};

pub fn GenericCoordRange(comptime C: type) type {
    return struct {
        const Self = @This();

        first: C = undefined,
        last: C = undefined,
        never_touched: bool = true,

        pub fn init() Self {
            return .{};
        }

        pub fn initWithBounds(first: C, last: C) Self {
            return .{ .first = first, .last = last };
        }

        pub fn amend(self: *Self, coord: C) void {
            if (self.never_touched) {
                self.first = coord;
                self.last = coord;
                self.never_touched = false;
            }
            else {
                inline for (@typeInfo(C).Struct.fields) |field| {
                    @field(self.first, field.name) = std.math.min(@field(self.first, field.name), @field(coord, field.name));
                    @field(self.last, field.name) = std.math.max(@field(self.last, field.name), @field(coord, field.name));
                }
            }
        }

        pub fn coordInRange(self: *const Self, coord: C) bool {
            inline for (@typeInfo(C).Struct.fields) |field| {
                if (@field(coord, field.name) < @field(self.first, field.name)) {
                    return false;
                }
                if (@field(coord, field.name) > @field(self.last, field.name)) {
                    return false;
                }
            }
            return true;
        }

        pub fn iterator(self: *const Self) GenericCoordRangeIterator(C) {
            return GenericCoordRangeIterator(C).init(self.first, self.last);
        }
    };
}
pub const CoordRange = GenericCoordRange(Coord);
pub const CoordRange2D = GenericCoordRange(Coord2D);

pub fn GenericCoordRangeIterator(comptime C: type) type {
    return struct {
        const Self = @This();

        first: C,
        last: C,
        curr: C,
        completed: bool = false,

        pub fn init(first: C, last: C) Self {
            return .{ .first = first, .last = last, .curr = first };
        }

        pub fn next(self: *Self) ?C {
            if (self.completed) {
                return null;
            }

            comptime var field_idx = @typeInfo(C).Struct.fields.len;
            var res = self.curr;
            var go = true; // required because breaking out of inline loop crashes compiler
            inline while (field_idx > 0) : (field_idx -= 1) {
                comptime const field_name = @typeInfo(C).Struct.fields[field_idx - 1].name;
                if (go) {
                    if (@field(self.curr, field_name) == @field(self.last, field_name)) {
                        if (field_idx == 1) {
                            self.completed = true;
                        }
                        else {
                            // Next field shall be incremented in the next iteration
                            @field(self.curr, field_name) = @field(self.first, field_name);
                        }
                    }
                    else {
                        @field(self.curr, field_name) += 1;
                        // break; // can't break here, crashes compiler, so next line is used as a hack
                        go = false;
                    }
                }
            }

            return res;
        }
    };
}
pub const CoordRangeIterator = GenericCoordRangeIterator(Coord);
pub const CoordRangeIterator2D = GenericCoordRangeIterator(Coord2D);

pub fn GenericCoordNeighborIterator(comptime C: type) type {
    return struct {
        const Self = @This();

        center: C,
        range_iter: GenericCoordRangeIterator(C),

        pub fn init(center: C) Self {
            var offset: C = undefined;
            inline for (@typeInfo(C).Struct.fields) |field| {
                @field(offset, field.name) = 1;
            }
            return .{
                .center = center,
                .range_iter = GenericCoordRangeIterator(C).init(
                    center.subtract(offset),
                    center.add(offset)
                )
            };
        }

        pub fn next(self: *Self) ?C {
            const curr = self.range_iter.next();
            if (self.center.equals(curr)) {
                return self.range_iter.next();
            }
            return curr;
        }
    };
}
pub const CoordNeighborIterator = GenericCoordNeighborIterator(Coord);
pub const CoordNeighborIterator2D = GenericCoordNeighborIterator(Coord2D);
