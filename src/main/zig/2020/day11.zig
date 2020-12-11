const aoc = @import("../aoc.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

const VACANT: u8 = 'L';
const OCCUPIED: u8 = '#';
const FLOOR: u8 = '.';

const SeatIterator = struct {
    allocator: *Allocator,
    seats: []u8,
    follow_blanks: bool,
    width: usize,
    height: usize,
    row: isize = 0,
    col: isize = -1,
    
    fn init(allocator: *Allocator, seats: []const u8, follow_blanks: bool) !SeatIterator {
        const width = std.mem.indexOf(u8, seats, &[_]u8 {'\n'}).?;
        const height = seats.len / (width + 1);
        return SeatIterator {
            .allocator = allocator,
            .seats = try allocator.dupe(u8, seats),
            .follow_blanks = follow_blanks,
            .width = width,
            .height = height
        };
    }

    fn deinit(self: *SeatIterator) void {
        self.allocator.free(self.seats);
    }

    fn next(self: *SeatIterator) ?u8 {
        while (true) {
            self.col += 1;
            if (self.col == self.width) {
                self.col = 0;
                self.row += 1;
                if (self.row == self.height) {
                    return null;
                }
            }

            const seat = self.getSeat(self.row, self.col);
            if (seat != FLOOR) {
                return seat;
            }
        }
    }

    fn getIndex(self: *const SeatIterator, row: isize, col: isize) usize {
        return @intCast(usize, row) * (self.width+1) + @intCast(usize, col);
    }

    fn getSeat(self: *const SeatIterator, row: isize, col: isize) u8 {
        return self.seats[self.getIndex(row, col)];
    }

    fn isOccupied(self: *const SeatIterator, drow: isize, dcol: isize) u8 {
        var row = self.row + drow;
        var col = self.col + dcol;
        while (row >= 0 and row < self.height and col >= 0 and col < self.width) : ({row += drow; col += dcol;}) {
            switch (self.getSeat(row, col)) {
                FLOOR => if (!self.follow_blanks) return 0,
                VACANT => return 0,
                OCCUPIED => return 1,
                else => unreachable
            }
        }
        return 0;
    }

    fn getOccupiedNeighbors(self: *const SeatIterator) u8 {
        return
            self.isOccupied(-1, -1) +
            self.isOccupied(-1,  0) +
            self.isOccupied(-1,  1) +
            self.isOccupied( 0, -1) +
            self.isOccupied( 0,  1) +
            self.isOccupied( 1, -1) +
            self.isOccupied( 1,  0) +
            self.isOccupied( 1,  1);
    }

    fn setSeat(self: *SeatIterator, row: isize, col: isize, new_seat: u8) void {
        self.seats[self.getIndex(row, col)] = new_seat;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    const res1 = try getOccupiedSeats(problem.allocator, problem.input, 4, false);
    const res2 = try getOccupiedSeats(problem.allocator, problem.input, 5, true);
    return aoc.Solution { .p1 = res1, .p2 = res2 };
}

fn getOccupiedSeats(allocator: *Allocator, seats: []const u8, tolerance: u8, follow_blanks: bool) !usize {
    var prev_arrangement = try SeatIterator.init(allocator, seats, follow_blanks);
    while (true) {
        var next_arrangement = try SeatIterator.init(allocator, seats, follow_blanks);
        while (prev_arrangement.next()) |prev_seat| {
            const occupiedNeighbors = prev_arrangement.getOccupiedNeighbors();
            var next_seat =
                if (prev_seat == VACANT and occupiedNeighbors == 0) OCCUPIED
                else if (prev_seat == OCCUPIED and occupiedNeighbors >= tolerance) VACANT
                else prev_seat;
            next_arrangement.setSeat(prev_arrangement.row, prev_arrangement.col, next_seat);
        }

        if (std.mem.eql(u8, prev_arrangement.seats, next_arrangement.seats)) {
            const res = std.mem.count(u8, prev_arrangement.seats, &[_]u8 {OCCUPIED});
            prev_arrangement.deinit();
            next_arrangement.deinit();
            return res;
        }

        prev_arrangement.deinit();
        prev_arrangement = next_arrangement;
    }
}
