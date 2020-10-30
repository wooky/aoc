const std = @import("std");
const VisitedMap = std.AutoHashMap(Address, void);

const Address = struct {
    row: i32 = 0, col: i32 = 0,

    fn advance(self: *Address, visited: *VisitedMap, count: *u16, direction: u8) void {
        switch (direction) {
            '^' => self.row -= 1,
            'v' => self.row += 1,
            '<' => self.col -= 1,
            '>' => self.col += 1,
            0 => {},
            else => unreachable,
        }
        const dest = Address { .row = self.row, .col = self.col };
        if (!visited.contains(dest)) {
            count.* += 1;
            _ = visited.put(dest, {}) catch {};
        }
    }
};

pub fn main() !void {
    var count_solo: u16 = 0;
    var visited_solo = VisitedMap.init(std.heap.page_allocator);
    defer visited_solo.deinit();
    var santa_solo = Address {}; santa_solo.advance(&visited_solo, &count_solo, 0);

    var count_pair: u16 = 0;
    var visited_pair = VisitedMap.init(std.heap.page_allocator);
    defer visited_pair.deinit();
    var santa_pair = Address {}; santa_pair.advance(&visited_pair, &count_pair, 0);
    var robot_pair = Address {}; robot_pair.advance(&visited_pair, &count_pair, 0);
    var santa_turn = true;

    var buf: [8192]u8 = undefined;
    const file = try std.fs.cwd().openFile("input/2015/day03.txt", .{});
    defer file.close();
    const size = try file.read(&buf);
    for (buf[0..size]) |c| {
        santa_solo.advance(&visited_solo, &count_solo, c);
        (if (santa_turn) santa_pair else robot_pair).advance(&visited_pair, &count_pair, c);
        santa_turn = !santa_turn;
    }
    std.debug.warn("{}\n{}\n", .{count_solo, count_pair});
}
