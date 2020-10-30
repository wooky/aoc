const std = @import("std");

pub fn main() !void {
    var floor: i16 = 0;
    var basement_idx: usize = 0;
    var buf: [8192]u8 = undefined;
    const file = try std.fs.cwd().openFile("input/2015/day01.txt", .{});
    defer file.close();
    const bytes_read = try file.readAll(&buf);
    for (buf[0..bytes_read]) |c, idx| {
        floor += switch (c) {
            '(' => @intCast(i16, 1),
            ')' => -1,
            else => unreachable,
        };
        if (floor == -1 and basement_idx == 0) {
            basement_idx = idx + 1;
        }
    }
    std.debug.warn("{}\n{}\n", .{floor, basement_idx});
}
