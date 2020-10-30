const std = @import("std");

pub fn main() !void {
    var unprintable: usize = 0;
    var extra: u16 = 0;

    var buf: [8192]u8 = undefined;
    const file = try std.fs.cwd().openFile("input/2015/day08.txt", .{});
    defer file.close();
    const size = try file.read(&buf);
    var lines = std.mem.tokenize(buf[0..size], "\n");
    while (lines.next()) |line| {
        var saw_backtick = false;
        var skip: u8 = 0;
        for (line) |c| {
            if (skip > 0) {
                skip -= 1;
            }
            else if (saw_backtick) {
                saw_backtick = false;
                if (c == 'x') {
                    skip = 2;
                    unprintable += 3;
                }
                else {
                    unprintable += 1;
                    extra += 1;
                }
            }
            else if (c == '\\') {
                extra += 1;
                saw_backtick = true;
            }
            else if (c == '"') {
                unprintable += 1;
                extra += 1;
            }
        }
        extra += 2;
    }
    std.debug.warn("{}\n{}\n", .{unprintable, extra});
}
