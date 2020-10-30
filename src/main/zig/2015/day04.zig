const problem = "ckczppom";
const std = @import("std");

pub fn main() !void {
    var counter: u32 = 1;
    var five: u32 = 0;
    var six: u32 = 0;
    var buf: [32]u8 = undefined;
    var hash: [16]u8 = undefined;
    while (true) {
        const input = try std.fmt.bufPrint(&buf, "{}{}", .{problem, counter});
        std.crypto.Md5.hash(input, &hash);
        var str_hash = try std.fmt.bufPrint(&buf, "{x}", .{hash});
        if (five == 0 and std.mem.startsWith(u8, str_hash, "00000")) {
            five = counter;
            if (six != 0) {
                break;
            }
        }
        if (six == 0 and std.mem.startsWith(u8, str_hash, "000000")) {
            six = counter;
            if (five != 0) {
                break;
            }
        }
        counter += 1;
    }
    std.debug.warn("{}\n{}\n", .{five, six});
}
