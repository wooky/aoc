const aoc = @import("../aoc.zig");
const std = @import("std");

const HashCounter = struct {
    count: usize = 0,
    count5: ?usize = null,
    count6: ?usize = null,
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var counter = HashCounter {};
    var loop: std.event.Loop = undefined;
    try loop.init();
    defer loop.deinit();
    for (loop.extra_threads) |_| {
        try loop.runDetached(problem.allocator, doHash, .{problem.input, &counter});
    }
    loop.run();

    return problem.solution(counter.count5.?, counter.count6.?);
}

// not thread safe btw
fn doHash(input: []const u8, counter: *HashCounter) void {
    var buf: [16]u8 = undefined;
    var hash: [std.crypto.hash.Md5.digest_length]u8 = undefined;
    while (counter.count6 == null) {
        const count = @atomicRmw(usize, &counter.count, .Add, 1, .SeqCst);
        const hash_input = std.fmt.bufPrint(&buf, "{s}{d}", .{input, count}) catch unreachable;
        std.crypto.hash.Md5.hash(hash_input, &hash, .{});
        if (hash[0] == 0 and hash[1] == 0) {
            if (hash[2] == 0) {
                counter.count6 = count;
            }
            else if (counter.count5 == null and hash[2] & 0xF0 == 0) {
                counter.count5 = count;
            }
        }
    }
}
