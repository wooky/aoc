const aoc = @import("../aoc.zig");
const std = @import("std");

const Password = struct {
    input: []const u8,
    pwd1: u32 = 0,
    pwd1_idx: u8 = 0,
    pwd2: u32 = 0,
    pwd2_bitmask: u8 = 0,
    hash_idx: usize = 0,
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    // var password = Password{ .input = problem.input };
    // var loop: std.event.Loop = undefined;
    // try loop.init();
    // defer loop.deinit();
    // for (loop.extra_threads) |_| {
    //     try loop.runDetached(problem.allocator, doHash, .{&password});
    // }
    // loop.run();

    // const s1 = try std.fmt.allocPrint(problem.allocator, "{x:0>8}", .{password.pwd1});
    // defer problem.allocator.free(s1);
    // const s2 = try std.fmt.allocPrint(problem.allocator, "{x:0>8}", .{password.pwd2});
    // defer problem.allocator.free(s2);
    // return problem.solution(s1, s2);

    // TODO https://github.com/wooky/aoc/issues/9
    return problem.solution("TODO #9", "TODO #9");
}

// not thread safe yolo
fn doHash(password: *Password) void {
    var hash: [std.crypto.hash.Md5.digest_length]u8 = undefined;
    var input_buf: [16]u8 = undefined;
    while (password.pwd2_bitmask != 0xFF) {
        const hash_idx = @atomicRmw(usize, &password.hash_idx, .Add, 1, .SeqCst);
        const input = std.fmt.bufPrint(&input_buf, "{s}{}", .{ password.input, hash_idx }) catch unreachable;
        std.crypto.hash.Md5.hash(input, &hash, .{});
        if (hash[0] == 0 and hash[1] == 0 and hash[2] & 0xF0 == 0) {
            const c6 = @as(u4, @intCast(hash[2] & 0x0F));
            if (password.pwd1_idx < 8) {
                password.pwd1 = (password.pwd1 << 4) | c6;
                password.pwd1_idx += 1;
            }
            if (c6 < 8) {
                const c6_bitpos = @as(u3, @intCast(7 - c6));
                const c6_bitmask = @as(u8, 1) << c6_bitpos;
                if (password.pwd2_bitmask & c6_bitmask == 0) {
                    const c7: u32 = hash[3] >> 4;
                    password.pwd2 |= c7 << (@as(u5, c6_bitpos) * 4);
                    password.pwd2_bitmask |= c6_bitmask;
                }
            }
        }
    }
}
