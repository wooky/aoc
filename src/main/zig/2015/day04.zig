const aoc = @import("../aoc.zig");
const std = @import("std");


const HashIterator = struct {
    const Md5 = std.crypto.hash.Md5;
    const HashOutput = [Md5.digest_length]u8;

    counter: usize = 0,
    input: []const u8,

    fn init(input: []const u8) HashIterator {
        return HashIterator { .input = input };
    }

    fn next(self: *HashIterator) !?HashOutput {
        self.counter += 1;
        var buf: [16]u8 = undefined;
        const hash_input = try std.fmt.bufPrint(&buf, "{}{}", .{self.input, self.counter});
        var hash_output: HashOutput = undefined;
        Md5.hash(hash_input, &hash_output, .{});
        return hash_output;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var iterator = HashIterator.init(problem.input);

    const five = blk: {
        while (try iterator.next()) |hash| {
            if (hash[0] == 0 and hash[1] == 0 and hash[2] & 0xF0 == 0) {
                break :blk iterator.counter;
            }
        }
        unreachable;
    };

    const six = blk: {
        while (try iterator.next()) |hash| {
            if (hash[0] == 0 and hash[1] == 0 and hash[2] == 0) {
                break :blk iterator.counter;
            }
        }
        unreachable;
    };

    return aoc.Solution{ .p1 = five, .p2 = six };
}
