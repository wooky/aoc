const aoc = @import("../aoc.zig");
const std = @import("std");

const Packet = struct {
    const Operation = enum (u3) {
        Sum = 0,
        Product = 1,
        Minimum = 2,
        Maximum = 3,
        Literal = 4,
        GreaterThan = 5,
        LessThan = 6,
        Equal = 7,
    };

    const SubpacketParsage = union (enum) {
        Type0: struct {
            end_bit: usize,
        },
        Type1: struct {
            count: u11,
            idx: u11 = 0,
        },
    };

    version_total: usize,
    computation_total: usize,

    fn parse(input: []const u8, bit: *usize) Packet {
        var version_total: usize = readChunk(u3, input, bit);
        const operation = @intToEnum(Operation, readChunk(u3, input, bit));
        if (operation == .Literal) {
            var literal: usize = 0;
            var read_next: u1 = 1;
            while (read_next == 1) {
                read_next = readChunk(u1, input, bit);
                literal = (literal << 4) | readChunk(u4, input, bit);
            }
            return .{ .version_total = version_total, .computation_total = literal };
        }

        const length_type_id = readChunk(u1, input, bit);
        var subpacket_parsage: SubpacketParsage = if (length_type_id == 0)
            SubpacketParsage{ .Type0 = .{ .end_bit = readChunk(u15, input, bit) + bit.* } } // order of operations is very important!
        else
            SubpacketParsage{ .Type1 = .{ .count = readChunk(u11, input, bit) } };
        
        var computation_total = blk: {
            const result = parseSubpacket(input, bit, &subpacket_parsage).?;
            version_total += result.version_total;
            break :blk result.computation_total;
        };

        while (parseSubpacket(input, bit, &subpacket_parsage)) |subpacket| {
            computation_total = switch (operation) {
                .Sum => computation_total + subpacket.computation_total,
                .Product => computation_total * subpacket.computation_total,
                .Minimum => std.math.min(computation_total, subpacket.computation_total),
                .Maximum => std.math.max(computation_total, subpacket.computation_total),
                .Literal => unreachable,
                .GreaterThan => @boolToInt(computation_total > subpacket.computation_total),
                .LessThan => @boolToInt(computation_total < subpacket.computation_total),
                .Equal => @boolToInt(computation_total == subpacket.computation_total),
            };
            version_total += subpacket.version_total;
        }

        return .{ .version_total = version_total, .computation_total = computation_total };
    }

    fn parseSubpacket(input: []const u8, bit: *usize, parsage: *SubpacketParsage) ?Packet {
        switch (parsage.*) {
            .Type0 => |t0| {
                if (bit.* == t0.end_bit) {
                    return null;
                }
            },
            .Type1 => |*t1| {
                if (t1.idx == t1.count) {
                    return null;
                }
                t1.idx += 1;
            }
        }
        return parse(input, bit);
    }

    fn readChunk(comptime T: type, input: []const u8, bit: *usize) T {
        const end_bit = bit.* + std.meta.bitCount(T);
        var result: usize = 0;
        while (bit.* != end_bit) {
            const from = @intCast(u2, bit.* % 4);
            const bits_left_to_read = end_bit - bit.*;
            const to_excl = std.math.min(from + bits_left_to_read, 4);
            const bits_read = @intCast(u3, to_excl - from);
            const nibble = readNibble(input[bit.* / 4], from, @intCast(u2, to_excl - 1));
            result = (result << bits_read) | nibble;
            bit.* += bits_read;
        }
        return @intCast(T, result);
    }

    fn readNibble(input: u8, from: u2, to: u2) u4 {
        var result = if (input <= '9') @intCast(u4, input - '0') else @intCast(u4, input - 'A' + 10);
        result <<= from;
        result >>= 3 - (to - from);
        return result;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var start_bit: usize = 0;
    var packet = Packet.parse(problem.input, &start_bit);

    return problem.solution(packet.version_total, packet.computation_total);
}
