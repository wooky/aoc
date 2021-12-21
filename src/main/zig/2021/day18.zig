const aoc = @import("../aoc.zig");
const std = @import("std");

const SnailfishNumber = struct {
    const Elements = std.ArrayList(struct {
        number: u8,
        depth: u8,
    });

    elements: Elements,

    fn initFromLine(allocator: std.mem.Allocator, line: []const u8) !SnailfishNumber {
        var snailfish_number = SnailfishNumber {
            .elements = Elements.init(allocator),
        };
        var depth: u8 = 0;
        for (line) |c| {
            switch (c) {
                '[' => depth += 1,
                ']' => depth -= 1,
                ',' => {},
                else => try snailfish_number.elements.append(.{ .number = c - '0', .depth = depth - 1 })
            }
        }
        return snailfish_number;
    }

    fn deinit(self: *SnailfishNumber) void {
        self.elements.deinit();
    }

    fn add(self: *const SnailfishNumber, other: *const SnailfishNumber) !SnailfishNumber {
        // std.debug.print("{s: <8}", .{" "});
        // var asdf: u8 = 0;
        // while (asdf < 25) : (asdf += 1) {
        //     std.debug.print("{: >4} ", .{asdf});
        // }
        // std.debug.print("\n", .{});
        var result = SnailfishNumber {
            .elements = Elements.init(self.elements.allocator),
        };
        for (self.elements.items) |element| {
            try result.elements.append(.{
                .number = element.number,
                .depth = element.depth + 1,
            });
        }
        for (other.elements.items) |element| {
            try result.elements.append(.{
                .number = element.number,
                .depth = element.depth + 1,
            });
        }

        blk: while (true) {
            // Explode
            for (result.elements.items) |element, idx| {
                const depth = element.depth;
                if (depth >= 4 and result.elements.items[idx + 1].depth == depth) {
                    if (idx > 0) {
                        result.elements.items[idx - 1].number += element.number;
                    }
                    if (idx < result.elements.items.len - 2) {
                        result.elements.items[idx + 2].number += result.elements.items[idx + 1].number;
                    }
                    result.elements.items[idx] = .{
                        .number = 0,
                        .depth = depth - 1,
                    };
                    _ = result.elements.orderedRemove(idx + 1);
                    // std.debug.print("x {: <2},{: <2} ", .{idx, idx+1});
                    // result.print();
                    continue :blk;
                }
            }

            // Split
            for (result.elements.items) |element, idx| {
                const number = element.number;
                if (element.number >= 10) {
                    const depth = element.depth;
                    result.elements.items[idx] = .{
                        .number = try std.math.divFloor(u8, number, 2),
                        .depth = depth + 1,
                    };
                    try result.elements.insert(idx + 1, .{
                        .number = try std.math.divCeil(u8, number, 2),
                        .depth = depth + 1,
                    });
                    // std.debug.print("s {: <2}    ", .{idx});
                    // result.print();
                    continue :blk;
                }
            }

            // Done
            break;
        }

        // result.print();
        return result;
    }

    fn magnitude(self: *const SnailfishNumber, depth: u8, idx: *u8) usize {
        // std.debug.print("d{} i{}\n", .{depth, idx.*});
        const left = @as(usize, 3) * blk: {
            const element = self.elements.items[idx.*];
            break :blk if (element.depth != depth)
                self.magnitude(depth + 1, idx)
            else
                element.number
            ;
        };

        idx.* += 1;

        const right = @as(usize, 2) * blk: {
            const element = self.elements.items[idx.*];
            break :blk if (element.depth != depth)
                self.magnitude(depth + 1, idx)
            else
                element.number
            ;
        };

        // std.debug.print("={}\n", .{left + right});
        return left + right;
    }

    fn print(self: *const SnailfishNumber) void {
        for (self.elements.items) |element| {
            std.debug.print("{: >2}d{} ", .{element.number, element.depth});
        }
        std.debug.print("\n", .{});
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var numbers = std.ArrayList(SnailfishNumber).init(problem.allocator);
    defer {
        for (numbers.items) |*number| {
            number.deinit();
        }
        numbers.deinit();
    }

    const total_sum = blk: {
        var result: ?SnailfishNumber = null;
        while (problem.line()) |line| {
            var next = try SnailfishNumber.initFromLine(problem.allocator, line);
            try numbers.append(next);
            
            if (result) |*r| {
                var next_result = try r.add(&next);
                r.deinit();
                result = next_result;
            }
            else {
                result = try numbers.items[0].add(&next);
            }
        }

        defer result.?.deinit();
        var idx: u8 = 0;
        break :blk result.?.magnitude(0, &idx);
    };

    const max_sum = blk: {
        var max_sum: usize = 0;
        var i: u8 = 0;
        while (i < numbers.items.len) : (i += 1) {
            var j: u8 = 0;
            while (j < numbers.items.len) : (j += 1) {
                if (i == j) {
                    continue;
                }
                var result = try numbers.items[i].add(&numbers.items[j]);
                defer result.deinit();
                var idx: u8 = 0;
                max_sum = std.math.max(max_sum, result.magnitude(0, &idx));
            }
        }
        break :blk max_sum;
    };

    return problem.solution(total_sum, max_sum);
}
