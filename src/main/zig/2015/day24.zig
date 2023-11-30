const aoc = @import("../aoc.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

const Combinator = struct {
    pub const CombinationResult = struct {
        unprocessed: Elements = undefined,
        sum: usize = 0,
        product: usize = 1,
    };

    const Elements = []const usize;
    const ResultBuf = std.ArrayList(usize);

    elements: Elements,
    unprocessed_buf: ResultBuf,
    r: u6 = undefined,
    last_bitmask: usize = undefined,
    max_bitmask: usize = undefined,

    pub fn init(allocator: Allocator, elements: Elements) Combinator {
        return Combinator{
            .elements = elements,
            .unprocessed_buf = ResultBuf.init(allocator),
        };
    }

    pub fn deinit(self: *Combinator) void {
        self.unprocessed_buf.deinit();
    }

    pub fn reset_with_combinations(self: *Combinator, r: u6) void {
        self.r = r;
        self.last_bitmask = (@as(usize, @intCast(1)) << r) - 1;
        self.max_bitmask = self.last_bitmask << (@as(u6, @intCast(self.elements.len)) - r);
    }

    pub fn next(self: *Combinator) !?CombinationResult {
        while (self.last_bitmask <= self.max_bitmask) {
            const bitmask = self.last_bitmask;
            self.last_bitmask += 1;
            if (@popCount(bitmask) == self.r) {
                self.unprocessed_buf.items.len = 0;
                var result = CombinationResult{};
                var idx: usize = 0;
                while (idx < self.elements.len) : (idx += 1) {
                    const element = self.elements[idx];
                    if ((@as(usize, @intCast(1)) << @as(u6, @intCast(idx))) & bitmask == 0) {
                        try self.unprocessed_buf.append(element);
                    } else {
                        result.sum += element;
                        result.product *= element;
                    }
                }
                result.unprocessed = self.unprocessed_buf.items;
                return result;
            }
        }
        return null;
    }
};

const QuantumEntanglement = struct {
    const Weights = std.ArrayList(usize);

    allocator: Allocator,
    weights: Weights,
    total_weight: usize = 0,

    fn init(allocator: Allocator) QuantumEntanglement {
        return QuantumEntanglement{ .allocator = allocator, .weights = Weights.init(allocator) };
    }

    fn deinit(self: *QuantumEntanglement) void {
        self.weights.deinit();
    }

    fn addWeight(self: *QuantumEntanglement, weight: usize) !void {
        try self.weights.append(weight);
        self.total_weight += weight;
    }

    fn calcMinQe(self: *QuantumEntanglement, groups: u8) !usize {
        const target_weight = self.total_weight / groups;
        const max_usize: usize = std.math.maxInt(usize);
        var min_qe = max_usize;
        var combinator = Combinator.init(self.allocator, self.weights.items);
        defer combinator.deinit();
        var r: u6 = 1;
        while (min_qe == max_usize) : (r += 1) {
            combinator.reset_with_combinations(r);
            while (try combinator.next()) |result| {
                if (result.sum == target_weight and result.product < min_qe and
                    try self.isValidGroup(result.unprocessed, groups - 1, r, target_weight))
                {
                    min_qe = result.product;
                }
            }
        }
        return min_qe;
    }

    fn isValidGroup(self: *QuantumEntanglement, group: []const usize, depth: u8, min_r: u6, target_weight: usize) anyerror!bool {
        var combinator = Combinator.init(self.allocator, group);
        defer combinator.deinit();
        var r: u6 = min_r;
        while (r <= group.len / depth) : (r += 1) {
            combinator.reset_with_combinations(r);
            while (try combinator.next()) |result| {
                if (result.sum == target_weight) {
                    if (depth == 2) {
                        return true;
                    }
                    if (try self.isValidGroup(result.unprocessed, depth - 1, r, target_weight)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    fn printSlice(slice: []const usize) void {
        for (slice) |a| {
            std.debug.print("{} ", .{a});
        }
        std.debug.print("\n", .{});
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var qe = QuantumEntanglement.init(problem.allocator);
    defer qe.deinit();
    while (problem.line()) |line| {
        const weight = try std.fmt.parseInt(usize, line, 10);
        try qe.addWeight(weight);
    }

    const qe1 = try qe.calcMinQe(3);
    const qe2 = try qe.calcMinQe(4);

    return problem.solution(qe1, qe2);
}
