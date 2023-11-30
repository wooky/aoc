const aoc = @import("../aoc.zig");
const std = @import("std");

const Operand = union(enum) {
    constant: u16,
    wire: []const u8,

    fn parse(op: []const u8) Operand {
        const constant = std.fmt.parseInt(u16, op, 10) catch return Operand{ .wire = op };
        return Operand{ .constant = constant };
    }

    fn evaluate(self: Operand, cache: *WireCache) u16 {
        return switch (self) {
            .wire => |w| cache.get(w),
            .constant => |c| c,
        };
    }
};

const LogicGate = enum { AND, OR, LSHIFT, RSHIFT, NOT, BUFFER };
const Operation = struct {
    op1: Operand = .{ .constant = 0 },
    op2: Operand = .{ .constant = 0 },
    gate: LogicGate = undefined,

    fn evaluate(self: *Operation, cache: *WireCache) u16 {
        const op1 = self.op1.evaluate(cache);
        const op2 = self.op2.evaluate(cache);

        return switch (self.gate) {
            .AND => op1 & op2,
            .OR => op1 | op2,
            .LSHIFT => op1 << @as(u4, @intCast(op2)),
            .RSHIFT => op1 >> @as(u4, @intCast(op2)),
            .NOT => ~op2,
            .BUFFER => op1,
        };
    }
};

const OutputMap = std.StringHashMap(Operation);

const WireCache = struct {
    backing: WireCacheBacking,
    outputs: OutputMap,

    const WireCacheBacking = std.StringHashMap(u16);

    fn init(allocator: std.mem.Allocator, outputs: OutputMap) WireCache {
        return WireCache{ .backing = WireCacheBacking.init(allocator), .outputs = outputs };
    }

    fn deinit(self: *WireCache) void {
        self.backing.deinit();
    }

    fn get(self: *WireCache, output: []const u8) u16 {
        const cached_value = self.backing.get(output);
        if (cached_value) |v| {
            return v;
        }
        const value = self.outputs.getPtr(output).?.evaluate(self);
        _ = self.backing.put(output, value) catch unreachable;
        return value;
    }
};

const TokenState = enum { operand1_not, gate_arrow, operand2, arrow, output };

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var outputs = OutputMap.init(problem.allocator);
    defer outputs.deinit();

    while (problem.line()) |line| {
        var operation = Operation{};
        var output: []const u8 = undefined;
        var state = TokenState.operand1_not;
        var tokens = std.mem.tokenize(u8, line, " ");
        while (tokens.next()) |token| {
            switch (state) {
                .operand1_not => {
                    if (std.mem.eql(u8, token, "NOT")) {
                        operation.gate = .NOT;
                        state = .operand2;
                    } else {
                        operation.op1 = Operand.parse(token);
                        state = .gate_arrow;
                    }
                },
                .gate_arrow => {
                    if (std.mem.eql(u8, token, "->")) {
                        operation.gate = .BUFFER;
                        state = .output;
                    } else if (std.mem.eql(u8, token, "AND")) {
                        operation.gate = .AND;
                        state = .operand2;
                    } else if (std.mem.eql(u8, token, "OR")) {
                        operation.gate = .OR;
                        state = .operand2;
                    } else if (std.mem.eql(u8, token, "LSHIFT")) {
                        operation.gate = .LSHIFT;
                        state = .operand2;
                    } else if (std.mem.eql(u8, token, "RSHIFT")) {
                        operation.gate = .RSHIFT;
                        state = .operand2;
                    }
                },
                .operand2 => {
                    operation.op2 = Operand.parse(token);
                    state = .arrow;
                },
                .arrow => state = .output,
                .output => output = token,
            }
        }
        _ = try outputs.put(output, operation);
    }

    var cache1 = WireCache.init(problem.allocator, outputs);
    defer cache1.deinit();
    const part1 = outputs.getPtr("a").?.evaluate(&cache1);

    outputs.getPtr("b").?.op1.constant = part1;
    var cache2 = WireCache.init(problem.allocator, outputs);
    defer cache2.deinit();
    const part2 = outputs.getPtr("a").?.evaluate(&cache2);

    return problem.solution(part1, part2);
}
