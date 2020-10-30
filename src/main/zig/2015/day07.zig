const std = @import("std");

const Operand = union(enum) {
    constant: u16, wire: []const u8,

    fn parse(op: []const u8) Operand {
        const constant = std.fmt.parseInt(u16, op, 10) catch return Operand { .wire = op };
        return Operand { .constant = constant };
    }

    fn evaluate(self: Operand, cache: *WireCache) u16 {
        return switch (self) {
            Operand.wire => |w| cache.get(w),
            Operand.constant => |c| c,
        };
    }
};

const LogicGate = enum { AND, OR, LSHIFT, RSHIFT, NOT, BUFFER };
const Operation = struct {
    op1: Operand = Operand{.constant = 0}, op2: Operand = Operand{.constant = 0}, gate: LogicGate = undefined,

    fn evaluate(self: *Operation, cache: *WireCache) u16 {
        const op1 = self.op1.evaluate(cache);
        const op2 = self.op2.evaluate(cache);

        return switch (self.gate) {
            LogicGate.AND    =>  op1 &  op2,
            LogicGate.OR     =>  op1 |  op2,
            LogicGate.LSHIFT =>  op1 << @intCast(u4, op2),
            LogicGate.RSHIFT =>  op1 >> @intCast(u4, op2),
            LogicGate.NOT    => ~op2,
            LogicGate.BUFFER =>  op1,
        };
    }
};

const OutputMap = std.StringHashMap(Operation);

const WireCache = struct {
    backing: WireCacheBacking = WireCacheBacking.init(std.heap.page_allocator),
    outputs: OutputMap,

    const WireCacheBacking = std.StringHashMap(u16);

    fn init(outputs: OutputMap) WireCache {
        return WireCache { .outputs = outputs };
    }

    fn get(self: *WireCache, output: []const u8) u16 {
        const cached_value = self.backing.getValue(output);
        if (cached_value) |v| {
            return v;
        }
        const value = self.outputs.getValue(output).?.evaluate(self);
        _ = self.backing.put(output, value) catch unreachable;
        return value;
    }
};

const TokenState = enum {
    operand1_not, gate_arrow, operand2, arrow, output
};

pub fn main() !void {
    var outputs = OutputMap.init(std.heap.page_allocator);
    defer outputs.deinit();

    var buf: [8192]u8 = undefined;
    const file = try std.fs.cwd().openFile("input/2015/day07.txt", .{});
    defer file.close();
    const size = try file.read(&buf);
    var lines = std.mem.tokenize(buf[0..size], "\n");
    while (lines.next()) |line| {
        var operation = Operation {};
        var output: []const u8 = undefined;
        var state = TokenState.operand1_not;
        var tokens = std.mem.tokenize(line, " ");
        while (tokens.next()) |token| {
            switch (state) {
                TokenState.operand1_not => {
                    if (std.mem.eql(u8, token, "NOT")) {
                        operation.gate = LogicGate.NOT;
                        state = TokenState.operand2;
                    }
                    else {
                        operation.op1 = Operand.parse(token);
                        state = TokenState.gate_arrow;
                    }
                },
                TokenState.gate_arrow => {
                    if (std.mem.eql(u8, token, "->")) {
                        operation.gate = LogicGate.BUFFER;
                        state = TokenState.output;
                    }
                    else if (std.mem.eql(u8, token, "AND")) {
                        operation.gate = LogicGate.AND;
                        state = TokenState.operand2;
                    }
                    else if (std.mem.eql(u8, token, "OR")) {
                        operation.gate = LogicGate.OR;
                        state = TokenState.operand2;
                    }
                    else if (std.mem.eql(u8, token, "LSHIFT")) {
                        operation.gate = LogicGate.LSHIFT;
                        state = TokenState.operand2;
                    }
                    else if (std.mem.eql(u8, token, "RSHIFT")) {
                        operation.gate = LogicGate.RSHIFT;
                        state = TokenState.operand2;
                    }
                },
                TokenState.operand2 => {
                    operation.op2 = Operand.parse(token);
                    state = TokenState.arrow;
                },
                TokenState.arrow => state = TokenState.output,
                TokenState.output => output = token,
            }
        }
        _ = try outputs.put(output, operation);
    }

    const part1 = outputs.getValue("a").?.evaluate(&WireCache.init(outputs));

    outputs.get("b").?.value.op1.constant = part1;
    const part2 = outputs.getValue("a").?.evaluate(&WireCache.init(outputs));

    std.debug.warn("{}\n{}\n", .{part1, part2});
}
