const std = @import("std");

const Self = @This();
pub const TapeElement = i64;
pub const TapeIndex = usize;
const Opcode = usize;
const Tape = std.ArrayList(TapeElement);
const Memory = std.AutoHashMap(TapeIndex, TapeElement);
const Inputs = std.ArrayList(TapeElement);

tape: Tape,
memory: Memory,
inputs: Inputs,
idx: TapeIndex = undefined,
inputs_idx: usize = undefined,
output: ?TapeElement = undefined,

pub fn init(allocator: *std.mem.Allocator, code: []const u8) !Self {
    var intcode = Self{ .tape = Tape.init(allocator), .memory = Memory.init(allocator), .inputs = Inputs.init(allocator) };
    intcode.reinit();
    var tokens = std.mem.tokenize(code, ",\n");
    while (tokens.next()) |token| {
        try intcode.tape.append(try std.fmt.parseInt(TapeElement, token, 10));
    }
    return intcode;
}

pub fn deinit(self: *Self) void {
    self.tape.deinit();
    self.memory.deinit();
    self.inputs.deinit();
}

pub fn run(self: *Self) !?TapeElement {
    while (true) {
        const idx = self.idx;
        const op = @intCast(Opcode, self.getMemory(idx));
        self.idx = try switch (op % 100) {
            1 => self.math(idx, op, std.math.add),
            2 => self.math(idx, op, std.math.mul),
            3 => self.readInputIntoMemory(idx),
            4 => self.outputFromMemory(idx, op),
            5 => self.jump(idx, op, true),
            6 => self.jump(idx, op, false),
            7 => self.cmp(idx, op, std.math.CompareOperator.lt),
            8 => self.cmp(idx, op, std.math.CompareOperator.eq),
            99 => return null,
            else => unreachable
        };
        if (self.output) |o| {
            self.output = null;
            return o;
        }
    }
}

fn math(self: *Self, idx: TapeIndex, opcode: Opcode, comptime alu_op: fn(comptime type, var, var)TapeElement) !TapeIndex {
    const p1 = self.getMemoryByOpcode(idx + 1, opcode / 100);
    const p2 = self.getMemoryByOpcode(idx + 2, opcode / 1000);
    const res = try alu_op(TapeElement, p1, p2);
    const dest = @intCast(TapeIndex, self.getMemory(idx + 3));
    try self.setMemory(dest, res);
    return idx + 4;
}

fn readInputIntoMemory(self: *Self, idx: TapeIndex) !TapeIndex {
    const value = self.inputs.items[self.inputs_idx];
    self.inputs_idx += 1;
    const dest = @intCast(TapeIndex, self.getMemory(idx + 1));
    try self.setMemory(dest, value);
    return idx + 2;
}

fn outputFromMemory(self: *Self, idx: TapeIndex, opcode: Opcode) TapeIndex {
    self.output = self.getMemoryByOpcode(idx + 1, opcode / 100);
    return idx + 2;
}

fn jump(self: *Self, idx: TapeIndex, opcode: Opcode, nonzero: bool) TapeIndex {
    const param = self.getMemoryByOpcode(idx + 1, opcode / 100);
    if ((param != 0) == nonzero) {
        return @intCast(TapeIndex, self.getMemoryByOpcode(idx + 2, opcode / 1000));
    }
    return idx + 3;
}

fn cmp(self: *Self, idx: TapeIndex, opcode: Opcode, operator: std.math.CompareOperator) !TapeIndex {
    const p1 = self.getMemoryByOpcode(idx + 1, opcode / 100);
    const p2 = self.getMemoryByOpcode(idx + 2, opcode / 1000);
    const res: TapeElement = if (std.math.compare(p1, operator, p2)) 1 else 0;
    const dest = @intCast(TapeIndex, self.getMemory(idx + 3));
    try self.setMemory(dest, res);
    return idx + 4;
}

pub fn getMemory(self: *const Self, idx: TapeIndex) TapeElement {
    return self.memory.getValue(idx) orelse self.tape.items[idx];
}

pub fn setMemory(self: *Self, idx: TapeIndex, value: TapeElement) !void {
    _ = try self.memory.put(idx, value);
}

fn getMemoryPositionMode(self: *const Self, idx: TapeIndex) TapeElement {
    return self.getMemory(@intCast(TapeIndex, self.getMemory(idx)));
}

fn getMemoryByOpcode(self: *const Self, idx: TapeIndex, opcode: Opcode) TapeElement {
    return switch (opcode % 10) {
        0 => self.getMemoryPositionMode(idx),
        1 => self.getMemory(idx),
        else => unreachable
    };
}

pub fn reset(self: *Self) void {
    self.inputs.deinit();
    self.reinit();
}

fn reinit(self: *Self) void {
    self.memory.clear();
    self.inputs = Inputs.init(self.inputs.allocator);
    self.idx = 0;
    self.inputs_idx = 0;
    self.output = null;
}
