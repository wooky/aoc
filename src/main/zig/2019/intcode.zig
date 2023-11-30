const std = @import("std");
const Allocator = std.mem.Allocator;

pub const State = struct {
    const Memory = std.AutoHashMap(TapeIndex, TapeElement);
    const Inputs = std.ArrayList(TapeElement);

    memory: Memory,
    inputs: Inputs,
    idx: TapeIndex = 0,
    inputs_idx: usize = 0,
    relative_base_offset: TapeElement = 0,

    fn init(allocator: Allocator) State {
        return State{ .memory = Memory.init(allocator), .inputs = Inputs.init(allocator) };
    }

    pub fn deinit(self: *State) void {
        self.memory.deinit();
        self.inputs.deinit();
    }
};

const Self = @This();
pub const TapeElement = i64;
pub const TapeIndex = usize;
const Opcode = usize;
const Tape = std.ArrayList(TapeElement);

allocator: Allocator,
tape: Tape,

pub fn init(allocator: std.mem.Allocator, code: []const u8) !Self {
    var intcode = Self{ .allocator = allocator, .tape = Tape.init(allocator) };
    var tokens = std.mem.tokenize(u8, code, ",\n");
    while (tokens.next()) |token| {
        try intcode.tape.append(try std.fmt.parseInt(TapeElement, token, 10));
    }
    return intcode;
}

pub fn deinit(self: *Self) void {
    self.tape.deinit();
}

pub fn newState(self: *const Self) State {
    return State.init(self.allocator);
}

pub fn run(self: *const Self, state: *State) !?TapeElement {
    while (true) {
        const op = @as(Opcode, @intCast(self.getMemory(state, state.idx)));
        const output = try switch (op % 100) {
            1 => self.math(state, op, std.math.add),
            2 => self.math(state, op, std.math.mul),
            3 => self.readInputIntoMemory(state, op),
            4 => self.outputFromMemory(state, op),
            5 => self.jump(state, op, true),
            6 => self.jump(state, op, false),
            7 => self.cmp(state, op, std.math.CompareOperator.lt),
            8 => self.cmp(state, op, std.math.CompareOperator.eq),
            9 => self.adj_relative_base(state, op),
            99 => return null,
            else => unreachable,
        };
        if (output) |o| {
            return o;
        }
    }
}

fn math(self: *const Self, state: *State, opcode: Opcode, comptime alu_op: fn (comptime type, anytype, anytype) TapeElement) !?TapeElement {
    const p1 = self.getMemoryByOpcode(state, state.idx + 1, opcode / 100);
    const p2 = self.getMemoryByOpcode(state, state.idx + 2, opcode / 1000);
    const res = try alu_op(TapeElement, p1, p2);
    const dest = self.getDestByOpcode(state, state.idx + 3, opcode / 10000);
    _ = try state.memory.put(dest, res);
    state.idx += 4;
    return null;
}

fn readInputIntoMemory(self: *const Self, state: *State, opcode: Opcode) !?TapeElement {
    const value = state.inputs.items[state.inputs_idx];
    state.inputs_idx += 1;
    const dest = self.getDestByOpcode(state, state.idx + 1, opcode / 100);
    _ = try state.memory.put(dest, value);
    state.idx += 2;
    return null;
}

fn outputFromMemory(self: *const Self, state: *State, opcode: Opcode) !?TapeElement {
    const output = self.getMemoryByOpcode(state, state.idx + 1, opcode / 100);
    state.idx += 2;
    return output;
}

fn jump(self: *const Self, state: *State, opcode: Opcode, nonzero: bool) !?TapeElement {
    const param = self.getMemoryByOpcode(state, state.idx + 1, opcode / 100);
    if ((param != 0) == nonzero) {
        state.idx = @as(TapeIndex, @intCast(self.getMemoryByOpcode(state, state.idx + 2, opcode / 1000)));
    } else {
        state.idx += 3;
    }
    return null;
}

fn cmp(self: *const Self, state: *State, opcode: Opcode, operator: std.math.CompareOperator) !?TapeElement {
    const p1 = self.getMemoryByOpcode(state, state.idx + 1, opcode / 100);
    const p2 = self.getMemoryByOpcode(state, state.idx + 2, opcode / 1000);
    const res: TapeElement = if (std.math.compare(p1, operator, p2)) 1 else 0;
    const dest = self.getDestByOpcode(state, state.idx + 3, opcode / 10000);
    _ = try state.memory.put(dest, res);
    state.idx += 4;
    return null;
}

fn adj_relative_base(self: *const Self, state: *State, opcode: Opcode) !?TapeElement {
    const param = self.getMemoryByOpcode(state, state.idx + 1, opcode / 100);
    state.relative_base_offset += param;
    state.idx += 2;
    return null;
}

pub fn getMemory(self: *const Self, state: *const State, idx: TapeIndex) TapeElement {
    return state.memory.get(idx) orelse
        if (idx < self.tape.items.len) self.tape.items[idx] else 0;
}

fn getMemoryByOpcode(self: *const Self, state: *const State, idx: TapeIndex, opcode: Opcode) TapeElement {
    return switch (opcode % 10) {
        0 => self.getMemory(state, @as(TapeIndex, @intCast(self.getMemory(state, idx)))),
        1 => self.getMemory(state, idx),
        2 => self.getMemory(state, @as(TapeIndex, @intCast(self.getMemory(state, idx) + state.relative_base_offset))),
        else => unreachable,
    };
}

fn getDestByOpcode(self: *const Self, state: *const State, idx: TapeIndex, opcode: Opcode) TapeIndex {
    return @as(TapeIndex, @intCast(switch (opcode % 10) {
        0 => self.getMemory(state, idx),
        2 => self.getMemory(state, idx) + state.relative_base_offset,
        else => unreachable,
    }));
}
