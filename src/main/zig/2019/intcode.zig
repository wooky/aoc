const std = @import("std");

const Self = @This();
pub const TapeElement = i64;
pub const TapeIndex = usize;
const Tape = std.ArrayList(TapeElement);
const Memory = std.AutoHashMap(TapeIndex, TapeElement);

tape: Tape, memory: Memory,

pub fn init(allocator: *std.mem.Allocator, input: []const u8) !Self {
    var intcode = Self{ .tape = Tape.init(allocator), .memory = Memory.init(allocator) };
    var tokens = std.mem.tokenize(input, ",\n");
    while (tokens.next()) |token| {
        try intcode.tape.append(try std.fmt.parseInt(TapeElement, token, 10));
    }
    return intcode;
}

pub fn deinit(self: *Self) void {
    self.tape.deinit();
}

pub fn run(self: *Self) !void {
    var idx: TapeIndex = 0;
    while (true) {
        const op = self.getMemory(idx);
        idx += try switch (op) {
            1 => self.add(idx),
            2 => self.mult(idx),
            99 => break,
            else => {std.debug.warn("{}@{}", .{op, idx});unreachable;}
        };
    }
}

fn add(self: *Self, idx: TapeIndex) !TapeIndex {
    const p1 = self.getMemoryPositionMode(idx + 1);
    const p2 = self.getMemoryPositionMode(idx + 2);
    const res = p1 + p2;
    const dest = @intCast(TapeIndex, self.getMemory(idx + 3));
    try self.setMemory(dest, res);
    return 4;
}

fn mult(self: *Self, idx: TapeIndex) !TapeIndex {
    const p1 = self.getMemoryPositionMode(idx + 1);
    const p2 = self.getMemoryPositionMode(idx + 2);
    const res = p1 * p2;
    const dest = @intCast(TapeIndex, self.getMemory(idx + 3));
    try self.setMemory(dest, res);
    return 4;
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

pub fn reset(self: *Self) void {
    self.memory.clear();
}
