const aoc = @import("../aoc.zig");
const std = @import("std");

const Command = union(enum) {
    // RotateBased tables for password.len = 8
    const rotate_based_table = [_]u8{ 7, 6, 5, 4, 2, 1, 0, 7 };
    const inverse_rotate_based_table = [_]u8{ 1, 1, 6, 2, 7, 3, 0, 4 };

    SwapPosition: struct { pos_x: u8, pos_y: u8 },
    SwapLetter: struct { letter_x: u8, letter_y: u8 },
    RotateLeft: u8,
    RotateRight: u8,
    RotateBased: u8,
    Reverse: struct { pos_x: u8, pos_y: u8 },
    Move: struct { pos_x: u8, pos_y: u8 },
    InverseRotateBased: u8,

    fn process(self: Command, password: []u8) void {
        switch (self) {
            .SwapPosition => |sp| std.mem.swap(u8, &password[sp.pos_x], &password[sp.pos_y]),
            .SwapLetter => |sl| std.mem.swap(u8, &password[std.mem.indexOfScalar(u8, password, sl.letter_x).?], &password[std.mem.indexOfScalar(u8, password, sl.letter_y).?]),
            .RotateLeft => |rl| std.mem.rotate(u8, password, rl),
            .RotateRight => |rr| std.mem.rotate(u8, password, password.len - rr),
            .RotateBased => |rb| std.mem.rotate(u8, password, rotate_based_table[std.mem.indexOfScalar(u8, password, rb).?]),
            .Reverse => |r| std.mem.reverse(u8, password[r.pos_x .. r.pos_y + 1]),
            .Move => |m| if (m.pos_x < m.pos_y) {
                std.mem.rotate(u8, password[m.pos_x .. m.pos_y + 1], 1);
            } else {
                std.mem.rotate(u8, password[m.pos_y .. m.pos_x + 1], m.pos_x - m.pos_y);
            },
            .InverseRotateBased => |irb| std.mem.rotate(u8, password, inverse_rotate_based_table[std.mem.indexOfScalar(u8, password, irb).?]),
        }
    }

    fn inverse(self: Command) Command {
        return switch (self) {
            .SwapPosition => self,
            .SwapLetter => self,
            .RotateLeft => |rl| .{ .RotateRight = rl },
            .RotateRight => |rr| .{ .RotateLeft = rr },
            .RotateBased => |rb| .{ .InverseRotateBased = rb },
            .Reverse => self,
            .Move => |m| .{ .Move = .{ .pos_x = m.pos_y, .pos_y = m.pos_x } },
            .InverseRotateBased => unreachable,
        };
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var commands = std.ArrayList(Command).init(problem.allocator);
    defer commands.deinit();
    while (problem.line()) |line| {
        const command = blk: {
            var tokens = std.mem.tokenize(u8, line, " ");
            const instr1 = tokens.next().?;
            const instr2 = tokens.next().?;
            if (std.mem.eql(u8, instr1, "swap")) {
                const x = tokens.next().?;
                _ = tokens.next().?;
                _ = tokens.next().?;
                const y = tokens.next().?;
                break :blk if (std.mem.eql(u8, instr2, "position"))
                    Command{ .SwapPosition = .{ .pos_x = try std.fmt.parseInt(u8, x, 10), .pos_y = try std.fmt.parseInt(u8, y, 10) } }
                else
                    Command{ .SwapLetter = .{ .letter_x = x[0], .letter_y = y[0] } };
            }
            if (std.mem.eql(u8, instr1, "rotate")) {
                if (std.mem.eql(u8, instr2, "based")) {
                    _ = tokens.next().?;
                    _ = tokens.next().?;
                    _ = tokens.next().?;
                    _ = tokens.next().?;
                    const x = tokens.next().?;
                    break :blk Command{ .RotateBased = x[0] };
                }
                const x = try std.fmt.parseInt(u8, tokens.next().?, 10);
                break :blk if (std.mem.eql(u8, instr2, "left"))
                    Command{ .RotateLeft = x }
                else
                    Command{ .RotateRight = x };
            }
            if (std.mem.eql(u8, instr1, "reverse")) {
                const x = try std.fmt.parseInt(u8, tokens.next().?, 10);
                _ = tokens.next().?;
                const y = try std.fmt.parseInt(u8, tokens.next().?, 10);
                break :blk Command{ .Reverse = .{ .pos_x = x, .pos_y = y } };
            }
            if (std.mem.eql(u8, instr1, "move")) {
                const x = try std.fmt.parseInt(u8, tokens.next().?, 10);
                _ = tokens.next().?;
                _ = tokens.next().?;
                const y = try std.fmt.parseInt(u8, tokens.next().?, 10);
                break :blk Command{ .Move = .{ .pos_x = x, .pos_y = y } };
            }
            unreachable;
        };
        try commands.append(command);
    }

    const s1 = blk: {
        var password: [8]u8 = undefined;
        std.mem.copy(u8, &password, "abcdefgh");
        for (commands.items) |command| {
            command.process(&password);
        }
        break :blk password;
    };

    const s2 = blk: {
        var password: [8]u8 = undefined;
        std.mem.copy(u8, &password, "fbgdceah");
        var command_idx: usize = commands.items.len - 1;
        while (true) : (command_idx -= 1) {
            commands.items[command_idx].inverse().process(&password);
            if (command_idx == 0) {
                break :blk password;
            }
        }
        unreachable;
    };

    return problem.solution(&s1, &s2);
}
