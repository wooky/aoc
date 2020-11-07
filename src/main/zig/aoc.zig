const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Problem = struct {
    input: []const u8 = undefined, allocator: *Allocator,
    buf: []u8, tokenizer: ?std.mem.TokenIterator = null,

    pub fn init(year: u16, day: u16, allocator: *Allocator) !Problem {
        var problem = Problem { .allocator = allocator, .buf = try allocator.alloc(u8, 65536) };
        var buf: [32]u8 = undefined;
        const filename = try std.fmt.bufPrint(&buf, "input/{}/day{:0>2}.txt", .{year, day});
        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();
        problem.input = problem.buf[0..try file.read(problem.buf)];
        return problem;
    }

    pub fn deinit(self: *Problem) void {
        self.allocator.free(self.buf);
    }

    pub fn line(self: *Problem) ?[]const u8 {
        if (self.tokenizer == null) {
            self.tokenizer = std.mem.tokenize(self.input, "\n");
        }
        return self.tokenizer.?.next();
    }
};

pub const Solution = struct { p1: usize, p2: usize, s2: ?[]const u8 = null };

pub const StringTable = @import("lib/table.zig").StringTable;
pub const StringMultimap = @import("lib/multimap.zig").StringMultimap;
pub const Permutator = @import("lib/permutator.zig").Permutator;
