const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Problem = struct {
    allocator: *Allocator,
    input: [:0]const u8 = undefined,
    tokenizer: ?std.mem.TokenIterator = null,
    splitter: ?std.mem.SplitIterator = null,

    pub fn init(year: u16, day: u16, allocator: *Allocator) !Problem {
        var problem = Problem { .allocator = allocator };
        var buf: [32]u8 = undefined;
        const filename = try std.fmt.bufPrint(&buf, "input/{}/day{:0>2}.txt", .{year, day});
        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();
        problem.input = try file.readToEndAllocOptions(allocator, 262144, null, @alignOf(u8), 0);
        return problem;
    }

    pub fn deinit(self: *Problem) void {
        self.allocator.free(self.input);
    }

    pub fn line(self: *Problem) ?[]const u8 {
        if (self.tokenizer == null) {
            self.tokenizer = std.mem.tokenize(self.input, "\n");
        }
        return self.tokenizer.?.next();
    }

    pub fn group(self: *Problem) ?[]const u8 {
        if (self.splitter == null) {
            self.splitter = std.mem.split(self.input, "\n\n");
        }
        return self.splitter.?.next();
    }
};

pub const Solution = struct { p1: usize, s1: ?[]const u8 = null, p2: usize, s2: ?[]const u8 = null };

usingnamespace @import("lib/coord.zig");
usingnamespace @import("lib/matrix.zig");
usingnamespace @import("lib/multimap.zig");
usingnamespace @import("lib/permutator.zig");
usingnamespace @import("lib/regex.zig");
usingnamespace @import("lib/table.zig");
