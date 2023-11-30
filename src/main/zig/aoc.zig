const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Problem = struct {
    allocator: Allocator,
    input: [:0]const u8,
    tokenizer: ?std.mem.TokenIterator(u8, .any) = null,
    splitter: ?std.mem.SplitIterator(u8, .sequence) = null,

    pub fn init(allocator: Allocator, input: [*:0]const u8) Problem {
        return Problem{
            .allocator = allocator,
            .input = std.mem.span(input),
        };
    }

    pub fn deinit(self: *Problem) void {
        self.allocator.free(self.input);
    }

    pub fn line(self: *Problem) ?[]const u8 {
        if (self.tokenizer == null) {
            self.tokenizer = std.mem.tokenizeAny(u8, self.input, "\n");
        }
        return self.tokenizer.?.next();
    }

    pub fn group(self: *Problem) ?[]const u8 {
        if (self.splitter == null) {
            self.splitter = std.mem.splitSequence(u8, self.input, "\n\n");
        }
        return self.splitter.?.next();
    }

    pub fn solution(self: *Problem, s1: anytype, s2: anytype) Solution {
        const fmt1 = comptime if (std.meta.trait.isZigString(@TypeOf(s1))) "{s}" else "{any}";
        const fmt2 = comptime if (std.meta.trait.isZigString(@TypeOf(s2))) "{s}" else "{any}";
        return .{
            .s1 = std.fmt.allocPrint(self.allocator, fmt1, .{s1}) catch unreachable,
            .s2 = std.fmt.allocPrint(self.allocator, fmt2, .{s2}) catch unreachable,
        };
    }
};

pub const Solution = struct {
    s1: []const u8,
    s2: []const u8,

    pub fn deinit(self: *Solution, allocator: Allocator) void {
        allocator.free(self.s1);
        allocator.free(self.s2);
    }
};

pub usingnamespace @import("lib/conway.zig");
pub usingnamespace @import("lib/coord.zig");
pub usingnamespace @import("lib/matrix.zig");
pub usingnamespace @import("lib/multimap.zig");
pub usingnamespace @import("lib/permutator.zig");
pub usingnamespace @import("lib/regex.zig");
pub usingnamespace @import("lib/table.zig");
