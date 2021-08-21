const aoc = @import("../aoc.zig");
const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;

const RawRules = struct {
    const Rule = union(enum) {
        Literal: []const u8,
        Goto: [][][]const u8,
    };

    const RuleMap = std.StringHashMap(Rule);

    arena: ArenaAllocator = undefined,
    unprocessed_rule_map: RuleMap = undefined,
    processed_rule_map: RuleMap = undefined,

    fn init(self: *RawRules, allocator: *std.mem.Allocator) void {
        self.arena = ArenaAllocator.init(allocator);
        self.unprocessed_rule_map = RuleMap.init(&self.arena.allocator);
    }

    fn deinit(self: *RawRules) void {
        self.arena.deinit();
    }

    fn addRule(self: *RawRules, line: []const u8) !void {
        var colon = std.mem.split(line, ": ");
        const rule_idx = colon.next().?;
        const raw_rule = colon.next().?;
        const rule = blk: {
            if (raw_rule[0] == '"') {
                break :blk Rule { .Literal = raw_rule[1..2] };
            }
            else {
                var goto = std.ArrayList([][]const u8).init(&self.arena.allocator);
                var pipe = std.mem.split(raw_rule, " | ");
                while (pipe.next()) |chunk| {
                    var indexes = std.ArrayList([]const u8).init(&self.arena.allocator);
                    var tokens = std.mem.tokenize(chunk, " ");
                    while (tokens.next()) |token| {
                        try indexes.append(token);
                    }
                    try goto.append(indexes.toOwnedSlice());
                }
                break :blk Rule { .Goto = goto.toOwnedSlice() };
            }
        };
        try self.unprocessed_rule_map.put(rule_idx, rule);
    }

    fn formValidMessages(self: *RawRules) !aoc.Regex {
        self.processed_rule_map = try self.unprocessed_rule_map.clone();
        const pattern = try self.formMessage("0");
        const patternz = try std.fmt.allocPrintZ(&self.arena.allocator, "^{s}$", .{pattern});
        defer self.arena.allocator.free(patternz);
        return aoc.Regex.compilez(patternz);
    }

    fn formMessage(self: *RawRules, rule_idx: []const u8) anyerror![]const u8 {
        switch (self.processed_rule_map.get(rule_idx).?) {
            .Literal => |l| return l,
            .Goto => |goto| {
                var pattern = try std.fmt.allocPrint(
                    &self.arena.allocator,
                    "({s}",
                    .{ try self.formMessageFromSingleRule(goto[0]) }
                );
                for (goto[1..]) |rule| {
                    pattern = try std.fmt.allocPrint(
                        &self.arena.allocator,
                        "{s}|{s}",
                        .{ pattern, try self.formMessageFromSingleRule(rule) }
                    );
                }
                pattern = try std.fmt.allocPrint(&self.arena.allocator, "{s})", .{pattern});
                try self.processed_rule_map.put(rule_idx, .{ .Literal = pattern });
                return pattern;
            }
        }
    }

    fn formMessageFromSingleRule(self: *RawRules, rules: [][]const u8) anyerror![]const u8 {
        var pattern: []const u8 = &[_]u8 {};

        for (rules) |rule| {
            pattern = try std.fmt.allocPrint(
                &self.arena.allocator,
                "{s}{s}",
                .{ pattern, try self.formMessage(rule) }
            );
        }

        return pattern;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var raw_rules = RawRules {};
    raw_rules.init(problem.allocator);
    defer raw_rules.deinit();

    var rule_lines = std.mem.tokenize(problem.group().?, "\n");
    while (rule_lines.next()) |line| {
        try raw_rules.addRule(line);
    }

    var res1: usize = 0;
    var valid_messages1 = try raw_rules.formValidMessages();
    defer valid_messages1.deinit();

    var res2: usize = 0;
    try raw_rules.addRule("8: 42 | 42 42 | 42 42 42 | 42 42 42 42 | 42 42 42 42 42");
    try raw_rules.addRule("11: 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31 | 42 42 42 42 42 31 31 31 31 31");
    var valid_messages2 = try raw_rules.formValidMessages();
    defer valid_messages2.deinit();

    var message_lines = std.mem.tokenize(problem.group().?, "\n");
    while (message_lines.next()) |line| {
        if (valid_messages1.matches(line)) {
            res1 += 1;
        }
        if (valid_messages2.matches(line)) {
            res2 += 1;
        }
    }

    return aoc.Solution { .p1 = res1, .p2 = res2 };
}
