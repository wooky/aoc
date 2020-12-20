const aoc = @import("../aoc.zig");
const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;

const RawRules = struct {
    const RuleToken = union(enum) {
        None: void,
        Goto: []const u8,
        RepeatFirst: void,
    };
    const Rule = union(enum) {
        Literal: PatternMap,
        Unprocessed: struct {
            first: [2]RuleToken,
            second: ?[3]RuleToken,
        },
    };
    const None = RuleToken { .None = {} };

    const RuleMap = std.StringHashMap(Rule);
    const PatternMap = std.StringHashMap(void);

    arena: ArenaAllocator = undefined,
    rule_map: RuleMap = undefined,

    fn init(self: *RawRules, allocator: *std.mem.Allocator) void {
        self.arena = ArenaAllocator.init(allocator);
        self.rule_map = RuleMap.init(&self.arena.allocator);
    }

    fn deinit(self: *RawRules) void {
        self.rule_map.deinit();
        self.arena.deinit();
    }

    fn addRule(self: *RawRules, line: []const u8) !void {
        const colon = std.mem.indexOf(u8, line, ":").?;
        const raw_rule = line[colon + 2..];
        const rule = blk: {
            if (raw_rule[0] == '"') {
                var map = PatternMap.init(&self.arena.allocator);
                try map.put(raw_rule[1..2], {});
                break :blk Rule { .Literal = map };
            }
            else if (std.mem.indexOf(u8, raw_rule, " |")) |pipe| {
                break :blk Rule { .Unprocessed = .{
                    .first = parseSingleRule(2, raw_rule[0..pipe]),
                    .second = parseSingleRule(3, raw_rule[pipe + 3..]),
                } };
            }
            else {
                break :blk Rule { .Unprocessed = .{
                    .first = parseSingleRule(2, raw_rule),
                    .second = null,
                } };
            }
        };
        try self.rule_map.put(line[0..colon], rule);
    }

    fn parseSingleRule(comptime size: u8, chunk: []const u8) [size]RuleToken {
        var rule_tokens: [size]RuleToken = undefined;
        var idx: u8 = 0;
        var tokens = std.mem.tokenize(chunk, " ");
        while (tokens.next()) |token| : (idx += 1) {
            rule_tokens[idx] = .{ .Goto = token };
        }
        while (idx < size) : (idx += 1) {
            rule_tokens[idx] = None;
        }
        return rule_tokens;
    }

    fn formValidMessages(self: *RawRules) !aoc.Regex {
        const rule0 = self.rule_map.get("0").?.Unprocessed.first;

        var pattern = std.ArrayList(u8).init(&self.arena.allocator);
        defer pattern.deinit();
        try pattern.appendSlice("^(");

        {
            var iter = (try self.formMessage(rule0[0].Goto)).iterator();
            while (iter.next()) |message| {
                try pattern.appendSlice(message.key);
                try pattern.append('|');
            }
            _ = pattern.pop();
        }
        try pattern.appendSlice(")(");
        {
            var iter = (try self.formMessage(rule0[1].Goto)).iterator();
            while (iter.next()) |message| {
                try pattern.appendSlice(message.key);
                try pattern.append('|');
            }
            _ = pattern.pop();
        }
        try pattern.appendSlice(")$\x00");
        
        return aoc.Regex.compilez(@ptrCast([*c]const u8, pattern.items));
    }

    fn formMessage(self: *RawRules, rule_idx: []const u8) anyerror!PatternMap {
        switch (self.rule_map.get(rule_idx).?) {
            .Literal => |l| return l,
            .Unprocessed => |rule| {
                var map = PatternMap.init(&self.arena.allocator);
                try appendMap(&map, try self.formMessageFromSingleRule(&rule.first, null));
                if (rule.second) |s| {
                    try appendMap(&map, try self.formMessageFromSingleRule(&s, &map));
                }
                try self.rule_map.put(rule_idx, .{ .Literal = map });
                return map;
            }
        }
    }

    fn formMessageFromSingleRule(self: *RawRules, rules: []const RuleToken, first: ?*const PatternMap) anyerror!PatternMap {
        var left_map = PatternMap.init(&self.arena.allocator);
        try left_map.put("", {}); // Need at least one item to combine with something

        for (rules) |rule| {
            var right_map: PatternMap = undefined;
            switch (rule) {
                .None => break,
                .Goto => |g| right_map = try self.formMessage(g),
                .RepeatFirst => right_map = first.?.*,
            }
            var new_map = PatternMap.init(&self.arena.allocator);
            var left_iterator = left_map.iterator();
            std.debug.print("Go through {} * {} values\n", .{left_map.count(), right_map.count()});
            while (left_iterator.next()) |left| {
                var right_iterator = right_map.iterator();
                while (right_iterator.next()) |right| {
                    const pattern =
                        if (rule == .RepeatFirst)
                            try std.fmt.allocPrint(&self.arena.allocator, "{}({})+", .{left.key, right.key})
                        else
                            try std.fmt.allocPrint(&self.arena.allocator, "{}{}", .{left.key, right.key})
                        ;
                    try new_map.put(pattern, {});
                }
            }
            left_map.deinit();
            left_map = new_map;
        }

        return left_map;
    }

    fn appendMap(dest: *PatternMap, src: PatternMap) !void {
        var iter = src.iterator();
        while (iter.next()) |kv| {
            try dest.put(kv.key, {});
        }
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var raw_rules1 = RawRules {};
    raw_rules1.init(problem.allocator);

    var raw_rules2 = RawRules {};
    raw_rules2.init(problem.allocator);

    var rule_lines = std.mem.tokenize(problem.group().?, "\n");
    while (rule_lines.next()) |line| {
        try raw_rules1.addRule(line);
        try raw_rules2.addRule(line);
    }
    try raw_rules2.rule_map.put("8", .{ .Unprocessed = .{
        .first = [_]RawRules.RuleToken { .{ .Goto = "42" }, RawRules.None },
        .second = [_]RawRules.RuleToken { .{ .Goto = "42" }, .{ .RepeatFirst = {} }, RawRules.None }
    } });
    try raw_rules2.rule_map.put("11", .{ .Unprocessed = .{
        .first = [_]RawRules.RuleToken { .{ .Goto = "42" }, .{ .Goto = "31" } },
        .second = [_]RawRules.RuleToken { .{ .Goto = "42" }, .{ .RepeatFirst = {} }, .{ .Goto = "31" } },
    } });

    var res1: usize = 0;
    var valid_messages1 = try raw_rules1.formValidMessages();
    raw_rules1.deinit();
    defer valid_messages1.deinit();

    var res2: usize = 0;
    var valid_messages2 = try raw_rules2.formValidMessages();
    raw_rules2.deinit();
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
