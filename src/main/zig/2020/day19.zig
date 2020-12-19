const aoc = @import("../aoc.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

// TODO so much leaked memory
const RawRules = struct {
    const SingleRule = struct {
        left: []const u8,
        right: ?[]const u8,
    };
    const Rule = union(enum) {
        Literal: u8,
        Single: SingleRule,
        Dual: struct {
            Rule1: SingleRule,
            Rule2: SingleRule,
        },
    };

    const RuleMap = std.StringHashMap(Rule);
    const StringBuilder = std.ArrayList(u8);
    const StringBuilderList = std.ArrayList(StringBuilder);
    const ValidMessages = std.StringHashMap(void);

    allocator: *Allocator,
    rule_map: RuleMap,

    fn init(allocator: *Allocator) RawRules {
        return RawRules {
            .allocator = allocator,
            .rule_map = RuleMap.init(allocator),
        };
    }

    fn deinit(self: *RawRules) void {
        self.rule_map.deinit();
    }

    fn addRule(self: *RawRules, line: []const u8) !void {
        const colon = std.mem.indexOf(u8, line, ":").?;
        const raw_rule = line[colon + 2..];
        const rule =
            if (raw_rule[0] == '"')
                Rule { .Literal = raw_rule[1] }
            else if (std.mem.indexOf(u8, raw_rule, " |")) |pipe|
                Rule { .Dual = .{
                    .Rule1 = parseSingleRule(raw_rule[0..pipe]),
                    .Rule2 = parseSingleRule(raw_rule[pipe + 3..]),
                } }
            else
                Rule { .Single = parseSingleRule(raw_rule) };
        try self.rule_map.put(line[0..colon], rule);
    }

    fn parseSingleRule(chunk: []const u8) SingleRule {
        return if (std.mem.indexOf(u8, chunk, " ")) |space|
            SingleRule { .left = chunk[0..space], .right = chunk[space + 1..] }
        else
            SingleRule { .left = chunk, .right = null };
    }

    fn formValidMessages(self: *RawRules) !ValidMessages {
        var messages = try self.formMessage("0");
        var valid_messages = ValidMessages.init(self.allocator);
        for (messages.items) |message| {
            try valid_messages.put(message.items, {});
        }
        return valid_messages;
    }

    fn formMessage(self: *RawRules, rule_idx: []const u8) anyerror!StringBuilderList {
        switch (self.rule_map.get(rule_idx).?) {
            .Literal => |l| {
                var list = StringBuilderList.init(self.allocator);
                var sb = StringBuilder.init(self.allocator);
                try sb.append(l);
                try list.append(sb);
                return list;
            },
            .Single => |s| return try self.formMessageFromSingleRule(s),
            .Dual => |d| {
                var list1 = try self.formMessageFromSingleRule(d.Rule1);
                var list2 = try self.formMessageFromSingleRule(d.Rule2);
                defer list2.deinit();
                try list1.appendSlice(list2.items);
                return list1;
            }
        }
    }

    fn formMessageFromSingleRule(self: *RawRules, rule: SingleRule) anyerror!StringBuilderList {
        var left_list = try self.formMessage(rule.left);
        if (rule.right) |r| {
            defer left_list.deinit();
            var list = StringBuilderList.init(self.allocator);
            var right_list = try self.formMessage(r);
            defer right_list.deinit();
            for (left_list.items) |left| {
                for (right_list.items) |right| {
                    var sb = StringBuilder.init(self.allocator);
                    try sb.appendSlice(left.items);
                    try sb.appendSlice(right.items);
                    try list.append(sb);
                }
            }
            return list;
        }
        return left_list;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var raw_rules = RawRules.init(problem.allocator);
    defer raw_rules.deinit();

    {
        var lines = std.mem.tokenize(problem.group().?, "\n");
        while (lines.next()) |line| {
            try raw_rules.addRule(line);
        }
    }

    const res1 = blk: {
        var valid_messages = try raw_rules.formValidMessages();
        defer valid_messages.deinit();

        var filtered_messages: usize = 0;
        var lines = std.mem.tokenize(problem.group().?, "\n");
        while (lines.next()) |line| {
            if (valid_messages.contains(line)) {
                filtered_messages += 1;
            }
        }
        break :blk filtered_messages;
    };

    return aoc.Solution { .p1 = res1, .p2 = 0 };
}
