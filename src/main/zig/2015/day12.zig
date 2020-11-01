const aoc = @import("../aoc.zig");
const std = @import("std");

const Solution = struct {
    all: i64 = 0, nonred: i64 = 0,
    
    fn append(self: *Solution, other: Solution) void {
        self.all += other.all;
        self.nonred += other.nonred;
    }

    fn apply_red(self: *Solution, red: bool) *Solution {
        if (red) {
            self.nonred = 0;
        }
        return self;
    }
};

pub fn run(problem: *aoc.Problem) !void {
    var json = std.json.Parser.init(problem.allocator, false);
    defer json.deinit();
    const solution = switch ((try json.parse(problem.input)).root) {
        std.json.Value.Array => |arr| parse_array(arr),
        else => unreachable,
    };
    std.debug.warn("{}\n{}\n", .{solution.all, solution.nonred});
}

fn parse_object(obj: std.json.ObjectMap) Solution {
    var solution = Solution {};
    var red = false;
    var iter = obj.iterator();
    while (iter.next()) |kv| {
        solution.append(switch (kv.value) {
            std.json.Value.Object => |o| parse_object(o),
            std.json.Value.Array => |a| parse_array(a),
            std.json.Value.Integer => |i| Solution { .all = i, .nonred = i },
            std.json.Value.String => |s| blk: {
                if (std.mem.eql(u8, s, "red")) {
                    red = true;
                }
                break :blk Solution {};
            },
            else => Solution {},
        });
    }
    return solution.apply_red(red).*;
}

fn parse_array(arr: std.json.Array) Solution {
    var solution = Solution {};
    for (arr.items) |item| {
        solution.append(switch (item) {
            std.json.Value.Array => |a| parse_array(a),
            std.json.Value.Object => |o| parse_object(o),
            std.json.Value.Integer => |i| Solution { .all = i, .nonred = i },
            else => Solution {},
        });
    }
    return solution;
}
