const aoc = @import("../aoc.zig");
const std = @import("std");

const Solution = struct {
    all: i64 = 0,
    nonred: i64 = 0,

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

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var json = try std.json.parseFromSlice(std.json.Value, problem.allocator, problem.input, .{});
    defer json.deinit();
    const solution = switch (json.value) {
        .array => |arr| parse_array(arr),
        else => unreachable,
    };
    return problem.solution(solution.all, solution.nonred);
}

fn parse_object(obj: std.json.ObjectMap) Solution {
    var solution = Solution{};
    var red = false;
    var iter = obj.iterator();
    while (iter.next()) |kv| {
        solution.append(switch (kv.value_ptr.*) {
            .object => |o| parse_object(o),
            .array => |a| parse_array(a),
            .integer => |i| Solution{ .all = i, .nonred = i },
            .string => |s| blk: {
                if (std.mem.eql(u8, s, "red")) {
                    red = true;
                }
                break :blk Solution{};
            },
            else => Solution{},
        });
    }
    return solution.apply_red(red).*;
}

fn parse_array(arr: std.json.Array) Solution {
    var solution = Solution{};
    for (arr.items) |item| {
        solution.append(switch (item) {
            .array => |a| parse_array(a),
            .object => |o| parse_object(o),
            .integer => |i| Solution{ .all = i, .nonred = i },
            else => Solution{},
        });
    }
    return solution;
}
