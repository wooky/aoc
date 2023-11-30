const aoc = @import("../aoc.zig");
const std = @import("std");
const Ticket = std.ArrayList(u16);

const Field = struct {
    name: []const u8,
    from1: u16,
    to1: u16,
    from2: u16,
    to2: u16,

    fn contains(self: *const Field, what: u16) bool {
        return (what >= self.from1 and what <= self.to1) or (what >= self.from2 and what <= self.to2);
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    const fields = blk: {
        var fields = std.ArrayList(Field).init(problem.allocator);
        const rules = problem.group().?;
        var lines = std.mem.tokenize(u8, rules, "\n");
        while (lines.next()) |line| {
            const rule = line[0..std.mem.indexOf(u8, line, ":").?];
            var range_tokens = std.mem.tokenize(u8, line[rule.len..], ":- or");
            try fields.append(Field{
                .name = rule,
                .from1 = try std.fmt.parseInt(u16, range_tokens.next().?, 10),
                .to1 = try std.fmt.parseInt(u16, range_tokens.next().?, 10),
                .from2 = try std.fmt.parseInt(u16, range_tokens.next().?, 10),
                .to2 = try std.fmt.parseInt(u16, range_tokens.next().?, 10),
            });
        }
        break :blk fields;
    };
    defer fields.deinit();

    var possible_fields = blk: {
        var possible_fields = std.ArrayList(std.StringHashMap(void)).init(problem.allocator);
        for (fields.items) |_| {
            var index_fields = std.StringHashMap(void).init(problem.allocator);
            for (fields.items) |field| {
                try index_fields.put(field.name, {});
            }
            try possible_fields.append(index_fields);
        }
        break :blk possible_fields;
    };
    defer {
        for (possible_fields.items) |*pi| {
            pi.deinit();
        }
        possible_fields.deinit();
    }

    const your_ticket = blk: {
        const group = problem.group().?;
        const newline = std.mem.indexOf(u8, group, "\n").? + 1;
        break :blk try parseTicket(problem.allocator, group[newline..]);
    };
    defer your_ticket.deinit();

    const res1 = blk: {
        var error_rate: usize = 0;
        const nearby_tix = problem.group().?;
        var lines = std.mem.tokenize(u8, nearby_tix, "\n");
        _ = lines.next().?;
        while (lines.next()) |line| {
            const ticket = try parseTicket(problem.allocator, line);
            defer ticket.deinit();
            var is_ticket_valid = true;
            outer: for (ticket.items) |ticket_field| {
                for (fields.items) |field| {
                    if (field.contains(ticket_field)) {
                        continue :outer;
                    }
                }
                is_ticket_valid = false;
                error_rate += ticket_field;
            }
            if (!is_ticket_valid) {
                continue;
            }

            for (ticket.items, 0..) |ticket_field, idx| {
                for (fields.items) |field| {
                    if (!field.contains(ticket_field)) {
                        _ = possible_fields.items[idx].remove(field.name);
                    }
                }
            }
        }
        break :blk error_rate;
    };

    const res2 = blk: {
        var departures: usize = 1;
        var processed: u8 = 0;
        outer: while (processed != possible_fields.items.len) : (processed += 1) {
            for (possible_fields.items, 0..) |*pi, idx| {
                if (pi.count() == 1) {
                    var iter = pi.iterator();
                    const field = iter.next().?.key_ptr.*;
                    for (possible_fields.items) |*pi2| {
                        _ = pi2.remove(field);
                    }
                    if (std.mem.startsWith(u8, field, "departure")) {
                        departures *= your_ticket.items[idx];
                    }
                    continue :outer;
                }
            }
            unreachable;
        }
        break :blk departures;
    };

    return problem.solution(res1, res2);
}

fn parseTicket(allocator: std.mem.Allocator, line: []const u8) !Ticket {
    var ticket = Ticket.init(allocator);
    var tokens = std.mem.tokenize(u8, line, ",");
    while (tokens.next()) |token| {
        try ticket.append(try std.fmt.parseInt(u16, token, 10));
    }
    return ticket;
}
