const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Problem = struct {
    input: []const u8 = undefined, allocator: *Allocator,
    buf: [65536]u8 = undefined, tokenizer: ?std.mem.TokenIterator = null,

    pub fn init(year: u16, day: u16, allocator: *Allocator) !Problem {
        var problem = Problem { .allocator = allocator };
        var buf: [32]u8 = undefined;
        const filename = try std.fmt.bufPrint(&buf, "input/{}/day{:0>2}.txt", .{year, day});
        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();
        problem.input = problem.buf[0..try file.read(&problem.buf)];
        return problem;
    }

    pub fn line(self: *Problem) ?[]const u8 {
        if (self.tokenizer == null) {
            self.tokenizer = std.mem.tokenize(self.input, "\n");
        }
        return self.tokenizer.?.next();
    }
};

pub fn StringTable(comptime V: type) type {
    return struct {
        const Self = @This();
        const StringBackingSubmap = std.StringHashMap(V);
        const StringBackingMap = std.StringHashMap(StringBackingSubmap);

        backing: StringBackingMap,

        pub fn init(allocator: *Allocator) Self {
            return Self { .backing = StringBackingMap.init(allocator) };
        }

        pub fn get(self: Self, k1: []const u8, k2: []const u8) V {
            return self.backing.getValue(k1).?.getValue(k2).?;
        }

        pub fn put(self: *Self, k1: []const u8, k2: [] const u8, v: V) !void {
            var opt_submap = self.backing.get(k1);
            if (opt_submap) |kv| {
                _ = try kv.value.put(k2, v);
            }
            else {
                var submap = StringBackingSubmap.init(std.heap.page_allocator);
                _ = try submap.put(k2, v);
                _ = try self.backing.put(k1, submap);
            }
        }

        pub fn iterator(self: *Self) StringBackingMap.Iterator {
            return self.backing.iterator();
        }

        pub fn deinit(self: Self) void {
            var iter = self.backing.iterator();
            while (iter.next()) |kv| {
                kv.value.deinit();
            }
            self.backing.deinit();
        }
    };
}
