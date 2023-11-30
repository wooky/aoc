const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn StringTable(comptime V: type) type {
    return struct {
        const Self = @This();
        const StringBackingSubmap = std.StringHashMap(V);
        const StringBackingMap = std.StringHashMap(StringBackingSubmap);

        backing: StringBackingMap,

        pub fn init(allocator: Allocator) Self {
            return Self{ .backing = StringBackingMap.init(allocator) };
        }

        pub fn get(self: Self, k1: []const u8, k2: []const u8) ?V {
            const sub = self.backing.get(k1) orelse return null;
            return sub.get(k2);
        }

        pub fn put(self: *Self, k1: []const u8, k2: []const u8, v: V) !void {
            var opt_submap = self.backing.getPtr(k1);
            if (opt_submap) |submap| {
                _ = try submap.put(k2, v);
            } else {
                var submap = StringBackingSubmap.init(self.backing.allocator);
                _ = try submap.put(k2, v);
                _ = try self.backing.put(k1, submap);
            }
        }

        pub fn iterator(self: Self) StringBackingMap.Iterator {
            return self.backing.iterator();
        }

        pub fn deinit(self: Self) void {
            var backing = self.backing;
            var iter = backing.iterator();
            while (iter.next()) |kv| {
                kv.value_ptr.deinit();
            }
            backing.deinit();
        }
    };
}
