const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn StringMultimap(comptime V: type) type {
    return struct {
        const Self = @This();
        const BackingSubList = std.ArrayList(V);
        const BackingMap = std.StringHashMap(BackingSubList);

        backing: BackingMap,

        pub fn init(allocator: Allocator) Self {
            return Self{ .backing = BackingMap.init(allocator) };
        }

        pub fn deinit(self: *Self) void {
            var iterator = self.backing.iterator();
            while (iterator.next()) |kv| {
                kv.value_ptr.deinit();
            }
            self.backing.deinit();
        }

        pub fn get(self: Self, key: []const u8) ?[]V {
            const sub = self.backing.get(key) orelse return null;
            return sub.items;
        }

        pub fn put(self: *Self, key: []const u8, value: V) !void {
            var opt_sublist = self.backing.getPtr(key);
            if (opt_sublist) |v| {
                try v.append(value);
            } else {
                var sublist = BackingSubList.init(self.backing.allocator);
                try sublist.append(value);
                _ = try self.backing.put(key, sublist);
            }
        }
    };
}
