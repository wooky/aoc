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

        pub fn iterator(self: Self) StringBackingMap.Iterator {
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

pub fn Permutator(comptime T: type) type {
    return struct {
        const Self = @This();
        const Elements = std.ArrayList(T);

        elements: Elements, started: bool = false, completed: bool = false,
        c: [32]usize = [_]usize{0} ** 32, i: usize = 0,

        pub fn fromHashMapKeys(allocator: *Allocator, comptime M: type, map: M) !Self {
            var permutator = Self { .elements = Elements.init(allocator) };
            var iterator = map.iterator();
            while (iterator.next()) |kv| {
                try permutator.elements.append(kv.key);
            }
            return permutator;
        }

        // Heap's Algorithm
        // TODO make it async when https://github.com/ziglang/zig/issues/6917 gets resolved
        pub fn next(self: *Self) ?[]const T {
            if (self.completed) {
                return null;
            }

            if (!self.started) {
                self.started = true;
                return self.elements.items;
            }

            while (self.i < self.elements.items.len) {
                if (self.c[self.i] < self.i) {
                    if (self.i % 2 == 0) {
                        self.swap(0, self.i);
                    }
                    else {
                        self.swap(self.c[self.i], self.i);
                    }
                    self.c[self.i] += 1;
                    self.i = 0;
                    return self.elements.items;
                    
                }
                else {
                    self.c[self.i] = 0;
                    self.i += 1;
                }
            }

            self.completed = true;
            return null;
        }

        fn swap(self: *Self, a: usize, b: usize) void {
            var tmp = self.elements.items[a];
            self.elements.items[a] = self.elements.items[b];
            self.elements.items[b] = tmp;
        }

        pub fn deinit(self: *Self) void {
            self.elements.deinit();
        }
    };
}
