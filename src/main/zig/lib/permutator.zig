const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Permutator(comptime T: type) type {
    return struct {
        const Self = @This();
        const Elements = std.ArrayList(T);

        elements: Elements, started: bool = undefined, completed: bool = undefined,
        c: [32]usize = undefined, i: usize = undefined,

        pub fn init(allocator: *Allocator) !Self {
            var permutator = Self { .elements = Elements.init(allocator) };
            permutator.reset();
            return permutator;
        }

        pub fn fromHashMapKeys(allocator: *Allocator, comptime M: type, map: M) !Self {
            var permutator = try init(allocator);
            var iterator = map.iterator();
            while (iterator.next()) |kv| {
                try permutator.elements.append(kv.key);
            }
            return permutator;
        }

        pub fn reset(self: *Self) void {
            self.started = false;
            self.completed = false;
            self.c = [_]usize{0} ** 32;
            self.i = 0;
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
