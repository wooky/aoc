const c = @cImport(@cInclude("gsl/gsl_permutation.h"));
const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Permutator(comptime T: type) type {
    return struct {
        const Self = @This();
        const Elements = std.ArrayList(T);

        elements: Elements,
        permutation: *c.gsl_permutation,
        last_permutation_result: c_int = undefined,

        pub fn initUnprepared(allocator: *Allocator) !Self {
            return Self {
                .elements = Elements.init(allocator),
                .permutation = c.gsl_permutation_alloc(0),
            };
        }

        pub fn fromHashMapKeys(allocator: *Allocator, map: anytype) !Self {
            var permutator = try initUnprepared(allocator);
            var iterator = map.iterator();
            while (iterator.next()) |kv| {
                try permutator.elements.append(kv.key_ptr.*);
            }
            permutator.reset();
            return permutator;
        }

        pub fn reset(self: *Self) void {
            if (c.gsl_permutation_size(self.permutation) != self.elements.items.len) {
                c.gsl_permutation_free(self.permutation);
                self.permutation = c.gsl_permutation_alloc(self.elements.items.len);
            }
            c.gsl_permutation_init(self.permutation);
            self.last_permutation_result = c.GSL_SUCCESS;
        }

        pub fn next(self: *Self) ?PermutationAccessor(T) {
            if (self.last_permutation_result != c.GSL_SUCCESS) {
                return null;
            }

            const accessor = PermutationAccessor(T) { .elements = self.elements.items, .permutation = self.permutation };
            self.last_permutation_result = c.gsl_permutation_next(self.permutation);
            return accessor;
        }

        pub fn deinit(self: *Self) void {
            self.elements.deinit();
            c.gsl_permutation_free(self.permutation);
        }
    };
}

pub fn PermutationAccessor(comptime T: type) type {
    return struct {
        const Self = @This();

        elements: []T,
        permutation: c.gsl_permutation,

        pub fn size(self: Self) usize {
            return self.elements.len;
        }

        pub fn get(self: Self, idx: usize) T {
            return self.elements[c.gsl_permutation_get(self.permutation, idx)];
        }
    };
}
