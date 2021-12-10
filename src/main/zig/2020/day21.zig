const aoc = @import("../aoc.zig");
const std = @import("std");

const Ingredient = struct {
    allergens: std.StringHashMap(void),
    occurrences: usize = 1,

    fn init(allocator: *std.mem.Allocator) Ingredient {
        return .{
            .allergens = std.StringHashMap(void).init(allocator),
        };
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var arena = std.heap.ArenaAllocator.init(problem.allocator);
    defer arena.deinit();
    var ingredients = std.StringHashMap(Ingredient).init(&arena.allocator);

    while (problem.line()) |line| {
        var line_ingredients = std.ArrayList(*Ingredient).init(&arena.allocator);
        var line_allergens = std.ArrayList([]const u8).init(&arena.allocator);
        var ingredient_listing = true;
        var tokens = std.mem.tokenize(line, " ");
        while (tokens.next()) |token| {
            if (ingredient_listing) {
                if (std.mem.eql(u8, token, "(contains")) {
                    ingredient_listing = false;
                    continue;
                }
                var entry = try ingredients.getOrPut(token);
                if (entry.found_existing) {
                    entry.value_ptr.occurrences += 1;
                }
                else {
                    entry.value_ptr.* = Ingredient.init(&arena.allocator);
                }
                try line_ingredients.append(entry.value_ptr);
            }
            else {
                const allergen = token[0..token.len - 2];
                try line_allergens.append(allergen);
            }
        }

        if (line_allergens.items.len == 1) {
            for (line_ingredients.items) |ingredient| {
                try ingredient.allergens.putNoClobber(line_allergens.items[0]);
            }
        }
    }
}
