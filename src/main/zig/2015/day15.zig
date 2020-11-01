const aoc = @import("../aoc.zig");
const std = @import("std");

const Ingredient = struct {
    capacity: i32, durability: i32, flavor: i32, texture: i32, calories: i32
};

const Params = struct {
    i: i32 = 0, j: i32 = 0, k: i32 = 0, l: i32 = 0,
    ingredients: [4]Ingredient = undefined,

    fn calcSubscore(self: Params, comptime field: []const u8) ?i32 {
        const subscore = self.i*@field(self.ingredients[0], field) + self.j*@field(self.ingredients[1], field) +
                         self.k*@field(self.ingredients[2], field) + self.l*@field(self.ingredients[3], field);
        return if (subscore > 0) subscore else null;
    }
};

pub fn run(problem: *aoc.Problem) !void {
    var params = Params {};
    var ingredients_idx: usize = 0;
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, " ");
        _ = tokens.next().?;
        params.ingredients[ingredients_idx] = Ingredient {
            .capacity = try int_from_tokens(&tokens, 1),
            .durability = try int_from_tokens(&tokens, 1),
            .flavor = try int_from_tokens(&tokens, 1),
            .texture = try int_from_tokens(&tokens, 1),
            .calories = try int_from_tokens(&tokens, 0)
        };
        ingredients_idx += 1;
    }

    var best: i32 = 0;
    var best_500: i32 = 0;
    while (params.i <= 100) : (params.i += 1) {
        params.j = 0;
        while (params.j <= 100 - params.i) : (params.j += 1) {
            params.k = 0;
            while (params.k <= 100 - params.i - params.j) : (params.k += 1) {
                params.l = 100 - params.i - params.j - params.k;
                const capacity = params.calcSubscore("capacity") orelse continue;
                const durability = params.calcSubscore("durability") orelse continue;
                const flavor = params.calcSubscore("flavor") orelse continue;
                const texture = params.calcSubscore("texture") orelse continue;
                const score = capacity * durability * flavor * texture;
                best = std.math.max(best, score);

                const calories = params.calcSubscore("calories") orelse continue;
                if (calories == 500) {
                    best_500 = std.math.max(best_500, score);
                }
            }
        }
    }
    std.debug.warn("{}\n{}\n", .{best, best_500});
}

fn int_from_tokens(tokens: *std.mem.TokenIterator, offset: usize) !i32 {
    _ = tokens.next().?;
    const token = tokens.next().?;
    return try std.fmt.parseInt(i32, token[0..token.len-offset], 10);
}
