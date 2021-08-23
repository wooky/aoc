const aoc = @import("../aoc.zig");
const std = @import("std");

const Player = struct {
    const Deck = std.ArrayList(u8);

    deck: Deck,

    fn fromGroup(allocator: *std.mem.Allocator, group: []const u8) !Player {
        var deck = Deck.init(allocator);
        var tokenizer = std.mem.tokenize(group, "\n");
        _ = tokenizer.next().?;
        while (tokenizer.next()) |card| {
            try deck.append(try std.fmt.parseInt(u8, card, 10));
        }
        return Player { .deck = deck };
    }

    fn deinit(self: *Player) void {
        self.deck.deinit();
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var player1 = try Player.fromGroup(problem.allocator, problem.group().?);
    defer player1.deinit();
    var player2 = try Player.fromGroup(problem.allocator, problem.group().?);
    defer player2.deinit();

    const winning_player = blk: {
        while(true) {
            if (player1.deck.items.len == 0) {
                break :blk player2;
            }
            if (player2.deck.items.len == 0) {
                break :blk player1;
            }

            const card1 = player1.deck.orderedRemove(0);
            const card2 = player2.deck.orderedRemove(0);
            if (card1 > card2) {
                try player1.deck.appendSlice(&[_]u8{card1, card2});
            }
            else {
                try player2.deck.appendSlice(&[_]u8{card2, card1});
            }
        }
    };

    var winning_score: usize = 0;
    for (winning_player.deck.items) |card, idx| {
        winning_score += card * (winning_player.deck.items.len - idx);
    }

    return aoc.Solution { .p1 = winning_score, .p2 = 0 };
}
