const aoc = @import("../aoc.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Player = struct {
    const Deck = std.ArrayList(u8);

    deck: Deck,
    player2: bool,
    last_card: u8 = undefined,

    fn fromGroup(allocator: *Allocator, group: []const u8) !Player {
        var deck = Deck.init(allocator);
        var tokenizer = std.mem.tokenize(group, "\n");
        const player_row = tokenizer.next().?;
        const player2 = player_row[player_row.len - 2] == '2';
        while (tokenizer.next()) |card| {
            try deck.append(try std.fmt.parseInt(u8, card, 10));
        }
        return Player { .deck = deck, .player2 = player2 };
    }

    fn deinit(self: *Player) void {
        self.deck.deinit();
    }

    fn popFirst(self: *Player) void {
        self.last_card = self.deck.orderedRemove(0);
    }

    fn stealCard(self: *Player, other: *const Player) !void {
        try self.deck.append(self.last_card);
        try self.deck.append(other.last_card);
    }

    fn score(self: *const Player) usize {
        var winning_score: usize = 0;
        for (self.deck.items) |card, idx| {
            winning_score += card * (self.deck.items.len - idx);
        }
        return winning_score;
    }

    fn copy(self: *const Player) !Player {
        var deck = Deck.init(self.deck.allocator);
        try deck.appendSlice(self.deck.items);
        return Player { .deck = deck, .player2 = self.player2 };
    }
};

const Combat = struct {
    player1: Player,
    player2: Player,

    fn init(player1: *const Player, player2: *const Player) !Combat {
        return Combat { .player1 = try player1.copy(), .player2 = try player2.copy() };
    }

    fn deinit(self: *Combat) void {
        self.player1.deinit();
        self.player2.deinit();
    }

    fn play(self: *Combat) !*const Player {
        while(true) {
            if (self.checkEmptyDeck()) |winner| {
                return winner;
            }

            self.player1.popFirst();
            self.player2.popFirst();
            try self.checkLargestCard();
        }
    }

    fn checkEmptyDeck(self: *const Combat) ?*const Player {
        if (self.player1.deck.items.len == 0) {
            return &self.player2;
        }
        if (self.player2.deck.items.len == 0) {
            return &self.player1;
        }
        return null;
    }

    fn checkLargestCard(self: *Combat) !void {
        if (self.player1.last_card > self.player2.last_card) {
            try self.player1.stealCard(&self.player2);
        }
        else {
            try self.player2.stealCard(&self.player1);
        }
    }
};

const RecursiveCombat = struct {
    const CardArrangement = struct { deck1: []const u8, deck2: []const u8 };
    const CardsSeen = std.ArrayList(CardArrangement);

    arena: ArenaAllocator,
    combat: Combat,
    cards_seen: CardsSeen,

    fn init(allocator: *Allocator, player1: *const Player, player2: *const Player) !RecursiveCombat {
        return RecursiveCombat {
            .arena = ArenaAllocator.init(allocator),
            .combat = try Combat.init(player1, player2),
            .cards_seen = CardsSeen.init(allocator),
        };
    }

    fn deinit(self: *RecursiveCombat) void {
        self.cards_seen.deinit();
        self.combat.deinit();
        self.arena.deinit();
    }

    fn play(self: *RecursiveCombat) anyerror!*const Player {
        while (true) {
            // std.debug.print("Round {d}\n", .{self.cards_seen.items.len});
            if (self.combat.checkEmptyDeck()) |winner| {
                return winner;
            }

            for (self.cards_seen.items) |cards_seen| {
                if (std.mem.eql(u8, cards_seen.deck1, self.combat.player1.deck.items) and std.mem.eql(u8, cards_seen.deck2, self.combat.player2.deck.items)) {
                    // std.debug.print("!!!DUPE DECK!!!\n", .{});
                    return &self.combat.player1;
                }
            }
            try self.cards_seen.append(.{
                .deck1 = try self.arena.allocator.dupe(u8, self.combat.player1.deck.items),
                .deck2 = try self.arena.allocator.dupe(u8, self.combat.player2.deck.items),
            });

            self.combat.player1.popFirst();
            self.combat.player2.popFirst();
            if (self.combat.player1.last_card <= self.combat.player1.deck.items.len and self.combat.player2.last_card <= self.combat.player2.deck.items.len) {
                // std.debug.print(">>>\n", .{});
                var subgame = try RecursiveCombat.init(self.arena.child_allocator, &self.combat.player1, &self.combat.player2);
                defer subgame.deinit();
                subgame.combat.player1.deck.shrinkRetainingCapacity(self.combat.player1.last_card);
                subgame.combat.player2.deck.shrinkRetainingCapacity(self.combat.player2.last_card);
                if ((try subgame.play()).player2) {
                    try self.combat.player2.stealCard(&self.combat.player1);
                }
                else {
                    try self.combat.player1.stealCard(&self.combat.player2);
                }
                // std.debug.print("<<<\n", .{});
            }
            else {
                try self.combat.checkLargestCard();
            }
        }
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var player1 = try Player.fromGroup(problem.allocator, problem.group().?);
    defer player1.deinit();
    var player2 = try Player.fromGroup(problem.allocator, problem.group().?);
    defer player2.deinit();

    var combat = try Combat.init(&player1, &player2);
    defer combat.deinit();
    const res1 = (try combat.play()).score();

    var recursive_combat = try RecursiveCombat.init(problem.allocator, &player1, &player2);
    defer recursive_combat.deinit();
    const res2 = (try recursive_combat.play()).score();

    return problem.solution(res1, res2);
}
