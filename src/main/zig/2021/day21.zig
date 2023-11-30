const aoc = @import("../aoc.zig");
const std = @import("std");

const DeterministicDie = struct {
    current: u8 = 0,
    rolls: u16 = 0,

    fn roll(self: *DeterministicDie) [1]u8 {
        self.current = if (self.current == 100) 1 else self.current + 1;
        self.rolls += 1;
        return [_]u8{self.current};
    }
};

const DiracDie = struct {
    fn roll(_: *DiracDie) [3]u8 {
        return [_]u8{ 1, 2, 3 };
    }
};

const Player = struct {
    position: u8,
    score: u16 = 0,

    fn move(self: *Player, roll: u8) void {
        self.position = (self.position + roll) % 10;
    }

    fn calcScore(self: *Player) u16 {
        self.score += if (self.position == 0) 10 else self.position;
        return self.score;
    }
};

fn Game(comptime Die: type, comptime max_score: u16) type {
    return struct {
        const Self = @This();

        const Stats = struct {
            player1_wins: usize = 0,
            player2_wins: usize = 0,
            last_winning_game: Self = undefined,
        };

        die: Die = .{},
        player1: Player,
        player2: Player,
        player1_turn: bool = true,
        player_rolls: u8 = 0,

        fn init(player1_position: u8, player2_position: u8) Self {
            return .{
                .player1 = .{ .position = player1_position },
                .player2 = .{ .position = player2_position },
            };
        }

        fn play(self: *Self) Stats {
            var stats = Stats{};
            for (self.die.roll()) |roll| {
                var new_game = self.*;
                new_game.playRecursive(&stats, roll, 1);
            }
            return stats;
        }

        // TODO keep track of per-universe stats
        fn playRecursive(self: *Self, stats: *Stats, roll: u8, depth: u16) void {
            // var i: u16 = 0; while (i < depth) : (i += 1) std.debug.print(" ", .{});
            // std.debug.print("{} {} {} {} {}\n", .{roll, self.player1_turn, self.player_rolls, self.player1, self.player2});
            var current_player = if (self.player1_turn) &self.player1 else &self.player2;
            current_player.move(roll);
            self.player_rolls += 1;
            if (self.player_rolls == 3) {
                if (current_player.calcScore() >= max_score) {
                    if (self.player1_turn) stats.player1_wins += 1 else stats.player2_wins += 1;
                    stats.last_winning_game = self.*;
                    return;
                }
                self.player1_turn = !self.player1_turn;
                self.player_rolls = 0;
            }

            for (self.die.roll()) |next_roll| {
                var new_game = self.*;
                new_game.playRecursive(stats, next_roll, depth + 1);
            }
        }
    };
}

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    const player1_line = problem.line().?;
    const player1_position = player1_line[player1_line.len - 1] - '0';
    const player2_line = problem.line().?;
    const player2_position = player2_line[player2_line.len - 1] - '0';

    const s1 = blk: {
        var game = Game(DeterministicDie, 1000).init(player1_position, player2_position);
        const stats = game.play();
        const loser: usize = if (stats.player1_wins == 0) stats.last_winning_game.player1.score else stats.last_winning_game.player2.score;
        break :blk loser * stats.last_winning_game.die.rolls;
    };

    // const s2 = blk: {
    //     var game = Game(DiracDie, 21).init(player1_position, player2_position);
    //     const stats = game.play();
    //     break :blk std.math.max(stats.player1_wins, stats.player2_wins);
    // };
    const s2: usize = 0;

    return problem.solution(s1, s2);
}
