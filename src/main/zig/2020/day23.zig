const aoc = @import("../aoc.zig");
const std = @import("std");

const CupCircle = struct {
    const SIZE = 9;

    cups: [SIZE]u8 = undefined,
    curr_idx: u8 = 0,

    fn init() CupCircle {
        return .{};
    }

    fn getCurrentCup(self: *const CupCircle) u8 {
        return self.cups[self.curr_idx];
    }

    fn removeNext3Cups(self: *CupCircle) [3]u8 {
        var res: [3]u8 = undefined;
        var idx = self.curr_idx;
        for (res) |*r| {
            idx = (idx + 1) % SIZE;
            r.* = self.cups[idx];
            self.cups[idx] = 0;
        }
        return res;
    }

    fn getDestinationCupIndex(self: *const CupCircle) u8 {
        var destination_cup = self.getCurrentCup();
        while (true) {
            destination_cup -= 1;
            if (destination_cup == 0) {
                destination_cup = SIZE;
            }

            const idx = std.mem.indexOfScalar(u8, &self.cups, destination_cup) orelse continue;
            return @intCast(u8, idx);
        }
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var cups = CupCircle.init();
    std.mem.copy(u8, &cups.cups, problem.input);
    var iter: u8 = 0;
    while (iter < 10) : (iter += 1) {
        const moves = cups.removeNext3Cups();
        const destCupIdx = cups.getDestinationCupIndex();
    }

    return aoc.Solution { .p1 = 0, .p2 = 0 };
}
