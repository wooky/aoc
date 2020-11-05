const aoc = @import("../aoc.zig");
const std = @import("std");

const LightMap = struct {
    buf1: [10000]bool = undefined, buf2: [10000]bool = undefined,
    curr: []bool = undefined, other: []bool = undefined,

    fn init(self: *LightMap) void {
        self.curr = &self.buf1;
        self.other = &self.buf2;
    }

    fn animate(self: *LightMap) void {
        for (self.curr) |currState, idx| {
            const not_top = idx >= 100;
            const not_bottom = idx < 9900;
            const not_left = idx % 100 != 0;
            const not_right = idx % 100 != 99;

            var neighbors_on: u8 = 0;
            if (not_top    and not_left  and self.curr[idx - 101]) neighbors_on += 1;
            if (not_top                  and self.curr[idx - 100]) neighbors_on += 1;
            if (not_top    and not_right and self.curr[idx -  99]) neighbors_on += 1;
            if (               not_left  and self.curr[idx -   1]) neighbors_on += 1;
            if (               not_right and self.curr[idx +   1]) neighbors_on += 1;
            if (not_bottom and not_left  and self.curr[idx +  99]) neighbors_on += 1;
            if (not_bottom               and self.curr[idx + 100]) neighbors_on += 1;
            if (not_bottom and not_right and self.curr[idx + 101]) neighbors_on += 1;

            self.other[idx] = neighbors_on == 3 or (neighbors_on == 2 and currState);
        }

        var tmp = self.curr;
        self.curr = self.other;
        self.other = tmp;
    }

    fn makeDefective(self: *LightMap) void {
        self.curr[0000] = true;
        self.curr[0099] = true;
        self.curr[9900] = true;
        self.curr[9999] = true;
    }

    fn getLightCount(self: *LightMap) u16 {
        var lights_on: u16 = 0;
        for (self.curr) |on| {
            if (on) {
                lights_on += 1;
            }
        }
        return lights_on;
    }
};

pub fn run(problem: *aoc.Problem) aoc.Solution {
    var map_good = LightMap {}; map_good.init();
    var map_bad = LightMap {}; map_bad.init();
    var map_idx: usize = 0;
    for (problem.input) |c| {
        switch (c) {
            '#' => { map_good.curr[map_idx] = true;  map_bad.curr[map_idx] = true;  map_idx += 1; },
            '.' => { map_good.curr[map_idx] = false; map_bad.curr[map_idx] = false; map_idx += 1; },
            '\n' => {},
            else => unreachable,
        }
    }
    map_bad.makeDefective();

    var loop: u8 = 0;
    while (loop < 100) : (loop += 1) {
        map_good.animate();
        map_bad.animate();
        map_bad.makeDefective();
    }

    return .{ .p1 = map_good.getLightCount(), .p2 = map_bad.getLightCount() };
}
