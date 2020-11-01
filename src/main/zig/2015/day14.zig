const aoc = @import("../aoc.zig");
const std = @import("std");

const Reindeer = struct {
    speed: u16, flying_duration: u16, resting_duration: u16,
    time_just_flew: u16 = 0, time_just_rested: u16 = 0,
    distance_flown: u16 = 0, points: u16 = 0,

    fn tick(self: *Reindeer) void {
        if (self.time_just_flew == self.flying_duration) {
            self.time_just_rested += 1;
            if (self.time_just_rested == self.resting_duration) {
                self.time_just_flew = 0;
                self.time_just_rested = 0;
            }
        }
        else {
            self.distance_flown += self.speed;
            self.time_just_flew += 1;
        }
    }
};

pub fn run(problem: *aoc.Problem) !void {
    var reindeer_buf: [16]Reindeer = undefined;
    var reindeer_size: u8 = 0;
    while (problem.line()) |line| {
        var tokens = std.mem.tokenize(line, " ");
        _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?;
        const speed = try std.fmt.parseInt(u16, tokens.next().?, 10);
        _ = tokens.next().?; _ = tokens.next().?;
        const duration = try std.fmt.parseInt(u16, tokens.next().?, 10);
        _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?; _ = tokens.next().?;
        const rest = try std.fmt.parseInt(u16, tokens.next().?, 10);

        reindeer_buf[reindeer_size] = Reindeer { .speed = speed, .flying_duration = duration, .resting_duration = rest };
        reindeer_size += 1;
    }

    var time: u16 = 0;
    var furthest: u16 = 0;
    var most_points: u16 = 0;
    while (time < 2503) : (time += 1) {
        for (reindeer_buf[0..reindeer_size]) |*reindeer| {
            reindeer.tick();
            furthest = std.math.max(furthest, reindeer.distance_flown);
        }
        for (reindeer_buf[0..reindeer_size]) |*reindeer| {
            if (reindeer.distance_flown == furthest) {
                reindeer.points += 1;
                most_points = std.math.max(most_points, reindeer.points);
            }
        }
    }

    std.debug.warn("{}\n{}\n", .{furthest, most_points});
}
