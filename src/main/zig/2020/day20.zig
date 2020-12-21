const aoc = @import("../aoc.zig");
const std = @import("std");

const Coord = struct { x: i8 = 0, y: i8 = 0 };

const TileMap = struct {
    top_left: Coord = Coord {},
    bottom_right: Coord = Coord {},
    unprocessed_tiles: std.ArrayList(Tile),
    queue: std.ArrayList(Coord),
    tile_map: std.AutoHashMap(Coord, Tile),

    fn init(allocator: *std.mem.Allocator) TileMap {
        return TileMap {
            .unprocessed_tiles = std.ArrayList(Tile).init(allocator),
            .queue = std.ArrayList(Coord).init(allocator),
            .tile_map = std.AutoHashMap(Coord, Tile).init(allocator),
        };
    }

    fn deinit(self: *TileMap) void {
        self.unprocessed_tiles.deinit();
        self.queue.deinit();
        self.tile_map.deinit();
    }

    fn process(self: *TileMap) !void {
        try self.tile_map.put(Coord {}, self.unprocessed_tiles.pop());
        try self.queue.append(Coord {});
        while (self.queue.popOrNull()) |coord| {
            const tile = self.tile_map.get(coord).?;
            var idx: usize = 0;
            outer: while (idx < self.unprocessed_tiles.items.len) {
                var unprocessed_tile = self.unprocessed_tiles.items[idx];
                var transformations: u8 = 0;
                while (transformations < 8) : (transformations += 1) {
                    if (transformations % 4 == 0) {
                        unprocessed_tile.flip();
                    }
                    if (unprocessed_tile.offsetMatchingWithTile(&tile)) |offset| {
                        const next = Coord {
                            .x = coord.x + offset.x,
                            .y = coord.y + offset.y,
                        };
                        self.top_left.x = std.math.min(self.top_left.x, next.x);
                        self.top_left.y = std.math.min(self.top_left.y, next.y);
                        self.bottom_right.x = std.math.max(self.bottom_right.x, next.x);
                        self.bottom_right.y = std.math.max(self.bottom_right.y, next.y);

                        try self.tile_map.putNoClobber(next, unprocessed_tile);
                        try self.queue.append(next);
                        _ = self.unprocessed_tiles.swapRemove(idx);
                        continue :outer;
                    }
                    unprocessed_tile.rotate();
                }
                idx += 1;
            }
        }
    }
};

const Tile = struct {
    id: u16,
    tile: u100 = 0,

    fn fromGroup(group: []const u8) !Tile {
        var tile = Tile { .id = try std.fmt.parseInt(u16, group[5..9], 10) };
        for (group) |c| {
            const bit: u1 = switch (c) {
                '#' => 1,
                '.' => 0,
                else => continue
            };
            tile.tile <<= 1;
            tile.tile |= bit;
        }
        return tile;
    }

    fn rotate(self: *Tile) void {
        var new_tile: u100 = 0;
        var src_row: u7 = 0;
        while (src_row < 10) : (src_row += 1) {
            const dst_col = std.math.absCast(@intCast(i7, src_row) - 9);
            var src_col: u7 = 0;
            while (src_col < 10) : (src_col += 1) {
                const dst_row = src_col;
                new_tile |= ((self.tile >> (src_row*10 + src_col)) & 1) << (dst_row*10 + dst_col);
            }
        }
        self.tile = new_tile;
    }

    fn flip(self: *Tile) void {
        var new_tile: u100 = 0;
        var row: u7 = 0;
        while (row < 10) : (row += 1) {
            const src = (self.tile >> (row * 10)) & 0x3FF;
            const dest_shift = std.math.absCast((@intCast(i7, row) - 9)) * 10;
            new_tile |= src << dest_shift;
        }
        self.tile = new_tile;
    }

    fn offsetMatchingWithTile(self: *const Tile, other: *const Tile) ?Coord {
        if (self.topMatches(other)) {
            return Coord{ .y = 1 };
        }
        if (other.topMatches(self)) {
            return Coord{ .y = -1 };
        }
        if (self.leftMatches(other)) {
            return Coord{ .x = 1 };
        }
        if (other.leftMatches(self)) {
            return Coord{ .x = -1 };
        }
        return null;
    }

    fn topMatches(self: *const Tile, other: *const Tile) bool {
        return (self.tile >> 90) == (other.tile & 0x3FF);
    }

    fn leftMatches(self: *const Tile, other: *const Tile) bool {
        const mask: u100 = 0b1000000000100000000010000000001000000000100000000010000000001000000000100000000010000000001000000000;
        return (self.tile & mask) == ((other.tile << 9) & mask);
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var tile_map = TileMap.init(problem.allocator);
    defer tile_map.deinit();
    while (problem.group()) |group| {
        if (group.len != 0) {
            try tile_map.unprocessed_tiles.append(try Tile.fromGroup(group));
        }
    }

    try tile_map.process();
    const res1 =
        @intCast(usize, tile_map.tile_map.get(tile_map.top_left).?.id) * 
        tile_map.tile_map.get(.{ .x = tile_map.bottom_right.x, .y = tile_map.top_left.y }).?.id *
        tile_map.tile_map.get(.{ .x = tile_map.top_left.x, .y = tile_map.bottom_right.y }).?.id *
        tile_map.tile_map.get(tile_map.bottom_right).?.id;

    return aoc.Solution { .p1 = res1, .p2 = 0 };
}
