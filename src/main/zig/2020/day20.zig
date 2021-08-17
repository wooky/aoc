const aoc = @import("../aoc.zig");
const std = @import("std");
const LargeImage = u14400;

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
                var iter = unprocessed_tile.iterator();
                while (iter.next()) |_| {
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
                }
                idx += 1;
            }
        }
    }

    fn toLargeImage(self: *const TileMap) LargeImage {
        var image: LargeImage = 0;
        var row: i8 = 0;
        while (row < 12) : (row += 1) {
            var col: i8 = 0;
            while (col < 12) : (col += 1) {
                const tile = self.tile_map.get(.{ .x = self.bottom_right.x - col, .y = self.bottom_right.y - row }).?;
                
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

    fn iterator(self: *Tile) ImageTransformationIterator(u100) {
        return ImageTransformationIterator(u100).init(&self.tile);
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

fn ImageTransformationIterator(comptime T: type) type {
    comptime const dim = std.math.sqrt(@bitSizeOf(T));
    comptime const Shift = std.math.Log2Int(T);

    return struct {
        const Self = @This();

        image: *T,
        counter: u8 = 0,

        fn init(image: *T) Self {
            return Self { .image = image };
        }

        fn next(self: *Self) ?void {
            if (self.counter == 8) {
                return null;
            }
            if (self.counter % 4 == 0) {
                self.flip();
            }
            self.rotate();
            self.counter += 1;
        }

        fn rotate(self: *Self) void {
            var new_image: T = 0;
            var src_row: Shift = 0;
            while (src_row < dim) : (src_row += 1) {
                const dst_col = std.math.absCast((std.math.negateCast(src_row) catch unreachable) + dim - 1);
                var src_col: Shift = 0;
                while (src_col < dim) : (src_col += 1) {
                    const dst_row = src_col;
                    new_image |= ((self.image.* >> (src_row*dim + src_col)) & 1) << (dst_row*dim + dst_col);
                }
            }
            self.image.* = new_image;
        }

        fn flip(self: *Self) void {
            comptime const row_mask = (1 << dim) - 1;
            var new_image: T = 0;
            var row: Shift = 0;
            while (row < dim) : (row += 1) {
                const src = (self.image.* >> (row * dim)) & row_mask;
                const dest_shift = std.math.absCast((std.math.negateCast(row) catch unreachable) + dim - 1) * dim;
                new_image |= src << dest_shift;
            }
            self.image.* = new_image;
        }
    };
}

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
