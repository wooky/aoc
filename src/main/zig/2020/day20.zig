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
    const VECTOR_SIZE: usize = 10;

    id: u16,
    matrix: aoc.SquareMatrix,

    fn fromGroup(group: []const u8) !Tile {
        var tile = Tile {
            .id = try std.fmt.parseInt(u16, group[5..9], 10),
            .matrix = aoc.SquareMatrix.init(VECTOR_SIZE),
        };
        var row: usize = 0;
        var col: usize = 0;
        for (group[11..]) |c| {
            switch (c) {
                '#' => {
                    tile.matrix.set(row, col, 1);
                    col += 1;
                },
                '.' => {
                    tile.matrix.set(row, col, 0);
                    col += 1;
                },
                '\n' => {
                    row += 1;
                    col = 0;
                },
                else => unreachable
            }
        }
        return tile;
    }

    fn iterator(self: *Tile) TileIterator {
        return TileIterator { .matrix = &self.matrix };
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
        const other_bottom = other.matrix.submatrix(VECTOR_SIZE - 1, 0, 1, VECTOR_SIZE);
        return self.matrix.submatrix(0, 0, 1, VECTOR_SIZE).equals(&other_bottom);
    }

    fn leftMatches(self: *const Tile, other: *const Tile) bool {
        const other_right = other.matrix.submatrix(0, VECTOR_SIZE - 1, VECTOR_SIZE, 1);
        return self.matrix.submatrix(0, 0, VECTOR_SIZE, 1).equals(&other_right);
    }
};

const TileIterator = struct {
    matrix: *aoc.SquareMatrix,
    counter: u8 = 0,

    fn next(self: *TileIterator) ?void {
        if (self.counter == 8) {
            return null;
        }
        if (self.counter % 4 == 0) {
            self.matrix.flipHorizontally();
        }
        self.matrix.rotate90DegreesClockwise();
        self.counter += 1;
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
