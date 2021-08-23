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
        var tile_map_iter = self.tile_map.valueIterator();
        while (tile_map_iter.next()) |tile| {
            tile.deinit();
        }
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
                var iterator = unprocessed_tile.transformation_iterator();
                while (iterator.next()) {
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

    fn deinit(self: *Tile) void {
        self.matrix.deinit();
    }

    fn transformation_iterator(self: *Tile) MatrixTransformationIterator {
        return .{ .matrix = &self.matrix };
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

const Image = struct {
    const TILES_SIZE: usize = 12;
    const BORDERLESS_TILE_SIZE: usize = Tile.VECTOR_SIZE - 2;
    const ROW_BITS = TILES_SIZE * BORDERLESS_TILE_SIZE;
    const MONSTER_COORDS = [_]Coord {
        .{ .x = 18, .y = 0 },
        .{ .x = 0, .y = 1 },
        .{ .x = 5, .y = 1 },
        .{ .x = 6, .y = 1 },
        .{ .x = 11, .y = 1 },
        .{ .x = 12, .y = 1 },
        .{ .x = 17, .y = 1 },
        .{ .x = 18, .y = 1 },
        .{ .x = 19, .y = 1 },
        .{ .x = 1, .y = 2 },
        .{ .x = 4, .y = 2 },
        .{ .x = 7, .y = 2 },
        .{ .x = 10, .y = 2 },
        .{ .x = 13, .y = 2 },
        .{ .x = 16, .y = 2 },
    };
    const MONSTER_LIMIT = Coord { .x = 20, .y = 3 };

    matrix: aoc.SquareMatrix,
    roughness: usize,

    fn fromTileMap(tilemap: *const TileMap) Image {
        var matrix = aoc.SquareMatrix.init(ROW_BITS);
        var roughness: usize = 0;
        var tilemap_row: usize = 0;
        while (tilemap_row < TILES_SIZE) : (tilemap_row += 1) {
            var tilemap_col: usize = 0;
            while (tilemap_col < TILES_SIZE) : (tilemap_col += 1) {
                var tile = tilemap.tile_map.get(.{ .x = tilemap.top_left.x + @intCast(i8, tilemap_col), .y = tilemap.top_left.y + @intCast(i8, tilemap_row) }).?;
                var row: usize = 0;
                while (row < BORDERLESS_TILE_SIZE) : (row += 1) {
                    var col: usize = 0;
                    while (col < BORDERLESS_TILE_SIZE) : (col += 1) {
                        if (tile.matrix.get(row + 1, col + 1) == 1) {
                            roughness += 1;
                            matrix.set(tilemap_row * BORDERLESS_TILE_SIZE + row, tilemap_col * BORDERLESS_TILE_SIZE + col, 1);
                        }
                    }
                }
            }
        }
        return .{ .matrix = matrix, .roughness = roughness };
    }

    fn deinit(self: *Image) void {
        self.matrix.deinit();
    }

    fn transformation_iterator(self: *Image) MatrixTransformationIterator {
        return .{ .matrix = &self.matrix };
    }

    fn findMonsters(self: *Image) usize {
        var iter = self.transformation_iterator();
        while (iter.next()) {
            var monsters: usize = 0;
            var row: usize = 0;
            while (row < ROW_BITS - MONSTER_LIMIT.y) : (row += 1) {
                var col: usize = 0;
                outer: while (col < ROW_BITS - MONSTER_LIMIT.x) : (col += 1) {
                    for (MONSTER_COORDS) |monster_coord| {
                        if (self.matrix.get(row + @intCast(usize, monster_coord.y), col + @intCast(usize, monster_coord.x)) != 1) {
                            continue :outer;
                        }
                    }
                    monsters += 1;
                }
            }
            if (monsters > 0) {
                return self.roughness - (MONSTER_COORDS.len * monsters);
            }
        }
        unreachable;
    }
};

const MatrixTransformationIterator = struct {
    matrix: *aoc.SquareMatrix,
    counter: u4 = 0,

    fn next(self: *MatrixTransformationIterator) bool {
        if (self.counter == 8) {
            return false;
        }
        if (self.counter % 4 == 0) {
            self.matrix.flipHorizontally();
        }
        self.matrix.rotate90DegreesClockwise();
        self.counter += 1;
        return true;
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

    var image = Image.fromTileMap(&tile_map);
    defer image.deinit();
    const res2 = image.findMonsters();

    return aoc.Solution { .p1 = res1, .p2 = res2 };
}
