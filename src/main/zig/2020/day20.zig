const aoc = @import("../aoc.zig");
const std = @import("std");

const TileMap = struct {
    range: aoc.CoordRange = aoc.CoordRange.init(),
    unprocessed_tiles: std.ArrayList(Tile),
    queue: std.ArrayList(aoc.Coord),
    tile_map: std.AutoHashMap(aoc.Coord, Tile),

    fn init(allocator: std.mem.Allocator) TileMap {
        return TileMap {
            .unprocessed_tiles = std.ArrayList(Tile).init(allocator),
            .queue = std.ArrayList(aoc.Coord).init(allocator),
            .tile_map = std.AutoHashMap(aoc.Coord, Tile).init(allocator),
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
        try self.tile_map.put(aoc.PredefinedCoord.ORIGIN, self.unprocessed_tiles.pop());
        try self.queue.append(aoc.PredefinedCoord.ORIGIN);
        while (self.queue.popOrNull()) |coord| {
            const tile = self.tile_map.get(coord).?;
            var idx: usize = 0;
            outer: while (idx < self.unprocessed_tiles.items.len) {
                var unprocessed_tile = self.unprocessed_tiles.items[idx];
                var iterator = unprocessed_tile.transformation_iterator();
                while (iterator.next()) {
                    if (unprocessed_tile.offsetMatchingWithTile(&tile)) |offset| {
                        const next = coord.add(offset);
                        self.range.amend(next);
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
        var coord = aoc.PredefinedCoord.ORIGIN;
        for (group[11..]) |c| {
            switch (c) {
                '#' => {
                    tile.matrix.set(coord, 1);
                    coord.col += 1;
                },
                '.' => {
                    tile.matrix.set(coord, 0);
                    coord.col += 1;
                },
                '\n' => {
                    coord.row += 1;
                    coord.col = 0;
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

    fn offsetMatchingWithTile(self: *const Tile, other: *const Tile) ?aoc.Coord {
        if (self.topMatches(other)) {
            return aoc.PredefinedCoord.DOWN;
        }
        if (other.topMatches(self)) {
            return aoc.PredefinedCoord.UP;
        }
        if (self.leftMatches(other)) {
            return aoc.PredefinedCoord.RIGHT;
        }
        if (other.leftMatches(self)) {
            return aoc.PredefinedCoord.LEFT;
        }
        return null;
    }

    fn topMatches(self: *const Tile, other: *const Tile) bool {
        const other_bottom = other.matrix.submatrix(aoc.Coord.init(.{VECTOR_SIZE - 1, 0}), aoc.Coord.init(.{1, VECTOR_SIZE}));
        return self.matrix.submatrix(aoc.PredefinedCoord.ORIGIN, aoc.Coord.init(.{1, VECTOR_SIZE})).equals(&other_bottom);
    }

    fn leftMatches(self: *const Tile, other: *const Tile) bool {
        const other_right = other.matrix.submatrix(aoc.Coord.init(.{0, VECTOR_SIZE - 1}), aoc.Coord.init(.{VECTOR_SIZE, 1}));
        return self.matrix.submatrix(aoc.PredefinedCoord.ORIGIN, aoc.Coord.init(.{VECTOR_SIZE, 1})).equals(&other_right);
    }
};

const Image = struct {
    const TILES_SIZE: usize = 12;
    const BORDERLESS_TILE_SIZE: usize = Tile.VECTOR_SIZE - 2;
    const ROW_BITS = TILES_SIZE * BORDERLESS_TILE_SIZE;
    const MONSTER_COORDS = [_]aoc.Coord {
        aoc.Coord.init(.{0, 18}),
        aoc.Coord.init(.{1, 0}),
        aoc.Coord.init(.{1, 5}),
        aoc.Coord.init(.{1, 6}),
        aoc.Coord.init(.{1, 11}),
        aoc.Coord.init(.{1, 12}),
        aoc.Coord.init(.{1, 17}),
        aoc.Coord.init(.{1, 18}),
        aoc.Coord.init(.{1, 19}),
        aoc.Coord.init(.{2, 1}),
        aoc.Coord.init(.{2, 4}),
        aoc.Coord.init(.{2, 7}),
        aoc.Coord.init(.{2, 10}),
        aoc.Coord.init(.{2, 13}),
        aoc.Coord.init(.{2, 16}),
    };
    const MONSTER_LIMIT = aoc.Coord.init(.{3, 20});

    matrix: aoc.SquareMatrix,
    roughness: usize,

    fn fromTileMap(tilemap: *const TileMap) Image {
        var matrix = aoc.SquareMatrix.init(ROW_BITS);
        var roughness: usize = 0;

        var tilemap_iter = tilemap.range.iterator();
        while (tilemap_iter.next()) |tilemap_coord| {
            const image_superoffset = tilemap_coord.subtract(tilemap.range.first);
            const tile = tilemap.tile_map.get(tilemap_coord).?;
            var pixel_iter = aoc.CoordRangeIterator.init(
                aoc.PredefinedCoord.ORIGIN,
                aoc.Coord.init(.{BORDERLESS_TILE_SIZE - 1, BORDERLESS_TILE_SIZE - 1})
            );
            while (pixel_iter.next()) |pixel_coord| {
                if (tile.matrix.get(pixel_coord.add(aoc.Coord.init(.{1, 1}))) == 1) {
                    roughness += 1;
                    matrix.set(pixel_coord.add(image_superoffset.multiply(BORDERLESS_TILE_SIZE)), 1);
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
            var range_iter = aoc.CoordRangeIterator.init(
                aoc.PredefinedCoord.ORIGIN,
                aoc.Coord.init(.{ROW_BITS - MONSTER_LIMIT.row - 1, ROW_BITS - MONSTER_LIMIT.col - 1})
            );
            outer: while (range_iter.next()) |coord| {
                for (MONSTER_COORDS) |monster_coord| {
                    if (self.matrix.get(coord.add(monster_coord)) != 1) {
                        continue :outer;
                    }
                }
                monsters += 1;
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
        @intCast(usize, tile_map.tile_map.get(tile_map.range.first).?.id) * 
        tile_map.tile_map.get(aoc.Coord.init(.{tile_map.range.first.row, tile_map.range.last.col})).?.id *
        tile_map.tile_map.get(aoc.Coord.init(.{tile_map.range.last.row, tile_map.range.first.col})).?.id *
        tile_map.tile_map.get(tile_map.range.last).?.id;

    var image = Image.fromTileMap(&tile_map);
    defer image.deinit();
    const res2 = image.findMonsters();

    return problem.solution(res1, res2);
}
