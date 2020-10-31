const std = @import("std");

const PathMap = struct {
    backing: PathBackingMap = PathBackingMap.init(std.heap.page_allocator),

    const PathBackingSubmap = std.StringHashMap(u8);
    const PathBackingMap = std.StringHashMap(PathBackingSubmap);

    fn get(self: *PathMap, k1: []const u8, k2: []const u8) u8 {
        return self.backing.getValue(k1).?.getValue(k2).?;
    }

    fn put(self: *PathMap, k1: []const u8, k2: [] const u8, v: u8) !void {
        var opt_submap = self.backing.get(k1);
        if (opt_submap) |kv| {
            _ = try kv.value.put(k2, v);
        }
        else {
            var submap = PathBackingSubmap.init(std.heap.page_allocator);
            _ = try submap.put(k2, v);
            _ = try self.backing.put(k1, submap);
        }
    }

    fn deinit(self: *PathMap) void {
        var iter = self.backing.iterator();
        while (iter.next()) |kv| {
            kv.value.deinit();
        }
        self.backing.deinit();
    }
};

const Datum = struct { locations: [][]const u8, paths: *PathMap, min_dist: u16, max_dist: u16 };

pub fn main() !void {
    var paths = PathMap {};
    defer paths.deinit();
    var locations = std.StringHashMap(void).init(std.heap.page_allocator);
    defer locations.deinit();

    var buf: [1024]u8 = undefined;
    const file = try std.fs.cwd().openFile("input/2015/day09.txt", .{});
    const size = try file.read(&buf);
    var lines = std.mem.tokenize(buf[0..size], "\n");
    while (lines.next()) |line| {
        var tokens = std.mem.tokenize(line, " ");
        const loc1 = tokens.next().?;
        _ = tokens.next().?;
        const loc2 = tokens.next().?;
        _ = tokens.next().?;
        const distance = try std.fmt.parseInt(u8, tokens.next().?, 10);

        _ = try paths.put(loc1, loc2, distance);
        _ = try paths.put(loc2, loc1, distance);
        _ = try locations.put(loc1, {});
        _ = try locations.put(loc2, {});
    }

    var location_permutation = std.ArrayList([] const u8).init(std.heap.page_allocator);
    defer location_permutation.deinit();
    var location_iter = locations.iterator();
    while (location_iter.next()) |kv| {
        try location_permutation.append(kv.key);
    }

    var datum = Datum { .locations = location_permutation.items, .paths = &paths, .min_dist = std.math.maxInt(u16), .max_dist = 0 };
    compute_dists(location_permutation.items.len, &datum);
    std.debug.warn("{}\n{}\n", .{datum.min_dist, datum.max_dist});
}

// Heap's Algorithm
fn compute_dists(k: usize, datum: *Datum) void {
    var i: usize = 0;
    if (k == 1) {
        var dist: u16 = 0;
        while (i < datum.locations.len - 1) : (i += 1) {
            dist += datum.paths.get(datum.locations[i], datum.locations[i + 1]);
        }
        datum.min_dist = std.math.min(datum.min_dist, dist);
        datum.max_dist = std.math.max(datum.max_dist, dist);
    }
    else {
        compute_dists(k - 1, datum);
        while (i < k-1) : (i += 1) {
            if (k % 2 == 0) {
                swap(datum.locations, i, k-1);
            }
            else {
                swap(datum.locations, 0, k-1);
            }
            compute_dists(k - 1, datum);
        }
    }
}

fn swap(arr: [][]const u8, a: usize, b: usize) void {
    var tmp = arr[a];
    arr[a] = arr[b];
    arr[b] = tmp;
}
