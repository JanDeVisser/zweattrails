const std = @import("std");

pub const Coordinates = struct {
    lat: f32,
    lon: f32,

    pub fn for_tile(tile: Tile) Coordinates {
        const n: f32 = @floatFromInt(@shlExact(@as(u32, 1), tile.zoom));
        const lon_deg: f32 = @as(f32, @floatFromInt(tile.x)) / n * 360.0 - 180.0;
        const lat_rad = std.math.atan(std.math.sinh(std.math.pi * (1.0 - 2.0 * @as(f32, @floatFromInt(tile.y)) / n)));
        const lat_deg = std.math.radiansToDegrees(lat_rad);
        return Coordinates{ .lat = lat_deg, .lon = lon_deg };
    }

    pub fn on_tile(this: Coordinates, tile: Tile) bool {
        const sw = Coordinates.for_tile(Tile{ .x = tile.x, .y = tile.y + 1, .zoom = tile.zoom });
        const ne = Coordinates.for_tile(Tile{ .x = tile.x + 1, .y = tile.y, .zoom = tile.zoom });
        return this.in_box(sw, ne);
    }

    pub fn in_box(this: Coordinates, box: Box) bool {
        return box.has(this);
    }

    pub const _transient = false;
};

pub const Box = struct {
    sw: Coordinates,
    ne: Coordinates,

    pub fn for_tile(tile: Tile) Box {
        const sw = Tile{ .zoom = tile.zoom, .x = tile.x, .y = tile.y + 1 };
        const ne = Tile{ .zoom = tile.zoom, .x = tile.x + 1, .y = tile.y };
        return Box{ .sw = sw, .ne = ne };
    }

    pub fn with_margins(this: Box, margin: f32) Box {
        const mid = this.center();
        const f = 1.0 + margin;
        return Box{
            .sw = Coordinates{ .lon = mid.lon - (this.width() * f) / 2.0, .lat = mid.lat - (this.height() * f) / 2.0 },
            .ne = Coordinates{ .lon = mid.lon + (this.width() * f) / 2.0, .lat = mid.lat + (this.height() * f) / 2.0 },
        };
    }

    pub fn center(this: Box) Coordinates {
        return Coordinates{ .lat = (this.sw.lat + this.ne.lat) / 2.0, .lon = (this.sw.lon + this.ne.lon) / 2.0 };
    }

    pub fn width(this: Box) f32 {
        return this.ne.lon - this.sw.lon;
    }

    pub fn height(this: Box) f32 {
        return this.ne.lat - this.sw.lat;
    }

    pub fn contains(this: Box, other: Box) bool {
        return this.has(other.sw) and this.has(other.ne);
    }

    pub fn has(this: Box, point: Coordinates) bool {
        return point.lat >= this.sw.lat and point.lon >= this.sw.lon and point.lat <= this.ne.lat and point.lon <= this.ne.lon;
    }

    pub const _transient = false;
};

pub const Tile = struct {
    x: u32,
    y: u32,
    zoom: u5,

    pub fn for_coordinates(pos: Coordinates, zoom: u5) Tile {
        const lat_rad = std.math.degreesToRadians(pos.lat);
        const n = @as(f32, @floatFromInt(@as(u32, @shlExact(@as(u32, 1), zoom))));
        const xtile: u32 = @intFromFloat((pos.lon + 180.0) / 360.0 * n);
        const ytile: u32 = @intFromFloat((1.0 - std.math.asinh(@tan(lat_rad)) / std.math.pi) / 2.0 * n);
        return Tile{ .zoom = zoom, .x = xtile, .y = ytile };
    }

    pub fn box(this: Tile) Box {
        const sw = Coordinates.for_tile(Tile{ .x = this.x, .y = this.y + 1, .zoom = this.zoom });
        const ne = Coordinates.for_tile(Tile{ .x = this.x + 1, .y = this.y, .zoom = this.zoom });
        return Box{ .sw = sw, .ne = ne };
    }

    pub fn get_map(this: Tile, allocator: std.mem.Allocator) ![]const u8 {
        const cached_map = try this.get_cached_map(allocator);
        if (cached_map) |map| {
            return map;
        }
        var buf: [64]u8 = undefined;
        var headers: [4096]u8 = undefined;
        const url = std.fmt.bufPrint(&buf, "https://tile.openstreetmap.org/{}/{}/{}.png", .{ this.zoom, this.x, this.y }) catch std.debug.panic("Out of Memory", .{});
        var client = std.http.Client{ .allocator = allocator };
        defer client.deinit();
        const uri = std.Uri.parse(url) catch unreachable;
        var request = try client.open(.GET, uri, .{ .server_header_buffer = &headers });
        defer request.deinit();
        try request.send();
        try request.wait();
        std.debug.print("downloading tile map {s}\n", .{url});
        const map = try request.reader().readAllAlloc(allocator, 1024 * 1024);
        try this.cache_map(map, allocator);
        return map;
    }

    fn get_cache_dir(this: Tile, allocator: std.mem.Allocator) !std.fs.Dir {
        const app_dir = try std.fs.getAppDataDir(allocator, "zweattrails");
        std.fs.makeDirAbsolute(app_dir) catch |err| {
            if (err != std.posix.MakeDirError.PathAlreadyExists) {
                return err;
            }
        };
        var d = try std.fs.openDirAbsolute(app_dir, .{});
        defer d.close();
        var tilecache = try d.makeOpenPath("tilecache", .{});
        defer tilecache.close();
        const z: []const u8 = try std.fmt.allocPrint(allocator, "{}", .{this.zoom});
        defer allocator.free(z);
        return tilecache.makeOpenPath(z, .{});
    }

    fn get_cached_map(this: Tile, allocator: std.mem.Allocator) !?[]const u8 {
        var cache_dir = try this.get_cache_dir(allocator);
        defer cache_dir.close();
        const fname = try std.fmt.allocPrint(allocator, "{}-{}.png", .{ this.x, this.y });
        defer allocator.free(fname);
        var cache_file = cache_dir.openFile(fname, .{}) catch |err| {
            if (err == std.fs.File.OpenError.FileNotFound) {
                return null;
            }
            return err;
        };
        defer cache_file.close();
        std.debug.print("loading cached tile map {s}\n", .{fname});
        return try cache_file.readToEndAlloc(allocator, 1024 * 1024);
    }

    fn cache_map(this: Tile, map: []const u8, allocator: std.mem.Allocator) !void {
        var cache_dir = try this.get_cache_dir(allocator);
        defer cache_dir.close();
        const fname = try std.fmt.allocPrint(allocator, "{}-{}.png", .{ this.x, this.y });
        defer allocator.free(fname);
        const cache_file = try cache_dir.createFile(fname, .{});
        defer cache_file.close();
        return cache_file.writeAll(map);
    }
};

pub const Atlas = struct {
    zoom: u5,
    x: u32,
    y: u32,
    width: u16,
    height: u16,
    columns: u16,
    rows: u16,
    num_tiles: u16,
    maps: ?[][]const u8 = null,
    allocator: ?std.mem.Allocator = null,

    pub const _transient = true;

    pub fn for_box(b: Box, width: u8, height: u8) Atlas {
        std.debug.assert(width > 0 and width <= 8);
        std.debug.assert(height > 0 and height <= 4);
        const min_dim = @min(@as(u5, @truncate(width)), @as(u5, @truncate(height)));
        var zoom: u5 = 16 - min_dim - 1;
        const mid = b.center();
        while (zoom > 0) {
            const mid_tile = Tile.for_coordinates(mid, zoom);
            const tile_box = mid_tile.box();
            if (tile_box.width() > b.width() * 1.1 and tile_box.height() > b.height() * 1.1) {
                zoom += min_dim - 1;
                const t = Tile.for_coordinates(mid, zoom);
                return Atlas{
                    .zoom = zoom,
                    .x = t.x - width,
                    .y = t.y - height,
                    .width = width,
                    .height = height,
                    .rows = 2 * height + 1,
                    .columns = 2 * width + 1,
                    .num_tiles = (2 * width + 1) * (2 * height + 1),
                };
            }
            zoom -= 1;
        }
        unreachable;
    }

    pub fn deinit(this: *Atlas) void {
        if (this.maps) |maps| {
            std.debug.assert(this.allocator != null);
            for (maps) |m| {
                this.allocator.?.free(m);
            }
            this.allocator.?.free(maps);
        }
    }

    fn tile(this: Atlas, ix: usize) Tile {
        std.debug.assert(ix < this.num_tiles);
        const i: u32 = @intCast(ix);
        return Tile{
            .zoom = this.zoom,
            .x = this.x + i % this.columns,
            .y = this.y + i / this.columns,
        };
    }

    fn tile_xy(this: Atlas, x: u32, y: u32) Tile {
        std.debug.assert(x < this.columns and y < this.rows);
        return Tile{
            .zoom = this.zoom,
            .x = this.x + x,
            .y = this.y + y,
        };
    }

    pub fn get_maps(this: *Atlas, allocator: std.mem.Allocator) ![][]const u8 {
        if (this.maps) |ret| {
            return ret;
        }
        this.maps = try allocator.alloc([]const u8, this.num_tiles);
        this.allocator = allocator;
        for (0..this.num_tiles) |ix| {
            this.maps.?[ix] = try this.tile(ix).get_map(allocator);
        }
        return this.maps.?;
    }

    pub fn box(this: Atlas) Box {
        const t_sw = this.tile_xy(0, this.rows - 1);
        const t_ne = this.tile_xy(this.columns - 1, 0);
        return Box{ .sw = t_sw.box().sw, .ne = t_ne.box().ne };
    }

    pub fn sub_box(this: Atlas, ix: usize) Box {
        return this.tile(ix).box();
    }
};
