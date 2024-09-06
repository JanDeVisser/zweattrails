const std = @import("std");
const fit = @import("fit.zig");
const fittypes = @import("fittypes.zig");
const map = @import("map.zig");
const zorro = @import("zorro.zig");

const ray = @cImport(@cInclude("raylib.h"));
const libC = @cImport(@cInclude("time.h"));
const pg = @import("pg");

pub const log = std.log.scoped(.zweattrails);

pub const Timestamp = struct {
    value: i64,
};

pub const TimeOfDay = struct {
    hour: u8,
    minute: u8,
    second: u8,
    millis: u16,

    pub fn from_float(t: f32) TimeOfDay {
        const time_in_seconds: u32 = @intFromFloat(@trunc(t));
        const hour = @as(u8, @truncate(time_in_seconds / 3600));
        const minute = @as(u8, @truncate((time_in_seconds % 3600) / 60));
        const second = @as(u8, @truncate(time_in_seconds % 60));
        return TimeOfDay{
            .hour = hour % 24,
            .minute = minute,
            .second = second,
            .millis = @intFromFloat((t - @trunc(t)) * 1000),
        };
    }

    pub fn format(this: TimeOfDay, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("{}h {:02}'{:02}.{}\"", .{ this.hour, this.minute, this.second, this.millis });
    }
};

pub const Record = struct {
    id: ?i32,
    timestamp: u32,
    position: map.Coordinates,
    elevation: f32,
    distance: f32,
    power: f32,
    speed: f32,
    hr: f32,

    pub fn init(record: fittypes.record) !Record {
        return Record{
            .id = null,
            .timestamp = record.timestamp,
            .position = map.Coordinates{
                .lat = record.position_lat,
                .lon = record.position_long,
            },
            .elevation = record.enhanced_altitude,
            .distance = record.distance,
            .power = @floatFromInt(record.power),
            .speed = record.enhanced_speed,
            .hr = @floatFromInt(record.heart_rate),
        };
    }

    pub fn deinit(record: *Record) void {
        _ = record;
    }

    pub fn load(row: pg.Row) Record {
        const ret = row.to(Record, .{ .map = .name });
        return ret;
    }

    pub fn store(this: *Record, db: zorro.Db) !void {
        var txt = zorro.Zorro.init(db);
        defer txt.deinit();
        txt.persist(Record, this);
    }
};

pub const Session = struct {
    allocator: std.mem.Allocator,
    id: ?i32,
    records: std.ArrayList(Record),
    description: []const u8,
    start_time: u32,
    end_time: u32,
    elapsed_time: f32,
    moving_time: f32,
    distance: f32,
    min_elevation: f32,
    max_elevation: f32,
    max_power: f32,
    max_speed: f32,
    min_hr: u32,
    max_hr: u32,
    time_range: ray.Vector2,
    route_area: map.Box,
    atlas: map.Atlas,

    pub fn init(session: fittypes.session, allocator: std.mem.Allocator) !Session {
        const route_area = map.Box{
            .sw = map.Coordinates{ .lon = 200, .lat = 100 },
            .ne = map.Coordinates{ .lon = -200, .lat = -100 },
        };
        return Session{
            .allocator = allocator,
            .id = null,
            .records = std.ArrayList(Record).init(allocator),
            .start_time = session.start_time,
            .end_time = session.start_time + @as(u32, @intFromFloat(session.total_elapsed_time)),
            .description = "",
            .elapsed_time = session.total_elapsed_time,
            .moving_time = session.total_moving_time,
            .distance = session.total_distance,
            .min_elevation = session.enhanced_min_altitude,
            .max_elevation = session.enhanced_max_altitude,
            .max_power = @as(f32, @floatFromInt(session.max_power)),
            .max_speed = session.enhanced_max_speed,
            .min_hr = session.min_heart_rate,
            .max_hr = session.max_heart_rate,
            .time_range = ray.Vector2{
                .x = @floatFromInt(session.start_time),
                .y = @as(f32, @floatFromInt(session.start_time)) + session.total_elapsed_time,
            },
            .route_area = route_area,
            .atlas = map.Atlas.for_box(route_area, 2, 2),
        };
    }

    pub fn deinit(this: *Session) void {
        this.atlas.deinit();
        for (this.records.items) |*r| {
            r.deinit();
        }
        this.records.deinit();
    }

    pub fn append(this: *Session, record: Record) !void {
        try this.records.append(record);
        if (@abs(record.position.lat) > 0.1 and @abs(record.position.lon) > 0.1) {
            this.route_area.sw.lat = @min(this.route_area.sw.lat, record.position.lat);
            this.route_area.sw.lon = @min(this.route_area.sw.lon, record.position.lon);
            this.route_area.ne.lat = @max(this.route_area.ne.lat, record.position.lat);
            this.route_area.ne.lon = @max(this.route_area.ne.lon, record.position.lon);
            this.atlas = map.Atlas.for_box(this.route_area, 2, 2);
        }
    }

    pub fn append_all(this: *Session, records: std.ArrayList(Record)) !void {
        for (records.items) |r| {
            try this.append(r);
        }
    }

    pub fn load(row: pg.Row, allocator: std.mem.Allocator) !Session {
        var ret = row.to(Session, .{ .map = .name });
        ret.allocator = allocator;
        ret.records = std.ArrayList(Record).init(allocator);
        return ret;
    }

    fn store(this: *Session, db: zorro.Db) !void {
        var txt = zorro.Zorro.init(db);
        defer txt.deinit();
        try txt.persist(Session, this);
        for (this.records.items) |*r| {
            try txt.persist(Record, r);
        }
    }

    pub fn map_image(this: *Session) !ray.Image {
        const mid = this.route_area.center();
        var images: []ray.Image = try this.allocator.alloc(ray.Image, this.atlas.num_tiles);
        defer this.allocator.free(images);
        for (try this.atlas.get_maps(this.allocator), 0..) |m, ix| {
            images[ix] = ray.LoadImageFromMemory(".png", m.ptr, @intCast(m.len));
        }

        var m = ray.GenImageColor(@as(u16, this.atlas.columns) * 256, @as(u16, this.atlas.rows) * 256, ray.BLANK);
        for (0..this.atlas.num_tiles) |ix| {
            ray.ImageDraw(&m, images[ix], ray.Rectangle{
                .x = 0,
                .y = 0,
                .width = 256,
                .height = 256,
            }, ray.Rectangle{
                .x = @floatFromInt((ix % this.atlas.columns) * 256),
                .y = @floatFromInt((ix / this.atlas.columns) * 256),
                .width = 256,
                .height = 256,
            }, ray.WHITE);
            ray.UnloadImage(images[ix]);
        }

        const box = this.atlas.box();
        ray.ImageDrawLine(&m, 0, this.atlas.height * 256, @as(u16, this.atlas.columns * 256), this.atlas.height * 256, ray.GREEN);
        ray.ImageDrawLine(&m, 0, (this.atlas.height + 1) * 256, this.atlas.columns * 256, (this.atlas.height + 1) * 256, ray.GREEN);
        ray.ImageDrawLine(&m, this.atlas.width * 256, 0, this.atlas.width * 256, this.atlas.rows * 256, ray.GREEN);
        ray.ImageDrawLine(&m, (this.atlas.width + 1) * 256, 0, (this.atlas.width + 1) * 256, this.atlas.rows * 256, ray.GREEN);

        std.debug.print("atlas: zoom: {} x: {} y: {}\n  width: {} height: {} columns: {} rows: {}\n", .{
            this.atlas.zoom,
            this.atlas.x,
            this.atlas.y,
            this.atlas.width,
            this.atlas.height,
            this.atlas.columns,
            this.atlas.rows,
        });
        std.debug.print("atlas.route_area: sw: {d},{d} ne: {d},{d} dim {d}x{d}\n", .{ this.route_area.sw.lon, this.route_area.sw.lat, this.route_area.ne.lon, this.route_area.ne.lat, this.route_area.width(), this.route_area.height() });
        std.debug.print("box: sw: {d},{d} ne: {d},{d} dim {d}x{d}\n", .{ box.sw.lon, box.sw.lat, box.ne.lon, box.ne.lat, box.width(), box.height() });
        const r = ray.Rectangle{
            .x = (this.route_area.sw.lon - box.sw.lon) / box.width() * @as(f32, @floatFromInt(this.atlas.columns * 256)),
            .y = (1.0 - (this.route_area.ne.lat - box.sw.lat) / box.height()) * @as(f32, @floatFromInt(this.atlas.rows * 256)),
            .width = this.route_area.width() / box.width() * @as(f32, @floatFromInt(this.atlas.columns * 256)),
            .height = this.route_area.height() / box.height() * @as(f32, @floatFromInt(this.atlas.rows * 256)),
        };
        std.debug.print("r: {d}x{d}@({d},{d})\n", .{ r.width, r.height, r.x, r.y });
        const fat = ray.Rectangle{
            .x = r.x - (r.width * 0.05),
            .y = r.y - (r.height * 0.05),
            .width = r.width * 1.1,
            .height = r.height * 1.1,
        };
        const mid_x = fat.x + (fat.width / 2);
        const mid_y = fat.y + (fat.height / 2);
        const w = @as(f32, @floatFromInt(this.atlas.width * 256));
        const h = @as(f32, @floatFromInt(this.atlas.height * 256));
        const img_w = @as(f32, @floatFromInt(m.width));
        const img_h = @as(f32, @floatFromInt(m.height));

        const square = ray.Rectangle{
            .x = if (mid_x < w / 2) 0.0 else if (mid_x + w / 2 > img_w) img_w - w else mid_x - w / 2,
            .y = if (mid_y < h / 2) 0.0 else if (mid_y + h / 2 > img_h) img_h - h else mid_y - h / 2,
            .width = w,
            .height = h,
        };
        std.debug.print("square: {d}x{d}@({d},{d})\n", .{ square.width, square.height, square.x, square.y });

        ray.ImageDrawRectangleLines(&m, r, 2, ray.SKYBLUE);
        ray.ImageDrawRectangleLines(&m, fat, 2, ray.PINK);
        ray.ImageDrawRectangleLines(&m, square, 2, ray.DARKBLUE);
        ray.ImageDrawCircleV(&m, .{
            .x = (mid.lon - box.sw.lon) / box.width() * img_w,
            .y = (1.0 - (mid.lat - box.sw.lat) / box.height()) * img_h,
        }, 3, ray.BLACK);

        var prev_x: ?f32 = null;
        var prev_y: ?f32 = null;
        for (this.records.items, 0..) |record, ix| {
            if (ix == 0) {
                continue;
            }
            if (@abs(record.position.lat) < 0.1 or @abs(record.position.lon) < 0.1) {
                continue;
            }
            const dlat = 1.0 - (record.position.lat - box.sw.lat) / box.height();
            const dlon = (record.position.lon - box.sw.lon) / box.width();
            const x: f32 = @as(f32, @floatFromInt(this.atlas.columns * 256)) * dlon;
            const y: f32 = @as(f32, @floatFromInt(this.atlas.rows * 256)) * dlat;
            ray.ImageDrawCircleV(&m, .{
                .x = x,
                .y = y,
            }, 2, ray.RED);
            if (prev_x != null and std.math.hypot(x - prev_x.?, y - prev_y.?) > 2) {
                ray.ImageDrawLineV(&m, ray.Vector2{ .x = prev_x.?, .y = prev_y.? }, ray.Vector2{ .x = x, .y = y }, ray.RED);
            }
            prev_x = x;
            prev_y = y;
        }
        ray.ImageCrop(&m, square);
        ray.ImageResize(&m, this.atlas.width * 256, this.atlas.height * 256);
        // ray.ImageResize(&m, 984, 984);
        return m;
    }

    pub fn graph_image(this: *Session, width: u32, height: u32) !ray.Image {
        var image = ray.GenImageColor(@intCast(width), @intCast(height), ray.BLANK);
        var prev_x: f32 = 0;
        var prev_speed: f32 = 0;
        var prev_power: f32 = 0;
        const width_f: f32 = @floatFromInt(width);
        const height_f: f32 = @floatFromInt(height);
        const dt: f32 = width_f / @as(f32, @floatFromInt(this.records.items.len));
        const dalt = height_f / (this.max_elevation - this.min_elevation);
        const dspeed = height_f / this.max_speed;
        const dpower = if (this.max_power > 0) height_f / this.max_power else 0.0;

        var window: usize = 0;
        var power_window: [64]f32 = undefined;
        for (this.records.items, 0..) |record, ix| {
            const x = @as(f32, @floatFromInt(ix)) * dt;
            const alt = record.elevation;
            const alt_y: f32 = height_f - (alt - this.min_elevation) * dalt;
            const speed_y: f32 = height_f - record.speed * dspeed;
            power_window[window] = record.power;
            window += 1;
            if (x - prev_x > 1.0) {
                ray.ImageDrawRectangleRec(&image, ray.Rectangle{
                    .x = prev_x,
                    .y = alt_y,
                    .width = @ceil(x - prev_x),
                    .height = height_f - alt_y,
                }, ray.LIGHTGRAY);
                ray.ImageDrawLineV(&image, ray.Vector2{
                    .x = prev_x,
                    .y = @ceil(prev_speed),
                }, ray.Vector2{
                    .x = x,
                    .y = @ceil(speed_y),
                }, ray.DARKGREEN);
                prev_speed = speed_y;
                if (dpower > 0) {
                    const avg_power: f32 = avg_power_blk: {
                        var sum: f32 = 0.0;
                        for (power_window[0..window]) |p| {
                            sum += p;
                        }
                        break :avg_power_blk sum / @as(f32, @floatFromInt(window));
                    };
                    const power_y: f32 = height_f - avg_power * dpower;
                    ray.ImageDrawLineV(&image, ray.Vector2{
                        .x = prev_x,
                        .y = prev_power,
                    }, ray.Vector2{
                        .x = x,
                        .y = power_y,
                    }, ray.DARKBLUE);
                    prev_power = avg_power;
                }
                prev_x = x;
                window = 0.0;
            }
        }
        return image;
    }
};

pub const Activity = struct {
    allocator: std.mem.Allocator,
    id: ?i32 = null,
    sessions: std.ArrayList(Session),
    start_time: u32,
    serial_number: u32,

    pub fn import(dir: std.fs.Dir, filename: []const u8, allocator: std.mem.Allocator) !Activity {
        const fitfile = try fit.FITFile.init(allocator, dir, filename);
        std.debug.print("#messages: {}\n", .{fitfile.messages.items.len});
        var sessions = std.ArrayList(Session).init(allocator);
        var serial_number: u32 = undefined;
        var records = std.ArrayList(Record).init(allocator);
        for (fitfile.messages.items) |message| {
            switch (message) {
                inline .file_id => |f| {
                    std.debug.print("File, #records: {}\n", .{sessions.items.len});
                    serial_number = f.serial_number;
                },
                inline .session => |s| {
                    std.debug.print("Session, #records: {}\n", .{records.items.len});
                    try sessions.append(try Session.init(s, allocator));
                    try sessions.items[sessions.items.len - 1].append_all(records);
                    records.clearAndFree();
                },
                inline .record => |record| {
                    try records.append(try Record.init(record));
                },
                inline else => {},
            }
        }
        if (sessions.items.len == 0) {
            return error.NoActivityInFile;
        }
        return Activity{
            .allocator = allocator,
            .sessions = sessions,
            .start_time = sessions.items[0].start_time,
            .serial_number = serial_number,
        };
    }

    pub fn deinit(this: *Activity) void {
        for (this.sessions.items) |*r| {
            r.deinit();
        }
        this.sessions.deinit();
    }

    pub fn query(allocator: std.mem.Allocator, db: zorro.Db) ![]Activity {
        var conn = try db.pool.acquire();
        defer conn.deinit();

        var result = try conn.queryOpts("select * from activity", .{}, .{ .column_names = true });
        defer result.deinit();
        while (result.next()) |row| {
            var ret: Activity = row.to(Activity, .{ .map = .name });
            ret.allocator = allocator;
            ret.sessions = std.ArrayList(Session).init(allocator);
            {
                const sessions = try conn.queryOpts("select * from session", .{}, .{ .column_names = true });
                defer sessions.deinit();
                while (try result.next()) |session_row| {
                    const session = try Session.load(ret, session_row);
                    ret.sessions.append(session);
                }
            }
        }
    }

    pub fn load(id: u32, allocator: std.mem.Allocator, db: zorro.Db) !Activity {
        var conn = try db.pool.acquire();
        defer conn.deinit();

        var z = zorro.Zorro(db);
        defer z.deinit();
        var ret = z.load(Activity, id);

        const row_maybe = conn.rowOpts("select * from activity where id = $1", .{id}, .{ .column_names = true });
        if (row_maybe) |row| {
            defer row.deinit();
            var ret: Activity =
                row.to(Activity, .{ .map = .name });
            ret.allocator = allocator;
            ret.sessions = std.ArrayList(Session).init(allocator);
            {
                const result = try conn.queryOpts("select * from session where activity_id = $1", .{id}, .{ .column_names = true });
                defer result.deinit();
                while (try result.next()) |session_row| {
                    const session = try Session.load(ret, session_row);
                    ret.sessions.append(session);
                }
            }
            {
                const result = try conn.queryOpts("select record.* from record, session where record.session_id = session.id and session.activity_id = $1", .{id}, .{ .column_names = true });
                defer result.deinit();
                while (try result.next()) |record_row| {
                    const session_id = record_row.getCol(u32, "session_id");
                    const session: *Session = blk: {
                        for (ret.sessions.items) |*s| {
                            if (s.id == session_id) {
                                break :blk s;
                            }
                        }
                        unreachable;
                    };
                    session.records.append(try Record.load(Record, record_row));
                }
            }
            return ret;
        }
        return error.NoActivityFound;
    }

    fn store(this: *Activity, db: zorro.Db) !void {
        var txt = zorro.Zorro.init(db);
        defer txt.deinit();
        try txt.persist(Activity, this);
        for (this.sessions.items) |*s| {
            try s.store(db);
        }
    }
};

pub const Import = struct {
    db: zorro.Db,
    allocator: std.mem.Allocator,
    done: std.ArrayList([]const u8),
    errors: std.StringArrayHashMap([]const u8),
    status: union(enum) { Start: void, Idle: void, Processing: void, Importing: []const u8, Crashed: struct {
        filename: []const u8,
        message: []const u8,
    } } = .Start,
    thread: std.Thread = undefined,
    inbox_d: std.fs.Dir,
    done_d: std.fs.Dir,
    errors_d: std.fs.Dir,

    pub fn init(db: zorro.Db, rebuild: bool) !Import {
        const allocator = std.heap.c_allocator;
        const app_dir = try std.fs.getAppDataDir(allocator, "zweattrails");
        std.fs.makeDirAbsolute(app_dir) catch |err| {
            if (err != std.posix.MakeDirError.PathAlreadyExists) {
                return err;
            }
        };
        var d = try std.fs.openDirAbsolute(app_dir, .{});
        defer d.close();

        if (rebuild) {
            try d.deleteTree("inbox");
            try d.rename("done", "inbox");
        }

        return .{
            .db = db,
            .allocator = allocator,
            .done = std.ArrayList([]const u8).init(allocator),
            .errors = std.StringArrayHashMap([]const u8).init(allocator),
            .inbox_d = try d.makeOpenPath("inbox", .{}),
            .done_d = try d.makeOpenPath("done", .{}),
            .errors_d = try d.makeOpenPath("errors", .{}),
        };
    }

    pub fn deinit(this: *Import) void {
        this.inbox_d.close();
        this.done_d.close();
        this.errors_d.close();
        for (this.done.items) |d| {
            this.allocator.free(d);
        }
        this.done.deinit();
        var it = this.errors.iterator();
        while (it.next()) |entry| {
            this.allocator.free(entry.key_ptr.*);
            this.allocator.free(entry.value_ptr.*);
        }
        this.errors.deinit();
    }

    fn import_file(this: *Import, filename: []const u8) !void {
        this.status = .{ .Importing = filename };
        defer this.status = .{ .Processing = void{} };
        var activity = try Activity.import(this.inbox_d, filename, this.allocator);
        defer activity.deinit();
        activity.store(this.db) catch |err| {
            try this.errors.put(try this.allocator.dupe(u8, filename), try std.fmt.allocPrint(this.allocator, "{any}", .{err}));
            try std.fs.rename(this.inbox_d, filename, this.errors_d, filename);
            return err;
        };
        try this.done.append(try this.allocator.dupe(u8, filename));
        try std.fs.rename(this.inbox_d, filename, this.done_d, filename);
    }

    pub fn run(this: *Import) void {
        while (true) {
            defer std.time.sleep(1e9);
            if (this.status != .Idle and this.status != .Start) {
                return;
            }
            var it = this.inbox_d.iterate();
            if (it.next() catch @panic("Wut")) |_| {
                this.status = .{ .Processing = void{} };
                defer this.status = .{ .Idle = void{} };
                it.reset();
                while (it.next() catch @panic("Wut?")) |f| {
                    if (std.mem.endsWith(u8, f.name, ".fit") or std.mem.endsWith(u8, f.name, ".FIT")) {
                        this.import_file(f.name) catch |err| {
                            this.status = .{ .Crashed = .{
                                .filename = this.allocator.dupe(u8, f.name) catch @panic("Out of memory"),
                                .message = std.fmt.allocPrint(this.allocator, "{any}", .{err}) catch @panic("Out of memory"),
                            } };
                            return;
                        };
                    }
                }
            }
        }
    }

    pub fn start(this: *Import) !void {
        this.thread = try std.Thread.spawn(.{}, Import.run, .{this});
        this.thread.detach();
    }

    pub fn restart(this: *Import) void {
        switch (this.status) {
            .Idle, .Running => {},
            .Crashed => |c| {
                this.allocator.free(c.filename);
                this.allocator.free(c.message);
                this.status = .{ .Idle = void{} };
            },
        }
    }
};

pub const GUI = struct {
    allocator: std.mem.Allocator,
    db: zorro.Db,
    font: ray.Font = undefined,
    state: union(enum) { Importing: void, List: void, Display: struct {
        activity: Activity,
        map_texture: ray.Texture2D,
        graph_texture: ray.Texture2D,
    } },
    importer: *Import,
    screenWidth: i32 = 1550,
    screenHeight: i32 = 1024,

    pub const State = enum {
        Importing,
        List,
        Display,
    };

    pub fn init(allocator: std.mem.Allocator, db: zorro.Db, importer: *Import) !GUI {
        return .{
            .allocator = allocator,
            .db = db,
            .state = .Importing,
            .importer = importer,
        };
    }

    pub fn deinit(this: *GUI) void {
        this.leave_state() catch |err| std.debug.print("Error deallocating GUI: {any}\n", .{err});
    }

    pub fn show(this: *GUI) !void {
        ray.InitWindow(this.screenWidth, this.screenHeight, "Zweattrails");
        defer ray.CloseWindow();

        this.font = ray.LoadFontEx("VictorMono-Medium.ttf", 20, null, 0);
        defer ray.UnloadFont(this.font);

        try this.importer.start();

        ray.SetTargetFPS(60);

        var frame: u32 = 0;
        while (!ray.WindowShouldClose()) {
            defer frame += 1;
            ray.BeginDrawing();
            defer ray.EndDrawing();

            ray.ClearBackground(ray.DARKGRAY);
            switch (this.state) {
                .Importing => try this.render_import_state(),
                .Display => try this.render_display_state(),
                else => {},
            }
        }
    }

    fn render_import_state(this: *GUI) !void {
        switch (this.state) {
            .Importing => {
                var buf: [128]u8 = undefined;
                const label = switch (this.importer.status) {
                    .Start => "Start",
                    .Importing => |filename| filename,
                    .Processing => "Processing . . .",
                    .Idle => "<Idle>",
                    .Crashed => |c| try std.fmt.bufPrintZ(&buf, "Crashed processing {s}: {s}", .{ c.filename, c.message }),
                };
                ray.DrawTextEx(this.font, try std.fmt.bufPrintZ(&buf, "Progress             : {s}", .{label}), ray.Vector2{ .x = 40, .y = 40 }, 20, 1.0, ray.RAYWHITE);
                ray.DrawTextEx(this.font, try std.fmt.bufPrintZ(&buf, "Successfully imported: {}", .{this.importer.done.items.len}), ray.Vector2{ .x = 40, .y = 40 + 20 * 1.2 }, 20, 1.0, ray.RAYWHITE);
                ray.DrawTextEx(this.font, try std.fmt.bufPrintZ(&buf, "Errors               : {}", .{this.importer.errors.count()}), ray.Vector2{ .x = 40, .y = 40 + 2 * 20 * 1.2 }, 20, 1.0, ray.RAYWHITE);

                if (this.importer.status == .Idle) {
                    try this.leave_state();
                    this.state = .{ .List = void{} };
                }
            },
            else => std.debug.panic("render_import_state called while in state {s}", .{@tagName(this.state)}),
        }
    }

    fn render_display_state(this: *GUI) !void {
        switch (this.state) {
            .Display => |d| {
                const session = d.activity.sessions.items[0];
                ray.DrawTexture(d.map_texture, 20, 20, ray.WHITE);
                ray.DrawTexture(d.graph_texture, 20, d.map_texture.height + 40, ray.WHITE);

                var buf: [128]u8 = undefined;
                var st: libC.tm = undefined;
                const st64: c_long = @intCast(d.activity.start_time);
                _ = libC.localtime_r(&st64, &st);
                const t = TimeOfDay.from_float(session.elapsed_time);
                ray.DrawTextEx(this.font, try std.fmt.bufPrintZ(&buf, "Start time    : {:02}:{:02}", .{ @as(u32, @bitCast(st.tm_hour)), @as(u32, @bitCast(st.tm_min)) }), ray.Vector2{ .x = @floatFromInt(d.map_texture.width + 40), .y = 40 }, 20, 1.0, ray.RAYWHITE);
                ray.DrawTextEx(this.font, try std.fmt.bufPrintZ(&buf, "Total distance: {d:.3}km", .{session.distance / 1000.0}), ray.Vector2{ .x = @floatFromInt(d.map_texture.width + 40), .y = 40 + 20 * 1.2 }, 20, 1.0, ray.RAYWHITE);
                ray.DrawTextEx(this.font, try std.fmt.bufPrintZ(&buf, "Elapsed time  : {}", .{t}), ray.Vector2{ .x = @floatFromInt(d.map_texture.width + 40), .y = 40 + 2 * 20 * 1.2 }, 20, 1.0, ray.RAYWHITE);
            },
            else => std.debug.panic("render_display_state called while in state {s}", .{@tagName(this.state)}),
        }
    }

    fn leave_state(this: *GUI) !void {
        switch (this.state) {
            .Display => |*d| {
                d.activity.deinit();
                ray.UnloadTexture(d.map_texture);
                ray.UnloadTexture(d.graph_texture);
            },
            else => {},
        }
    }

    fn load_activity(this: *GUI, activity_id: i32) !void {
        const activity = try Activity.load(activity_id, this.allocator, this.db);
        const session = activity.sessions.items[0];
        const map_img = try session.map_image();
        const map_texture = ray.LoadTextureFromImage(map_img);
        ray.UnloadImage(map_img);

        std.debug.print("map_img.height: {}\n", .{map_img.height});
        std.debug.print("map_img.height casted: {}\n", .{@as(u32, @bitCast(map_img.height))});
        std.debug.print("screenHeight {}\n", .{this.screenHeight});
        std.debug.print("screenHeight - map_img.height {}\n", .{this.screenHeight - @as(u32, @bitCast(map_img.height))});
        const graph_img = try session.graph_image(@intCast(this.screenWidth - 40), this.screenHeight - @as(u32, @bitCast(map_img.height)) - 60);
        const graph_texture = ray.LoadTextureFromImage(graph_img);
        ray.UnloadImage(graph_img);

        this.state = .{ .Display = .{
            .activity = activity,
            .map_texture = map_texture,
            .graph_texture = graph_texture,
        } };
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    // const args = try std.process.argsAlloc(allocator);
    // if (args.len != 2) {
    //     std.log.err(
    //         "Incorrect number of arguments: wanted 2, got {d}",
    //         .{args.len},
    //     );
    //     return ProgramError.WrongAmountOfArguments;
    // }

    var db = try zorro.Db.init(allocator, true);
    defer db.deinit();

    var import = try Import.init(db, true);
    defer import.deinit();
    try import.start();

    var gui = try GUI.init(allocator, db, &import);
    defer gui.deinit();
    try gui.show();
}
