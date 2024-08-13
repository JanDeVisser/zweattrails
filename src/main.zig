const std = @import("std");
const fit = @import("fit.zig");
const fittypes = @import("fittypes.zig");
const map = @import("map.zig");

const ray = @cImport(@cInclude("raylib.h"));

pub const Activity = struct {
    fitfile: fit.FITFile,
    records: std.ArrayList(fittypes.record),
    elevation_range: ray.Vector2,
    route_area: map.Box,
    atlas: map.Atlas,

    pub fn init(filename: []const u8, allocator: std.mem.Allocator) !Activity {
        const fitfile = try fit.FITFile.init(allocator, filename);

        var records = std.ArrayList(fittypes.record).init(allocator);
        var elevation_range = ray.Vector2{ .x = 10000, .y = -500 };
        var route_area = map.Box{ .sw = map.Coordinates{ .lat = 100, .lon = 200 }, .ne = map.Coordinates{ .lat = -100, .lon = -200 } };

        var last_time: u32 = 0;
        for (fitfile.messages.items, 0..) |message, ix| {
            switch (message) {
                inline .record => |record| {
                    std.debug.assert(record.timestamp > last_time);
                    const alt = record.enhanced_altitude;
                    if (ix > 0 and alt > -100.0 and alt < 5000.0) {
                        elevation_range.x = @min(elevation_range.x, alt);
                        elevation_range.y = @max(elevation_range.y, alt);
                    }
                    if (@abs(record.position_lat) > 0.1 and @abs(record.position_long) > 0.1) {
                        route_area.sw.lat = @min(route_area.sw.lat, record.position_lat);
                        route_area.ne.lat = @max(route_area.ne.lat, record.position_lat);
                        route_area.sw.lon = @min(route_area.sw.lon, record.position_long);
                        route_area.ne.lon = @max(route_area.ne.lon, record.position_long);
                    }
                    try records.append(record);
                    last_time = record.timestamp;
                },
                inline else => {},
            }
        }
        return Activity{
            .fitfile = fitfile,
            .records = records,
            .elevation_range = elevation_range,
            .route_area = route_area,
            .atlas = map.Atlas.for_box(route_area, 2, 2),
        };
    }

    pub fn deinit(this: *Activity) void {
        this.fitfile.deinit();
        this.atlas.deinit();
    }

    pub fn map_image(this: *Activity, allocator: std.mem.Allocator) !ray.Image {
        const mid = this.route_area.center();
        var images: []ray.Image = try allocator.alloc(ray.Image, this.atlas.num_tiles);
        defer allocator.free(images);
        for (try this.atlas.get_maps(allocator), 0..) |m, ix| {
            images[ix] = ray.LoadImageFromMemory(".png", m.ptr, @intCast(m.len));
        }

        var m = ray.ImageCopy(images[0]);
        ray.UnloadImage(images[0]);
        ray.ImageResizeCanvas(&m, 5 * 256, 5 * 256, 0, 0, ray.WHITE);
        for (1..this.atlas.num_tiles) |ix| {
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
        ray.ImageDrawLine(&m, 0, 512, 1280, 512, ray.GREEN);
        ray.ImageDrawLine(&m, 0, 768, 1280, 768, ray.GREEN);
        ray.ImageDrawLine(&m, 512, 0, 512, 1280, ray.GREEN);
        ray.ImageDrawLine(&m, 768, 0, 768, 1280, ray.GREEN);

        const r = ray.Rectangle{
            .x = (this.route_area.sw.lon - box.sw.lon) / box.width() * 1280,
            .y = (1.0 - (this.route_area.ne.lat - box.sw.lat) / box.height()) * 1280,
            .width = this.route_area.width() / box.width() * 1280,
            .height = this.route_area.height() / box.height() * 1280,
        };
        const fat = ray.Rectangle{
            .x = r.x - (r.width * 0.05),
            .y = r.y - (r.height * 0.05),
            .width = r.width * 1.1,
            .height = r.height * 1.1,
        };
        const square = ray.Rectangle{
            .x = if (fat.height > fat.width) (fat.x + fat.width / 2) - fat.height / 2 else fat.x,
            .y = if (fat.height < fat.width) (fat.y + fat.height / 2) - fat.width / 2 else fat.y,
            .width = @max(fat.width, fat.height),
            .height = @max(fat.width, fat.height),
        };

        ray.ImageDrawRectangleLines(&m, r, 2, ray.SKYBLUE);
        ray.ImageDrawRectangleLines(&m, fat, 2, ray.PINK);
        ray.ImageDrawRectangleLines(&m, square, 2, ray.DARKBLUE);
        ray.ImageDrawCircleV(&m, .{
            .x = (mid.lon - box.sw.lon) / box.width() * 1280,
            .y = (1.0 - (mid.lat - box.sw.lat) / box.height()) * 1280,
        }, 3, ray.BLACK);

        var prev_x: ?f32 = null;
        var prev_y: ?f32 = null;
        for (this.records.items, 0..) |record, ix| {
            if (ix == 0) {
                continue;
            }
            if (@abs(record.position_lat) < 0.1 or @abs(record.position_long) < 0.1) {
                continue;
            }
            const dlat = 1.0 - (record.position_lat - box.sw.lat) / box.height();
            const dlon = (record.position_long - box.sw.lon) / box.width();
            const x: f32 = 1280 * dlon;
            const y: f32 = 1280 * dlat;
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
        // ray.ImageResize(&m, 512, 512);
        // ray.ImageResize(&m, 984, 984);
        return m;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const args = try std.process.argsAlloc(allocator);
    // if (args.len != 2) {
    //     std.log.err(
    //         "Incorrect number of arguments: wanted 2, got {d}",
    //         .{args.len},
    //     );
    //     return ProgramError.WrongAmountOfArguments;
    // }
    const filename = if (args.len == 2) args[1] else "Easy_Wednesday_Workout_Neg_Split_Intervals.fit";

    var activity = try Activity.init(filename, allocator);
    defer activity.deinit();

    const screenWidth = 1550;
    const screenHeight = 1024;

    ray.InitWindow(screenWidth, screenHeight, "Zweattrails");
    defer ray.CloseWindow();

    const img = try activity.map_image(allocator);
    const texture = ray.LoadTextureFromImage(img);
    defer ray.UnloadTexture(texture);
    ray.UnloadImage(img);
    ray.SetTargetFPS(60);

    var frame: u32 = 0;
    while (!ray.WindowShouldClose()) {
        defer frame += 1;
        ray.BeginDrawing();
        defer ray.EndDrawing();

        ray.ClearBackground(ray.DARKGRAY);
        ray.DrawTexture(texture, 20, 20, ray.WHITE);

        // var prev_x: ?c_int = null;
        // var prev_y: ?c_int = null;
        // for (records.items, 0..) |record, ix| {
        //     if (ix == 0) {
        //         continue;
        //     }
        //     if (@abs(record.position_lat) < 0.1 or @abs(record.position_long) < 0.1) {
        //         continue;
        //     }
        //     if (frame == 0 and ix < 50) {
        //         std.debug.print("ix: {} lat: {} lon: {}\n", .{ ix, record.position_lat, record.position_long });
        //     }
        //     const x: c_int = 20 + @as(c_int, @intFromFloat(@as(f32, @floatFromInt(screenHeight - 40)) * (record.position_long - min_long) / dpos));
        //     const y: c_int = screenHeight - (20 + @as(c_int, @intFromFloat(@as(f32, @floatFromInt(screenHeight - 40)) * (record.position_lat - min_lat) / dpos)));
        //     if (prev_x != null) {
        //         ray.DrawLine(prev_x.?, prev_y.?, x, y, ray.RAYWHITE);
        //     }
        //     prev_x = x;
        //     prev_y = y;
        //    const x: c_int = @intCast(20 + 760 * (record.timestamp - min_time) / dt);
        //    const alt = record.enhanced_altitude;
        //     if (ix > 0 and alt > -100 and alt < 5000) {
        //         if (frame == 0 and ix < 50) {
        //             std.debug.print("ix: {} min_alt: {} alt: {} dalt: {}\n", .{ ix, min_altitude, alt, dalt });
        //         }
        //         const y: c_int = @intCast(430 - 410 * @as(u32, @intFromFloat(alt - min_altitude)) / dalt);
        //         if (prev_x != null) {
        //             ray.DrawLine(prev_x.?, prev_y.?, x, y, ray.RAYWHITE);
        //         }
        //         prev_x = x;
        //         prev_y = y;
        //     }
        //}
    }
}
