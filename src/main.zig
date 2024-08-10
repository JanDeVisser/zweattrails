const std = @import("std");
const fit = @import("fit.zig");
const fittypes = @import("fittypes.zig");
const map = @import("map.zig");

const ray = @cImport(@cInclude("raylib.h"));

pub fn main() !void {
    // Get an allocator.
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    // if (args.len != 2) {
    //     std.log.err(
    //         "Incorrect number of arguments: wanted 2, got {d}",
    //         .{args.len},
    //     );
    //     return ProgramError.WrongAmountOfArguments;
    // }
    const filename = if (args.len == 2) args[1] else "Easy_Wednesday_Workout_Neg_Split_Intervals.fit";

    var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const path = try std.fs.realpathZ(filename, &path_buffer);

    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    var fitfile = try fit.FITFile.init(allocator, file);
    try fitfile.read_all();

    var records = std.ArrayList(fittypes.record).init(allocator);
    var min_altitude: f32 = 10000;
    var max_altitude: f32 = -500;
    var min_lat: f32 = 100.0;
    var max_lat: f32 = -100.0;
    var min_long: f32 = 200.0;
    var max_long: f32 = -200.0;

    var last_time: u32 = 0;
    for (fitfile.messages.items, 0..) |message, ix| {
        switch (message) {
            inline .record => |record| {
                std.debug.assert(record.timestamp > last_time);
                const alt = record.enhanced_altitude;
                if (ix > 0 and alt > -100 and alt < 5000) {
                    min_altitude = @min(min_altitude, alt);
                    max_altitude = @max(max_altitude, alt);
                }
                if (@abs(record.position_lat) > 0.1 and @abs(record.position_long) > 0.1) {
                    min_lat = @min(min_lat, record.position_lat);
                    max_lat = @max(max_lat, record.position_lat);
                    min_long = @min(min_long, record.position_long);
                    max_long = @max(max_long, record.position_long);
                }
                try records.append(record);
                last_time = record.timestamp;
            },
            inline else => {},
        }
    }
    const min_time: u32 = @intCast(records.items[0].timestamp);
    const dt: u32 = @intCast(records.items[records.items.len - 1].timestamp - min_time);
    const dalt: u32 = @intFromFloat(max_altitude - min_altitude);
    _ = dt;
    _ = dalt;

    const sw = map.Coordinates{ .lat = min_lat, .lon = min_long };
    const ne = map.Coordinates{ .lat = max_lat, .lon = max_long };
    const route_box = map.Box{ .sw = sw, .ne = ne };
    const mid = route_box.center();
    std.debug.print("SW: {}, {}\n", .{ sw.lat, sw.lon });
    std.debug.print("NE: {}, {}\n", .{ ne.lat, ne.lon });
    const dpos = @max(max_lat - min_lat, max_long - min_long);
    _ = dpos;
    const b = map.Box{ .sw = sw, .ne = ne };
    var atlas = map.Atlas.for_box(b.with_margins(0.1));
    var images: [9]ray.Image = undefined;
    for (try atlas.get_maps(allocator), 0..) |m, ix| {
        images[ix] = ray.LoadImageFromMemory(".png", m.ptr, @intCast(m.len));
    }

    var m = ray.ImageCopy(images[0]);
    ray.UnloadImage(images[0]);
    ray.ImageResizeCanvas(&m, 768, 768, 0, 0, ray.WHITE);
    for (1..9) |ix| {
        ray.ImageDraw(&m, images[ix], ray.Rectangle{ .x = 0, .y = 0, .width = 256, .height = 256 }, ray.Rectangle{ .x = @floatFromInt((ix % 3) * 256), .y = @floatFromInt((ix / 3) * 256), .width = 256, .height = 256 }, ray.WHITE);
        ray.UnloadImage(images[ix]);
    }

    const box = atlas.box();
    ray.ImageDrawLine(&m, 0, 256, 768, 256, ray.GREEN);
    ray.ImageDrawLine(&m, 0, 512, 768, 512, ray.GREEN);
    ray.ImageDrawLine(&m, 256, 0, 256, 768, ray.GREEN);
    ray.ImageDrawLine(&m, 512, 0, 512, 768, ray.GREEN);

    const r = ray.Rectangle{
        .x = (sw.lon - box.sw.lon) / box.width() * 768,
        .y = (1.0 - (ne.lat - box.sw.lat) / box.height()) * 768,
        .width = (ne.lon - sw.lon) / box.width() * 768,
        .height = (ne.lat - sw.lat) / box.height() * 768,
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
        .x = (mid.lon - box.sw.lon) / box.width() * 768,
        .y = (1.0 - (mid.lat - box.sw.lat) / box.height()) * 768,
    }, 3, ray.BLACK);

    var prev_x: ?f32 = null;
    var prev_y: ?f32 = null;
    for (records.items, 0..) |record, ix| {
        if (ix == 0) {
            continue;
        }
        if (@abs(record.position_lat) < 0.1 or @abs(record.position_long) < 0.1) {
            continue;
        }
        const dlat = 1.0 - (record.position_lat - box.sw.lat) / box.height();
        const dlon = (record.position_long - box.sw.lon) / box.width();
        const x: f32 = 768 * dlon;
        const y: f32 = 768 * dlat;
        if (prev_x != null) {
            ray.ImageDrawLineV(&m, ray.Vector2{ .x = prev_x.?, .y = prev_y.? }, ray.Vector2{ .x = x, .y = y }, ray.RED);
        }
        prev_x = x;
        prev_y = y;
    }
    ray.ImageCrop(&m, square);
    ray.ImageResize(&m, 256, 256);

    std.debug.print("Read {} messages\n", .{fitfile.messages.items.len});
    std.debug.print("Read {} records\n", .{records.items.len});

    const screenWidth = 800;
    const screenHeight = 800;

    ray.InitWindow(screenWidth, screenHeight, "raylib [core] example - basic window");
    defer ray.CloseWindow();

    const texture = ray.LoadTextureFromImage(m);
    defer ray.UnloadTexture(texture);
    ray.UnloadImage(m);
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
