const std = @import("std");

const ray = @cImport({
    @cInclude("raylib.h");
});

const fitbasetype = @import("fitbasetype.zig");
const fittypes = @import("fittypes.zig");

const FITBaseType = fitbasetype.FITBaseType;

const ProgramError = error{
    WrongAmountOfArguments,
    UnknownEnumeration,
    UnknownMessage,
};

pub fn mesg_num_of_type(comptime MesgType: type) fittypes.mesg_num {
    var it = std.mem.splitBackwardsScalar(u8, @typeName(MesgType), '.');
    const mesg_name = it.next() orelse unreachable;
    inline for (@typeInfo(fittypes.mesg_num).Enum.fields) |enum_fld| {
        if (std.mem.eql(u8, mesg_name, enum_fld.name)) {
            return @enumFromInt(enum_fld.value);
        }
    }
    unreachable;
}

pub fn field_map_entry_for_num(comptime MesgType: type, field_num: u32) fittypes.FieldMapEntry {
    const mesg_num = mesg_num_of_type(MesgType);
    for (fittypes.field_map) |entry| {
        if (entry.mesg_num == mesg_num and field_num == entry.field_num) {
            return entry;
        }
    }
    unreachable;
}

pub fn field_name_for_num(comptime MesgType: type, field_num: u32) []const u8 {
    const mesg_num = mesg_num_of_type(MesgType);
    for (fittypes.field_map) |entry| {
        if (entry.mesg_num == mesg_num and field_num == entry.field_num) {
            return entry.field_name;
        }
    }
    unreachable;
}

pub const FITHeader = struct {
    protocol_version: f32,
    profile_version: f32,
    data_size: u32,
    crc: u16,

    pub fn decode(fitfile: *FITFile) !FITHeader {
        const hdr_size = try fitfile.read(u8, true);
        if (hdr_size != 14) {
            return error.InvalidFITHeaderSize;
        }
        var printbuf: [16]u8 = undefined;
        const protocol_version_int = try fitfile.read(u8, true);
        const protocol_version = try std.fmt.parseFloat(f32, try std.fmt.bufPrint(&printbuf, "{}.{}", .{ protocol_version_int >> 4, protocol_version_int & 0x0F }));
        const profile_version_bytes: u16 = try fitfile.read(u16, true);
        const profile_version = try std.fmt.parseFloat(f32, try std.fmt.bufPrint(&printbuf, "{}.{}", .{ profile_version_bytes / 100, profile_version_bytes % 100 }));
        const data_size = try fitfile.read(u32, true);
        const data_type = try fitfile.read_bytes(4);
        if (!std.mem.eql(u8, data_type, ".FIT")) {
            return error.FITDecodeError;
        }
        return FITHeader{
            .protocol_version = protocol_version,
            .profile_version = profile_version,
            .data_size = data_size,
            .crc = try fitfile.read(u16, true),
        };
    }

    pub fn dump(hdr: *const FITHeader) void {
        std.debug.print("Header:\n", .{});
        std.debug.print("Header size: 14\n", .{});
        std.debug.print("Protocol version: {d:3.1}\n", .{hdr.protocol_version});
        std.debug.print("Profile version: {d:4.1}\n", .{hdr.profile_version});
        std.debug.print("Data size: {}\n", .{hdr.data_size});
    }
};

const FieldDefinition = struct {
    field_definition_number: u8,
    size: u8,
    base_type: FITBaseType,
    is_array: bool,
};

const DeveloperFieldDefinition = struct {
    field_number: u8,
    size: u8,
    developer_data_index: u8,
};

const RecordDefinition = struct {
    local_message_type: u4,
    little_endian: bool,
    global_message_number: u16,
    num_fields: u8,
    fields: []FieldDefinition,
    num_developer_fields: u8 = 0,
    developer_fields: []DeveloperFieldDefinition = &[0]DeveloperFieldDefinition{},
    mesg_num: ?fittypes.mesg_num,
    mesg_type: []const u8,

    pub fn decode(fitfile: *FITFile, hdr: u8) !RecordDefinition {
        const has_developer_data = hdr & 0x20 == 0x20;
        _ = fitfile.read(u8, true) catch return error.NoDefinitionReservedByte;
        const little_endian = (fitfile.read(u8, true) catch return error.NoDefinitionArchitectureByte) == 0;
        const global_message_number = fitfile.read(u16, little_endian) catch return error.NoDefinitionGlobalMessageNumber;
        var mesg_num: ?fittypes.mesg_num = null;
        var mesg_type: []const u8 = "Vendor";

        if (global_message_number < @intFromEnum(fittypes.mesg_num.mfg_range_min)) {
            inline for (@typeInfo(fittypes.mesg_num).Enum.fields) |enum_fld| {
                if (enum_fld.value == global_message_number) {
                    mesg_num = @enumFromInt(global_message_number);
                    mesg_type = enum_fld.name;
                }
            }
        } else {
            mesg_type = try std.fmt.allocPrint(fitfile.allocator, "Vendor #{}", .{global_message_number});
        }

        const num_fields = fitfile.read(u8, little_endian) catch return error.NoDefinitionNumberOfFields;
        var ret = RecordDefinition{
            .local_message_type = @intCast(hdr & 0x0F),
            .little_endian = little_endian,
            .global_message_number = global_message_number,
            .mesg_num = mesg_num,
            .mesg_type = mesg_type,
            .num_fields = num_fields,
            .fields = try fitfile.allocator.alloc(FieldDefinition, num_fields),
        };
        for (0..ret.num_fields) |ix| {
            const field_definition_number = try fitfile.read(u8, little_endian);
            const field_size = try fitfile.read(u8, little_endian);
            const base_type_int = try fitfile.read(u8, little_endian);
            const base_type: ?FITBaseType = FITBaseType.from_int(base_type_int);
            if (base_type == null) {
                std.debug.print("mesg_num {} field {} base_type {2x:002} ⚡️⚡️\n", .{ global_message_number, field_definition_number, base_type_int });
                continue;
            }
            ret.fields[ix] = FieldDefinition{
                .field_definition_number = field_definition_number,
                .size = field_size,
                .base_type = base_type.?,
                .is_array = if (base_type) |bt|
                    switch (bt) {
                        inline .string, .byte => false,
                        inline else => |bt_| field_size > @sizeOf(bt_.zig_type()),
                    }
                else
                    false,
            };
        }
        if (has_developer_data) {
            ret.num_developer_fields = fitfile.read(u8, little_endian) catch return error.NoDefinitionNumberOfDeveloperFields;
            ret.developer_fields = try fitfile.allocator.alloc(DeveloperFieldDefinition, ret.num_developer_fields);
            for (0..ret.num_developer_fields) |ix| {
                ret.developer_fields[ix] = DeveloperFieldDefinition{
                    .field_number = try fitfile.read(u8, little_endian),
                    .size = try fitfile.read(u8, little_endian),
                    .developer_data_index = try fitfile.read(u8, little_endian),
                };
            }
        }
        return ret;
    }

    pub fn dump(this: RecordDefinition) void {
        std.debug.print("Definition for {s}: {} ({}): ", .{ this.mesg_type, this.global_message_number, this.local_message_type });
        for (this.fields) |fld| {
            std.debug.print("{s}: {s} ({any} {}), ", .{ fld.name, switch (fld.base_type) {
                inline else => |bt| @tagName(bt),
            }, fld.field_definition_number, fld.size });
        }
        if (this.num_developer_fields > 0) {
            std.debug.print(" {} developer fields", .{this.num_developer_fields});
        }
        std.debug.print("\n", .{});
    }

    pub fn get_message_type(this: RecordDefinition) ?type {
        inline for (@typeInfo(fittypes.mesg_num).Enum.fields) |fld| {
            if (fld.value == this.global_message_number) {
                return @field(fittypes, fld.name);
            }
        }
        return null;
    }
};

const DataFieldValue = union(FITBaseType) {
    @"enum": u8,
    uint8: u8,
    uint8z: u8,
    sint8: i8,
    uint16: u16,
    uint16z: u16,
    sint16: i16,
    uint32: u32,
    uint32z: u32,
    sint32: i32,
    uint64: u64,
    uint64z: u64,
    sint64: i64,
    float32: f32,
    float64: f64,
    string: []u8,
    byte: []u8,
    bool: bool,

    pub fn dump(this: DataFieldValue) void {
        switch (this) {
            inline .string => std.debug.print("\"{s}\" ", .{this.string}),
            inline else => |v| std.debug.print("{any} ", .{v}),
        }
    }

    pub fn get(this: DataFieldValue, comptime T: type) T {
        switch (this) {
            inline .@"enum", .uint8, .uint8z => |i| {
                if (T == u8) {
                    return i;
                }
            },
            inline .sint8 => |i| {
                if (T == i8) {
                    return i;
                }
            },
            inline .uint16, .uint16z => |i| {
                if (T == u16) {
                    return i;
                }
            },
            inline .sint16 => |i| {
                if (T == i16) {
                    return i;
                }
            },
            inline .uint32, .uint32z => |i| {
                if (T == u32) {
                    return i;
                }
            },
            inline .sint32 => |i| {
                if (T == i32) {
                    return i;
                }
            },
            inline .uint64, .uint64z => |i| {
                if (T == u64) {
                    return i;
                }
            },
            inline .sint64 => |i| {
                if (T == i64) {
                    return i;
                }
            },
            inline .string => |s| {
                if (T == []const u8) {
                    return @constCast(s);
                }
            },
            inline .byte => |buf| {
                if (T == []u8) {
                    return buf;
                }
            },
            inline .bool => |b| {
                if (T == bool) {
                    return b;
                }
            },
            inline .float32 => |flt| {
                if (T == f32) {
                    return flt;
                }
                if (T == f64) {
                    return @floatCast(flt);
                }
            },
            inline .float64 => |flt| {
                if (T == f64) {
                    return flt;
                }
                if (T == f32) {
                    return @floatCast(flt);
                }
            },
        }
        std.debug.panic("Cannet get field value of type '{s}' as {s}", .{ switch (this) {
            inline else => @tagName(this),
        }, @typeName(T) });
        unreachable;
    }
};

pub const DataField = struct {
    field_definition_number: u8,
    base_type: FITBaseType,
    def: ?FieldDefinition,
    value: union(enum) {
        scalar_field: DataFieldValue,
        array_field: []DataFieldValue,
    },

    pub fn dump(this: DataField) void {
        switch (this.value) {
            inline .scalar_field => |value| value.dump(),
            inline .array_field => |arr| {
                std.debug.print("[ ", .{});
                for (arr) |v| {
                    v.dump();
                }
                std.debug.print(" ]", .{});
            },
        }
    }

    pub fn get_value(this: DataField, comptime T: type) T {
        return switch (this.value) {
            .scalar_field => |v| v.get(T),
            .array_field => |arr| blk: {
                std.debug.assert(arr.len == 1);
                break :blk arr[0].get(T);
            },
        };
    }

    pub fn get_scaled_value(this: DataField, comptime T: type, scale: f32, offset: f32) f32 {
        const v = switch (this.value) {
            .scalar_field => |v| v.get(T),
            .array_field => |arr| blk: {
                std.debug.assert(arr.len == 1);
                break :blk arr[0].get(T);
            },
        };
        return @as(f32, v) / scale - offset;
    }

    pub fn get_array(this: DataField, comptime T: type, allocator: std.mem.Allocator) []T {
        var data_values_one: [1]DataFieldValue = undefined;
        const data_values: []DataFieldValue = switch (this.value) {
            inline .scalar_field => |value| blk: {
                data_values_one[0] = value;
                break :blk &data_values_one;
            },
            inline .array_field => |arr| arr,
        };
        const values = allocator.alloc(T, data_values.len) catch std.debug.panic("Out of Memory!", .{});
        for (data_values, 0..) |value, ix| {
            values[ix] = value.get(T);
        }
        return values;
    }
};

pub const DataRecord = struct {
    allocator: std.mem.Allocator,
    definition: RecordDefinition,
    global_message_number: u16,
    fields: []DataField,

    pub fn dump(this: *const DataRecord) void {
        std.debug.print("{s}: {} ({}): ", .{ this.definition.mesg_type, this.global_message_number, this.definition.local_message_type });
        for (this.fields) |fld| {
            const flddef = fld.def orelse FieldDefinition{
                .base_type = fld.base_type,
                .field_definition_number = fld.field_definition_number,
                .name = "Unknown",
                .size = 0,
                .is_array = false,
            };
            std.debug.print("{s}: ", .{flddef.name});
            fld.dump();
        }
        std.debug.print("\n", .{});
    }

    pub fn decode(fitfile: *FITFile, hdr: u8) !?DataRecord {
        const local_message_type: u4 = @intCast(hdr & 0x0F);
        const def = fitfile.definitions[local_message_type];

        var ret = DataRecord{
            .allocator = fitfile.allocator,
            .global_message_number = def.global_message_number,
            .definition = def,
            .fields = try fitfile.allocator.alloc(DataField, def.fields.len),
        };
        for (def.fields, 0..) |fld, ix| {
            inline for (@typeInfo(FITBaseType).Enum.fields) |enum_value| {
                const bt: FITBaseType = @enumFromInt(enum_value.value);
                if (bt == fld.base_type) {
                    const T = bt.zig_type();
                    ret.fields[ix] = try DataRecord.read_field(fitfile, bt, fld, null, local_message_type);
                    if (fld.field_definition_number == 253 and T == u32) {
                        fitfile.last_timestamp = ret.fields[ix].value.scalar_field.uint32;
                    }
                }
            }
        }
        for (def.developer_fields) |fld| {
            _ = try fitfile.read_bytes(fld.size);
        }
        return ret;
    }

    fn read_one(fitfile: *FITFile, comptime bt: FITBaseType, fld: FieldDefinition, new_timestamp: ?u32, local_message_type: u4) !DataFieldValue {
        const T = bt.zig_type();
        const def = fitfile.definitions[local_message_type];
        const little_endian = def.little_endian;
        const value: T = if (new_timestamp != null and fld.field_definition_number == 253 and T == u32) if_blk: {
            fitfile.last_timestamp = new_timestamp.?;
            break :if_blk fitfile.last_timestamp;
        } else switch (bt) {
            inline .float32, .float64, .bool => try fitfile.read_float(T, little_endian),
            inline .string, .byte => try fitfile.read_bytes(fld.size),
            inline else => try fitfile.read(T, little_endian),
        };
        return @unionInit(DataFieldValue, @tagName(bt), value);
    }

    fn read_field(fitfile: *FITFile, comptime bt: FITBaseType, fld: FieldDefinition, new_timestamp: ?u32, local_message_type: u4) !DataField {
        const T = bt.zig_type();
        if (fld.is_array) {
            const num = fld.size / @sizeOf(T);
            var values: []DataFieldValue = try fitfile.allocator.alloc(DataFieldValue, num);
            for (0..num) |ix| {
                values[ix] = try DataRecord.read_one(fitfile, bt, fld, new_timestamp, local_message_type);
            }
            return DataField{
                .field_definition_number = fld.field_definition_number,
                .base_type = fld.base_type,
                .def = fld,
                .value = .{ .array_field = values },
            };
        } else {
            const data_value = try DataRecord.read_one(fitfile, bt, fld, new_timestamp, local_message_type);
            return DataField{
                .field_definition_number = fld.field_definition_number,
                .base_type = fld.base_type,
                .def = fld,
                .value = .{ .scalar_field = data_value },
            };
        }
    }

    pub fn decode_compressed_timestamp(fitfile: *FITFile, hdr: u8) !?DataRecord {
        const local_message_type: u4 = @intCast((hdr & 0x60) >> 5);
        const time_delta: u32 = @intCast(hdr & 0x1F);
        const last_delta: u32 = fitfile.last_timestamp & 0x1F;
        const new_timestamp = if (time_delta >= last_delta) fitfile.last_timestamp + time_delta else fitfile.last_timestamp + time_delta + 0x20;
        const def = fitfile.definitions[local_message_type];

        var ret = DataRecord{
            .allocator = fitfile.allocator,
            .global_message_number = def.global_message_number,
            .definition = def,
            .fields = try fitfile.allocator.alloc(DataField, def.fields.len),
        };
        for (def.fields, 0..) |fld, ix| {
            inline for (@typeInfo(FITBaseType).Enum.fields) |enum_value| {
                const bt: FITBaseType = @enumFromInt(enum_value.value);
                if (bt == fld.base_type) {
                    ret.fields[ix] = try DataRecord.read_field(fitfile, bt, fld, new_timestamp, local_message_type);
                }
            }
        }
        for (def.developer_fields) |fld| {
            _ = try fitfile.read_bytes(fld.size);
        }
        return ret;
    }

    var count: u32 = 0;

    fn create_message_by_name(this: DataRecord, comptime name: []const u8) !fittypes.FITMessage {
        std.debug.print("{} Reading {s}\n", .{ count, name });
        count += 1;
        inline for (@typeInfo(fittypes).Struct.decls) |decl| {
            if (std.mem.eql(u8, decl.name, name) and @hasField(fittypes.FITMessage, decl.name)) {
                return @unionInit(fittypes.FITMessage, decl.name, try @field(fittypes, decl.name).init(this));
            }
        }
        unreachable;
    }

    pub fn create_message(this: DataRecord) !fittypes.FITMessage {
        const mesg_num = this.definition.mesg_num orelse {
            std.debug.print("Unknown mesg_num {}\n", .{this.definition.global_message_number});
            return error.UnknownMessage;
        };
        inline for (@typeInfo(fittypes.mesg_num).Enum.fields) |enum_fld| {
            if (enum_fld.value == @intFromEnum(mesg_num)) {
                return this.create_message_by_name(enum_fld.name);
            }
        }
        unreachable;
    }
};

const FITFile = struct {
    allocator: std.mem.Allocator,
    file: std.fs.File,
    definitions: [16]RecordDefinition = undefined,
    developer_data_ids: std.AutoHashMap(u8, fittypes.developer_data_id),
    developer_fields: std.AutoHashMap(u8, fittypes.field_description),
    data: std.ArrayList(DataRecord),
    messages: std.ArrayList(fittypes.FITMessage),
    last_timestamp: u32 = 0,
    header: FITHeader = undefined,
    bytes_read: usize,

    pub fn init(allocator: std.mem.Allocator, file: std.fs.File) !FITFile {
        var ret = FITFile{
            .allocator = allocator,
            .file = file,
            .developer_data_ids = std.AutoHashMap(u8, fittypes.developer_data_id).init(allocator),
            .developer_fields = std.AutoHashMap(u8, fittypes.field_description).init(allocator),
            .data = std.ArrayList(DataRecord).init(allocator),
            .messages = std.ArrayList(fittypes.FITMessage).init(allocator),
            .bytes_read = 0,
        };
        ret.header = try FITHeader.decode(&ret);
        return ret;
    }

    pub fn read_bytes(this: *FITFile, length: u16) ![]u8 {
        const buf = try this.allocator.alloc(u8, length);
        const sz = try this.file.reader().read(buf);
        if (sz < length) {
            return error.ShortRead;
        }
        this.bytes_read += sz;
        return buf;
    }

    fn read_float(this: *FITFile, comptime T: type, little_endian: bool) !T {
        if (T != f32 and T != f64) {
            @panic("read_float called for non-float type");
        }
        const IntType = if (T == f32) u32 else u64;
        var rdr = this.file.reader();

        var buf: [@sizeOf(T)]u8 = undefined;
        this.bytes_read += try rdr.read(&buf);
        const value: IntType = @as(*IntType, @ptrCast(@alignCast(&buf))).*;
        if (little_endian) {
            return @bitCast(std.mem.littleToNative(IntType, value));
        }
        return @bitCast(std.mem.bigToNative(IntType, value));
    }

    pub fn read_string(this: *FITFile) ![]u8 {
        var buf = std.ArrayList(u8).init(this.allocator);
        while (true) {
            var ch = [_]u8{0};
            this.bytes_read += try this.file.reader().read(&ch);
            if (ch[0] == 0) {
                return try buf.toOwnedSlice();
            }
            try buf.append(ch[0]);
        }
        unreachable;
    }

    pub fn read(this: *FITFile, T: type, little_endian: bool) !T {
        const ret = try this.file.reader().readInt(T, if (little_endian) std.builtin.Endian.little else std.builtin.Endian.big);
        this.bytes_read += @sizeOf(T);
        return ret;
    }

    pub fn read_message(this: *FITFile) !void {
        const hdr = this.read(u8, true) catch return error.FITFileExhausted;
        if (hdr & 0x40 == 0x40) {
            const definition = try RecordDefinition.decode(this, hdr);
            this.definitions[definition.local_message_type] = definition;
        } else {
            const data_record_maybe = if (hdr & 0x80 == 0x80)
                try DataRecord.decode_compressed_timestamp(this, hdr)
            else
                try DataRecord.decode(this, hdr);
            if (data_record_maybe) |data_record| {
                try this.data.append(data_record);
                const msg = data_record.create_message() catch |err| switch (err) {
                    error.UnknownMessage => return,
                    else => return err,
                };
                try this.messages.append(msg);
                switch (msg) {
                    .developer_data_id => |dev_data_id| try this.register_developer_data_id(dev_data_id),
                    .field_description => |fld_description| try this.register_field_description(fld_description),
                    else => {},
                }
            }
        }
    }

    pub fn read_all(this: *FITFile) !void {
        while (this.bytes_read < this.header.data_size) {
            this.read_message() catch |err| switch (err) {
                error.FITFileExhausted => break,
                else => return err,
            };
        }
    }

    fn register_developer_data_id(this: *FITFile, dev_data_id: fittypes.developer_data_id) !void {
        _ = this;
        _ = dev_data_id;
    }

    fn register_field_description(this: *FITFile, fld_description: fittypes.field_description) !void {
        _ = this;
        _ = fld_description;
    }
};

const Coordinates = struct {
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
};

const Box = struct {
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
};

const Tile = struct {
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

const Atlas = struct {
    zoom: u5,
    x: u32,
    y: u32,
    maps: ?[9][]const u8 = null,

    pub fn for_box(b: Box) Atlas {
        var zoom: u5 = 15;
        const mid = b.center();
        while (zoom > 0) {
            const mid_tile = Tile.for_coordinates(mid, zoom);
            const tile_box = mid_tile.box();
            if (tile_box.width() > b.width() and tile_box.height() > b.height()) {
                zoom += 1;
                const t = Tile.for_coordinates(mid, zoom);
                return Atlas{
                    .zoom = zoom,
                    .x = t.x - 1,
                    .y = t.y - 1,
                };
            }
            zoom -= 1;
        }
        unreachable;
    }

    fn tile(this: Atlas, ix: usize) Tile {
        std.debug.assert(ix < 9);
        const i: u32 = @intCast(ix);
        return Tile{ .zoom = this.zoom, .x = this.x + i % 3, .y = this.y + i / 3 };
    }

    pub fn get_maps(this: *Atlas, allocator: std.mem.Allocator) ![9][]const u8 {
        if (this.maps) |ret| {
            return ret;
        }
        const maps: [9][]const u8 = undefined;
        this.maps = maps;
        for (0..9) |ix| {
            this.maps.?[ix] = try this.tile(ix).get_map(allocator);
        }
        return this.maps.?;
    }

    pub fn box(this: Atlas) Box {
        const t_sw = Tile{ .zoom = this.zoom, .x = this.x, .y = this.y + 2 };
        const t_ne = Tile{ .zoom = this.zoom, .x = this.x + 2, .y = this.y };
        return Box{ .sw = t_sw.box().sw, .ne = t_ne.box().ne };
    }

    pub fn sub_box(this: Atlas, ix: usize) Box {
        return this.tile(ix).box();
    }
};

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

    var fitfile = try FITFile.init(allocator, file);
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

    const sw = Coordinates{ .lat = min_lat, .lon = min_long };
    const ne = Coordinates{ .lat = max_lat, .lon = max_long };
    const route_box = Box{ .sw = sw, .ne = ne };
    const mid = route_box.center();
    std.debug.print("SW: {}, {}\n", .{ sw.lat, sw.lon });
    std.debug.print("NE: {}, {}\n", .{ ne.lat, ne.lon });
    const dpos = @max(max_lat - min_lat, max_long - min_long);
    _ = dpos;
    const b = Box{ .sw = sw, .ne = ne };
    var atlas = Atlas.for_box(b.with_margins(0.1));
    var map: [9]ray.Image = undefined;
    for (try atlas.get_maps(allocator), 0..) |m, ix| {
        map[ix] = ray.LoadImageFromMemory(".png", m.ptr, @intCast(m.len));
    }

    var m = ray.ImageCopy(map[0]);
    ray.UnloadImage(map[0]);
    ray.ImageResizeCanvas(&m, 768, 768, 0, 0, ray.WHITE);
    for (1..9) |ix| {
        ray.ImageDraw(&m, map[ix], ray.Rectangle{ .x = 0, .y = 0, .width = 256, .height = 256 }, ray.Rectangle{ .x = @floatFromInt((ix % 3) * 256), .y = @floatFromInt((ix / 3) * 256), .width = 256, .height = 256 }, ray.WHITE);
        ray.UnloadImage(map[ix]);
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
