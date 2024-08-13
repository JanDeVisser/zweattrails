const std = @import("std");
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
    allocator: std.mem.Allocator,
    local_message_type: u4,
    little_endian: bool,
    global_message_number: u16,
    num_fields: u8,
    fields: []FieldDefinition,
    num_developer_fields: u8 = 0,
    developer_fields: ?[]DeveloperFieldDefinition = null,
    mesg_num: ?fittypes.mesg_num,
    mesg_type: []const u8,

    pub fn deinit(this: RecordDefinition) void {
        this.allocator.free(this.fields);
        if (this.developer_fields) |developer_fields| {
            this.allocator.free(developer_fields);
        }
    }

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
            .allocator = fitfile.allocator,
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
                ret.developer_fields.?[ix] = DeveloperFieldDefinition{
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

    pub fn deinit(this: DataFieldValue, allocator: std.mem.Allocator) void {
        switch (this) {
            inline .string => |s| allocator.free(s),
            inline .byte => |b| allocator.free(b),
            inline else => {},
        }
    }

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

    pub fn deinit(this: DataField, allocator: std.mem.Allocator) void {
        switch (this.value) {
            .scalar_field => |fld| fld.deinit(allocator),
            .array_field => |arr| {
                for (arr) |fld| {
                    fld.deinit(allocator);
                }
                allocator.free(arr);
            },
        }
    }

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

    pub fn deinit(this: DataRecord) void {
        this.definition.deinit();
        for (this.fields) |fld| {
            fld.deinit(this.allocator);
        }
        this.allocator.free(this.fields);
    }

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
        if (def.developer_fields) |dev_flds| {
            for (dev_flds) |fld| {
                _ = try fitfile.read_bytes(fld.size);
            }
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
        if (def.developer_fields) |dev_flds| {
            for (dev_flds) |fld| {
                _ = try fitfile.read_bytes(fld.size);
            }
        }
        return ret;
    }

    var count: u32 = 0;

    fn create_message_by_name(this: DataRecord, comptime name: []const u8) !fittypes.FITMessage {
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

pub const FITFile = struct {
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

    pub fn init(allocator: std.mem.Allocator, filename: []const u8) !FITFile {
        var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
        const path = try std.fs.realpath(filename, &path_buffer);
        const file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();
        var ret = FITFile{
            .allocator = allocator,
            .file = file,
            .developer_data_ids = std.AutoHashMap(u8, fittypes.developer_data_id).init(allocator),
            .developer_fields = std.AutoHashMap(u8, fittypes.field_description).init(allocator),
            .data = std.ArrayList(DataRecord).init(allocator),
            .messages = std.ArrayList(fittypes.FITMessage).init(allocator),
            .bytes_read = 0,
        };
        errdefer ret.deinit();
        ret.header = try FITHeader.decode(&ret);
        try ret.read_all();
        return ret;
    }

    pub fn deinit(this: *FITFile) void {
        this.messages.deinit();
        for (this.data.items) |data| {
            data.deinit();
        }
        this.data.deinit();
        this.developer_data_ids.deinit();
        this.developer_fields.deinit();
    }

    fn read_bytes(this: *FITFile, length: u16) ![]u8 {
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

    fn read_string(this: *FITFile) ![]u8 {
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

    fn read(this: *FITFile, T: type, little_endian: bool) !T {
        const ret = try this.file.reader().readInt(T, if (little_endian) std.builtin.Endian.little else std.builtin.Endian.big);
        this.bytes_read += @sizeOf(T);
        return ret;
    }

    fn read_message(this: *FITFile) !void {
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

    fn read_all(this: *FITFile) !void {
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
