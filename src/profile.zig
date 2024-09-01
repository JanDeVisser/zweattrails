const std = @import("std");
const csv = @import("csv.zig");
const fitbasetype = @import("fitbasetype.zig");
const FITBaseType = fitbasetype.FITBaseType;

pub const MappedType = struct {
    mapped_type: []const u8,
    mapped_to: FITBaseType,
};

pub const mapped_types = [_]MappedType{
    MappedType{ .mapped_type = "date_time", .mapped_to = .uint32 },
    MappedType{ .mapped_type = "local_date_time", .mapped_to = .uint32 },
    MappedType{ .mapped_type = "weight", .mapped_to = .uint16 },
};

pub const non_exhaustive_enums: []const []const u8 = &[_][]const u8{
    "device_index",
    "manufacturer",
    "message_index",
    "battery_status",
    "source_type",
};

pub const ValueDef = struct {
    name: []const u8,
    value: u64,
};

pub const TypeDef = struct {
    profile: *FITProfile,
    name: []const u8,
    tag_type: FITBaseType,
    values: std.ArrayList(ValueDef),
    non_exhaustive: bool = false,
    used: bool = false,

    pub fn init(profile: *FITProfile, name: []const u8, tag_type: FITBaseType) !TypeDef {
        const non_exhaustive = exhaustive: {
            inline for (non_exhaustive_enums) |non_exhaustive_enum| {
                if (std.mem.eql(u8, non_exhaustive_enum, name)) {
                    break :exhaustive true;
                }
            }
            break :exhaustive false;
        };
        return .{
            .profile = profile,
            .name = try profile.allocator.dupe(u8, name),
            .tag_type = tag_type,
            .values = std.ArrayList(ValueDef).init(profile.allocator),
            .non_exhaustive = non_exhaustive,
        };
    }

    pub fn append(def: *TypeDef, value_name: []const u8, value: u64) !void {
        try def.values.append(ValueDef{
            .name = try def.profile.allocator.dupe(u8, value_name),
            .value = value,
        });
    }

    pub fn print(this: *TypeDef, comptime fmt: []const u8, args: anytype) void {
        this.profile.print(fmt, args);
    }

    pub fn write(this: *TypeDef) void {
        this.print("pub const @\"{s}\" = enum({s}) {{\n", .{ this.name, FITBaseType.zig_type_name(this.tag_type) });
        for (this.values.items) |val| {
            this.print("    @\"{s}\" = {},\n", .{ val.name, val.value });
        }
        if (this.non_exhaustive) {
            this.print("    _,\n", .{});
        }
        this.print("}};\n\n", .{});
    }
};

pub const Component = struct {
    name: []const u8,
    scale: f32 = 1.0,
    offset: f32 = 0.0,
    bits: u8 = 16,
};

pub const FieldType = union(enum) {
    BaseType: FITBaseType,
    Enumeration: *TypeDef,
    Aggregate: struct {
        underlying: FITBaseType,
        components: std.ArrayList(Component),
    },

    pub fn init(profile: *FITProfile, field_type: []const u8, is_array: bool) !FieldType {
        var base_type: ?FITBaseType = FITBaseType.get(field_type) orelse blk: {
            for (mapped_types) |mapped_type| {
                if (std.mem.eql(u8, field_type, mapped_type.mapped_type)) {
                    break :blk mapped_type.mapped_to;
                }
            }
            break :blk null;
        };
        return if (base_type) |bt| base_blk: {
            if (bt == .byte and !is_array) {
                base_type = .uint8;
            }
            break :base_blk .{ .BaseType = bt };
        } else enum_blk: {
            const enumeration = profile.types.getPtr(field_type) orelse return error.UnknownEnumeration;
            enumeration.used = true;
            break :enum_blk .{ .Enumeration = enumeration };
        };
    }

    pub fn zig_type(this: FieldType) []const u8 {
        return switch (this) {
            .BaseType => |bt| bt.zig_type_name(),
            .Enumeration => |td| td.tag_type.zig_type_name(),
            .Aggregate => |agg| agg.underlying.zig_type_name(),
        };
    }
};

pub const FieldDef = struct {
    profile: *FITProfile,
    message_def: usize,
    field_num: u16,
    field_type: FieldType,
    name: []const u8,
    unit: []const u8,
    is_array: bool,
    scale: f32 = 0,
    offset: f32 = 0,

    pub fn init(profile: *FITProfile, message_def: usize, field_num: u16, name: []const u8, field_type: []const u8, is_array: bool, scale: f32, offset: f32) !FieldDef {
        return FieldDef{
            .profile = profile,
            .message_def = message_def,
            .field_num = field_num,
            .name = try profile.allocator.dupe(u8, name),
            .field_type = try FieldType.init(profile, field_type, is_array),
            .is_array = is_array,
            .scale = scale,
            .offset = offset,
            .unit = "",
        };
    }

    pub fn init_aggregate(profile: *FITProfile, message_def: usize, field_num: u16, name: []const u8, field_type: []const u8, is_array: bool) !FieldDef {
        return FieldDef{
            .profile = profile,
            .message_def = message_def,
            .field_num = field_num,
            .name = try profile.allocator.dupe(u8, name),
            .field_type = FieldType{
                .Aggregate = .{
                    .underlying = FITBaseType.get(field_type) orelse return error.InvalidType,
                    .components = std.ArrayList(Component).init(profile.allocator),
                },
            },
            .is_array = is_array,
            .scale = 1.0,
            .offset = 0.0,
            .unit = "",
        };
    }

    pub fn zig_type(this: FieldDef) []const u8 {
        return switch (this.field_type) {
            .BaseType => |bt| if (this.scale != 1.0 or this.offset != 0.0 or std.mem.eql(u8, this.unit, "semicircles")) "f32" else bt.zig_type_name(),
            .Enumeration => |enumeration| enumeration.name,
            .Aggregate => |agg| agg.underlying.zig_type_name(),
        };
    }

    pub fn fit_type(this: FieldDef) []const u8 {
        return this.field_type.zig_type();
    }

    fn print(this: FieldDef, comptime fmt: []const u8, args: anytype) void {
        this.profile.print(fmt, args);
    }

    fn assign(this: FieldDef) void {
        if (this.is_array) {
            this.print(
                \\fn set__{s}(this: *M, values: []{s}) void {{
                \\
            , .{ this.name, this.fit_type() });
            if (this.scale == 1.0 and this.offset == 0) {
                if (this.field_type == .Enumeration) {
                    this.print(
                        \\      this.@"{s}" = this.record.allocator.alloc({s}, values.len) catch unreachable;
                        \\      for (values, 0..) |v, ix| {{
                        \\         this.@"{s}"[ix] = @enumFromInt(v);
                        \\      }}
                        \\
                    , .{ this.name, this.field_type.Enumeration.name, this.name });
                } else {
                    this.print(
                        \\   this.@"{s}" = values;
                        \\
                    , .{this.name});
                }
            } else {
                this.print(
                    \\      this.@"{s}" = this.record.allocator.alloc(f32, values.len) catch unreachable;
                    \\      for (values, 0..) |v, ix| {{
                    \\         this.@"{s}"[ix] = @as(f32, @floatFromInt(v)) / {} - {};
                    \\      }}
                    \\
                , .{ this.name, this.name, this.scale, this.offset });
            }
            this.print("  }}\n\n", .{});
            return;
        }
        const ft = this.fit_type();
        this.print(
            \\fn set__{s}(this: *M, v: {s}) void {{
            \\
        , .{ this.name, ft });
        switch (this.field_type) {
            .BaseType => {
                if (this.scale == 1.0 and this.offset == 0) {
                    if (std.mem.eql(u8, this.unit, "semicircles")) {
                        this.print("  this.@\"{s}\" = @as(f32, @floatFromInt(v)) * (180.0 / @as(f32, @floatFromInt(@as(u32, @shlExact(1, 31)))));\n", .{this.name});
                    } else {
                        this.print("  this.@\"{s}\" = v;\n", .{this.name});
                    }
                } else {
                    this.print("  this.@\"{s}\" = @as(f32, @floatFromInt(v)) / {} - {};\n", .{ this.name, this.scale, this.offset });
                }
            },
            .Enumeration => {
                this.print("  this.@\"{s}\" = @enumFromInt(v);\n", .{this.name});
            },
            .Aggregate => |agg| {
                this.print("  {s} v_ = v;\n", .{if (agg.components.items.len > 1) "var" else "const"});
                for (agg.components.items, 0..) |c, ix| {
                    this.print("  this.set__{s}(@intCast(@as(u{}, @truncate(v_))));\n", .{ c.name, c.bits });
                    if (ix < agg.components.items.len - 1) {
                        this.print("  v_ = @shrExact(v_, {});\n", .{c.bits});
                    }
                }
            },
        }
        this.print("  }}\n\n", .{});
    }

    pub fn unmarshall(this: *FieldDef) void {
        const type_name = this.field_type.zig_type();
        this.print("{} => {{\n", .{this.field_num});
        defer this.print("}},\n", .{});
        if (this.is_array) {
            this.print(
                \\   ret.set__{s}(data_fld.get_array({s}, record_.allocator));
                \\
            , .{ this.name, type_name });
            return;
        }
        this.print(
            \\   ret.set__{s}(data_fld.get_value({s}));
            \\
        , .{ this.name, type_name });
    }
};

pub const MessageDef = struct {
    profile: *FITProfile,
    name: []const u8,
    mesg_num: u16,
    fields: std.ArrayList(FieldDef),

    pub fn init(profile: *FITProfile, name: []const u8) !MessageDef {
        return .{
            .profile = profile,
            .name = try profile.allocator.dupe(u8, name),
            .mesg_num = try profile.find_mesg_num(name),
            .fields = std.ArrayList(FieldDef).init(profile.allocator),
        };
    }

    fn print(this: MessageDef, comptime fmt: []const u8, args: anytype) void {
        this.profile.print(fmt, args);
    }

    pub fn write(this: *MessageDef) void {
        for (this.fields.items) |*fld| {
            switch (fld.field_type) {
                .Aggregate => |agg| {
                    comp_blk: for (agg.components.items) |*c| {
                        for (this.fields.items) |f| {
                            if (std.mem.eql(u8, f.name, c.name)) {
                                continue :comp_blk;
                            }
                        }
                        const t = if (c.bits <= 8)
                            FITBaseType.uint8
                        else if (c.bits <= 16)
                            FITBaseType.uint16
                        else if (c.bits <= 32)
                            FITBaseType.uint32
                        else
                            FITBaseType.uint64;
                        const scale = if (c.scale == 1.0) fld.scale else c.scale;
                        const offset = if (c.offset == 0.0) fld.offset else c.offset;
                        const synthetic_fld = FieldDef.init(this.profile, this.profile.messages.items.len, @truncate(500 + this.fields.items.len), c.name, switch (t) {
                            inline else => |t_| @tagName(t_),
                        }, false, scale, offset) catch |err|
                            std.debug.panic("Cannot add synthetic field to message: {any}", .{err});
                        this.fields.append(synthetic_fld) catch @panic("Memory allocation error");
                    }
                },
                else => {},
            }
        }
        this.print(
            \\pub const @"{s}" = struct {{
            \\    const M = @This();
            \\
            \\    record: DataRecord,
            \\
        , .{this.name});
        for (this.fields.items) |*fld| {
            this.print("    @\"{s}\": ", .{fld.name});
            if (fld.is_array) {
                this.print("[]", .{});
            }
            this.print("{s},\n", .{fld.zig_type()});
        }
        this.print("\n", .{});
        for (this.fields.items) |fld| {
            fld.assign();
        }
        this.print(
            \\
            \\    pub fn init(record_: DataRecord) !M {{
            \\        var ret: M = undefined;
            \\        ret.record = record_;
            \\
            \\        for (record_.fields) |data_fld| {{
            \\            switch (data_fld.field_definition_number) {{
            \\
        , .{});
        for (this.fields.items) |*fld_def| {
            fld_def.unmarshall();
        }
        this.print(
            \\           else => unreachable,
            \\            }}
            \\        }}
            \\        return ret;
            \\    }}
            \\
            \\}};
            \\
            \\
        , .{});
    }
};

const FITProfile = struct {
    allocator: std.mem.Allocator,
    types: std.StringArrayHashMap(TypeDef),
    messages: std.ArrayList(MessageDef),
    configured_messages: std.ArrayList([]const u8),
    out_filename: []const u8,
    out_file: std.fs.File,
    current_type: ?*TypeDef = null,
    current_message: ?usize = null,

    pub fn init(allocator: std.mem.Allocator) !FITProfile {
        const out_filename = "src/fittypes.zig";
        const out_file = try std.fs.cwd().createFile(out_filename, .{});

        var ret = FITProfile{
            .allocator = allocator,
            .types = std.StringArrayHashMap(TypeDef).init(allocator),
            .messages = std.ArrayList(MessageDef).init(allocator),
            .configured_messages = std.ArrayList([]const u8).init(allocator),
            .out_filename = out_filename,
            .out_file = out_file,
        };

        ret.print(
            \\const std = @import("std");
            \\const fitbasetype = @import("fitbasetype.zig");
            \\const FITBaseType = fitbasetype.FITBaseType;
            \\const fit = @import("fit.zig");
            \\const DataRecord = fit.DataRecord;
            \\const DataField = fit.DataField;
            \\const DataFieldValue = fit.DataFieldValue;
            \\
            \\
        , .{});
        return ret;
    }

    pub fn deinit(profile: *FITProfile) void {
        profile.out_file.close();

        var argv: [3][]const u8 = undefined;
        argv[0] = "zig";
        argv[1] = "fmt";
        argv[2] = "src/fittypes.zig";
        _ = std.process.Child.run(.{
            .allocator = profile.allocator,
            .argv = &argv,
        }) catch |err| std.debug.print("zig fmt failed: {any}\n", .{err});
        profile.types.deinit();
        profile.messages.deinit();
        profile.configured_messages.deinit();
    }

    pub fn print(this: FITProfile, comptime fmt: []const u8, args: anytype) void {
        this.out_file.writer().print(fmt, args) catch |err| {
            std.debug.print("Could not write to output file: {any}\n", .{err});
            std.debug.panic("Terminating", .{});
        };
    }

    pub fn write(this: FITProfile, bytes: []const u8) !usize {
        return try this.out_file.writer().write(bytes);
    }

    pub fn find_mesg_num(this: *FITProfile, name: []const u8) !u16 {
        const mesg_num: *TypeDef = this.types.getPtr("mesg_num") orelse unreachable;
        for (mesg_num.values.items) |value| {
            if (std.mem.eql(u8, name, value.name)) {
                return @as(u16, @truncate(value.value));
            }
        }
        return error.UnknownEnumeration;
    }

    pub fn emit_types(this: *FITProfile) void {
        for (this.types.values()) |*t| {
            if (t.used or std.mem.eql(u8, t.name, "mesg_num")) {
                t.write();
            }
        }
    }

    pub fn emit_messages(this: *FITProfile) void {
        for (this.messages.items) |*m| {
            m.write();
        }
    }

    pub fn emit_FITMessage(this: *FITProfile) void {
        this.print("pub const FITMessage = union(mesg_num) {{\n", .{});
        const mesg_num: *TypeDef = this.types.getPtr("mesg_num") orelse unreachable;

        for (mesg_num.values.items) |value| {
            const message = blk: {
                for (this.messages.items) |m| {
                    if (std.mem.eql(u8, m.name, value.name)) {
                        break :blk m.name;
                    }
                }
                break :blk "void";
            };
            this.print("    {s}: {s},\n", .{ value.name, message });
        }
        this.print("}};\n\n", .{});
    }
};

pub fn handle_configured_messages(this: *FITProfile, fields: csv.CSVRow) void {
    if (!fields.empty()) {
        this.configured_messages.append(this.allocator.dupe(u8, fields.fields[0]) catch unreachable) catch unreachable;
    }
}

pub fn handle_types(this: *FITProfile, fields: csv.CSVRow) void {
    if (fields.empty()) {
        std.debug.assert(this.types.contains("mesg_num"));
        return;
    }

    if (fields.has(0)) {
        const name = fields.fields[0];
        for (mapped_types) |mapped_type| {
            if (std.mem.eql(u8, name, mapped_type.mapped_type)) {
                this.current_type = null;
                return;
            }
        }
        const base_type = FITBaseType.get(fields.fields[1]);
        std.debug.assert(base_type != null);
        const res = this.types.getOrPut(this.allocator.dupe(u8, name) catch unreachable) catch unreachable;
        res.value_ptr.* = TypeDef.init(this, name, base_type.?) catch @panic("Memory Allocation error");
        this.current_type = res.value_ptr;
        std.debug.assert(this.current_type != null);
        switch (this.types.count()) {
            0 => unreachable,
            1 => std.debug.assert(std.mem.eql(u8, name, "file")),
            2 => std.debug.assert(std.mem.eql(u8, name, "mesg_num")),
            else => std.debug.assert(this.types.contains("mesg_num")),
        }
    } else if (fields.has(2)) {
        if (this.current_type) |ct| {
            const val = fields.to_int_must(u64, 3) catch std.debug.panic("Invalid enum value integer value", .{});
            ct.append(fields.fields[2], val) catch std.debug.panic("Memory Allocation error", .{});
        }
    }
}

pub fn new_message(this: *FITProfile, tokens: csv.CSVRow) !void {
    var configured = false;
    for (this.configured_messages.items) |m| {
        blk: {
            if (std.mem.eql(u8, m, tokens.fields[0])) {
                configured = true;
                break :blk;
            }
        }
    }
    if (this.configured_messages.items.len > 0 and !configured) {
        this.current_message = null;
        return;
    }
    try this.messages.append(try MessageDef.init(this, tokens.fields[0]));
    this.current_message = this.messages.items.len - 1;
}

pub fn new_field(this: *FITProfile, msg_ix: usize, tokens: csv.CSVRow) !void {
    std.debug.assert(tokens.has(2));
    const field_name = tokens.fields[2];
    const field_num = try tokens.to_int_must(u16, 1);
    var fld: FieldDef = undefined;
    if (!tokens.has(5)) {
        const scale = try tokens.to_float_with_default(f32, 6, 1.0);
        const offset = try tokens.to_float_with_default(f32, 7, 0.0);
        fld = try FieldDef.init(this, msg_ix, field_num, field_name, tokens.fields[3], tokens.has(4), scale, offset);
    } else {
        fld = try FieldDef.init_aggregate(this, msg_ix, field_num, field_name, tokens.fields[3], tokens.has(4));
        var components = &fld.field_type.Aggregate.components;
        const comps = if (tokens.fields[5][0] == '"') tokens.fields[5][1 .. tokens.fields[5].len - 1] else tokens.fields[5];
        var comp_it = std.mem.splitScalar(u8, comps, ',');
        while (comp_it.next()) |c| {
            try components.append(Component{
                .name = try this.allocator.dupe(u8, c),
            });
        }
        if (try tokens.to_float_array(f32, 6, this.allocator)) |scales| {
            defer this.allocator.free(scales);
            if (scales.len != components.items.len) {
                std.debug.panic("{s}.{s}: component scale {s}: '{s}'", .{ this.messages.items[msg_ix].name, field_name, if (scales.len < components.items.len) "underflow" else "overflow", tokens.fields[6] });
            }
            for (scales, 0..) |s, ix| {
                components.items[ix].scale = s;
            }
        }
        if (try tokens.to_float_array(f32, 7, this.allocator)) |offsets| {
            defer this.allocator.free(offsets);
            if (offsets.len != components.items.len) {
                std.debug.panic("{s}.{s}: component offset {s}: '{s}'", .{ this.messages.items[msg_ix].name, field_name, if (offsets.len < components.items.len) "underflow" else "overflow", tokens.fields[7] });
            }
            for (offsets, 0..) |o, ix| {
                components.items[ix].offset = o;
            }
        }
        if (try tokens.to_int_array(u8, 9, this.allocator)) |bits| {
            defer this.allocator.free(bits);
            if (bits.len != components.items.len) {
                std.debug.panic("{s}.{s}: component offset {s}: '{s}'", .{ this.messages.items[msg_ix].name, field_name, if (bits.len < components.items.len) "underflow" else "overflow", tokens.fields[9] });
            }
            for (bits, 0..) |b, ix| {
                components.items[ix].bits = b;
            }
        }
    }
    if (tokens.has(8)) {
        fld.unit = try this.allocator.dupe(u8, tokens.fields[8]);
    }
    try this.messages.items[msg_ix].fields.append(fld);
}

pub fn handle_messages(this: *FITProfile, tokens: csv.CSVRow) void {
    if (tokens.len() == 0) {
        return;
    }

    if (tokens.has(0)) {
        new_message(this, tokens) catch |err| std.debug.panic("new message '{s}': {any}", .{ tokens.fields[0], err });
    } else if (tokens.has(1)) {
        if (this.current_message) |msg_ix| {
            new_field(this, msg_ix, tokens) catch |err| std.debug.panic("new field '{s}'.'{s}': {any}", .{ this.messages.items[msg_ix].name, tokens.fields[1], err });
        }
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var profile = try FITProfile.init(allocator);
    defer profile.deinit();
    try csv.read_file(FITProfile, &profile, "messages.txt", handle_configured_messages);
    try csv.read_file(FITProfile, &profile, "profile-types.csv", handle_types);
    try csv.read_file(FITProfile, &profile, "profile-messages.csv", handle_messages);
    profile.emit_types();
    profile.emit_messages();
    profile.emit_FITMessage();
}
