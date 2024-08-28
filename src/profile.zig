const std = @import("std");
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

    var initialized = false;
    pub var types: std.StringHashMap(TypeDef) = undefined;
    pub var decl_order: std.ArrayList([]const u8) = undefined;

    pub fn init(profile: *FITProfile, name: []const u8, tag_type: FITBaseType) !*TypeDef {
        if (!TypeDef.initialized) {
            decl_order = std.ArrayList([]const u8).init(profile.allocator);
            types = std.StringHashMap(TypeDef).init(profile.allocator);
            initialized = true;
        }
        if (types.contains(name)) {
            unreachable;
        }
        const non_exhaustive = exhaustive: {
            inline for (non_exhaustive_enums) |non_exhaustive_enum| {
                if (std.mem.eql(u8, non_exhaustive_enum, name)) {
                    break :exhaustive true;
                }
            }
            break :exhaustive false;
        };
        const def = TypeDef{
            .profile = profile,
            .name = try profile.allocator.dupe(u8, name),
            .tag_type = tag_type,
            .values = std.ArrayList(ValueDef).init(profile.allocator),
            .non_exhaustive = non_exhaustive,
        };
        try types.put(def.name, def);
        try decl_order.append(def.name);
        return types.getPtr(def.name) orelse unreachable;
    }

    pub fn append(def: *TypeDef, value_name: []const u8, value: u64) !void {
        try def.values.append(ValueDef{
            .name = try def.profile.allocator.dupe(u8, value_name),
            .value = value,
        });
    }
};

pub fn find_mesg_num(name: []const u8) !u16 {
    const mesg_num: *TypeDef = TypeDef.types.getPtr("mesg_num") orelse unreachable;
    for (mesg_num.values.items) |value| {
        if (std.mem.eql(u8, name, value.name)) {
            return @as(u16, @truncate(value.value));
        }
    }
    return error.UnknownEnumeration;
}

pub const Component = struct {
    name: []const u8,
    scale: f32 = 1.0,
    offset: f32 = 0.0,
    bits: u8 = 16,
};

pub const FieldType = union(enum) {
    BaseType: void,
    Enumeration: []const u8,
    Aggregate: std.ArrayList(Component),
};

pub const FieldDef = struct {
    message_def: *MessageDef,
    field_num: u16,
    base_type: FITBaseType,
    field_type: FieldType,
    name: []const u8,
    unit: []const u8,
    is_array: bool,
    scale: f32 = 0,
    offset: f32 = 0,

    pub fn init(message_def: *MessageDef, field_num: u16, name: []const u8, field_type: []const u8, is_array: bool, scale: f32, offset: f32) !FieldDef {
        var base_type: ?FITBaseType = FITBaseType.get(field_type) orelse blk: {
            for (mapped_types) |mapped_type| {
                if (std.mem.eql(u8, field_type, mapped_type.mapped_type)) {
                    break :blk mapped_type.mapped_to;
                }
            }
            break :blk null;
        };
        if ((base_type == null) and !TypeDef.types.contains(field_type)) {
            std.debug.print("-> {s}\n", .{field_type});
            return error.UnknownEnumeration;
        }
        if (base_type) |bt| {
            if (bt == .byte and !is_array) {
                base_type = .uint8;
            }
        }
        return FieldDef{
            .message_def = message_def,
            .field_num = field_num,
            .name = try message_def.profile.allocator.dupe(u8, name),
            .base_type = if (base_type) |bt| bt else TypeDef.types.getPtr(field_type).?.tag_type,
            .field_type = if (base_type != null)
                .BaseType
            else
                FieldType{ .Enumeration = try message_def.profile.allocator.dupe(u8, field_type) },
            .is_array = is_array,
            .scale = scale,
            .offset = offset,
            .unit = "",
        };
    }

    pub fn init_aggregate(message_def: *MessageDef, field_num: u16, name: []const u8, field_type: []const u8, is_array: bool) !FieldDef {
        const base_type: ?FITBaseType = FITBaseType.get(field_type) orelse blk: {
            for (mapped_types) |mapped_type| {
                if (std.mem.eql(u8, field_type, mapped_type.mapped_type)) {
                    break :blk mapped_type.mapped_to;
                }
            }
            break :blk null;
        };
        if (base_type == null) {
            std.debug.print("-> {s}\n", .{field_type});
            return error.UnknownType;
        }
        return FieldDef{
            .message_def = message_def,
            .field_num = field_num,
            .name = try message_def.profile.allocator.dupe(u8, name),
            .base_type = base_type.?,
            .field_type = FieldType{ .Aggregate = std.ArrayList(Component).init(message_def.profile.allocator) },
            .is_array = is_array,
            .scale = 1.0,
            .offset = 0.0,
            .unit = "",
        };
    }

    fn print(this: *FieldDef, comptime fmt: []const u8, args: anytype) void {
        this.message_def.print(fmt, args);
    }

    fn assign(this: *FieldDef) void {
        if (this.is_array) {
            this.print(
                \\fn set__{s}(this: *M, values: []{s}) void {{
                \\
            , .{ this.name, this.base_type.zig_type_name() });
            if (this.scale == 1.0 and this.offset == 0) {
                if (this.field_type == .Enumeration) {
                    this.print(
                        \\      this.@"{s}" = this.record.allocator.alloc({s}, values.len) catch unreachable;
                        \\      for (values, 0..) |v, ix| {{
                        \\         this.@"{s}"[ix] = @enumFromInt(v);
                        \\      }}
                        \\
                    , .{ this.name, this.field_type.Enumeration, this.name });
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
        this.print(
            \\fn set__{s}(this: *M, v: {s}) void {{
            \\
        , .{ this.name, this.base_type.zig_type_name() });
        switch (this.base_type) {
            inline else => {
                if (this.scale == 1.0 and this.offset == 0) {
                    if (this.field_type == .Enumeration) {
                        this.print("  this.@\"{s}\" = @enumFromInt(v);\n", .{this.name});
                    } else {
                        if (std.mem.eql(u8, this.unit, "semicircles")) {
                            this.print("  this.@\"{s}\" = @as(f32, @floatFromInt(v)) * (180.0 / @as(f32, @floatFromInt(@as(u32, @shlExact(1, 31)))));\n", .{this.name});
                        } else {
                            this.print("  this.@\"{s}\" = v;\n", .{this.name});
                        }
                    }
                } else {
                    this.print("  this.@\"{s}\" = @as(f32, @floatFromInt(v)) / {} - {};\n", .{ this.name, this.scale, this.offset });
                }
                switch (this.field_type) {
                    .Aggregate => |components| {
                        this.print("  {s} v_ = v;\n", .{if (components.items.len > 1) "var" else "const"});
                        for (components.items, 0..) |c, ix| {
                            this.print("  this.set__{s}(@intCast(@as(u{}, @truncate(v_))));\n", .{ c.name, c.bits });
                            if (ix < this.field_type.Aggregate.items.len - 1) {
                                this.print("  v_ = @shrExact(v_, {});\n", .{c.bits});
                            }
                        }
                    },
                    else => {},
                }
            },
        }
        this.print("  }}\n\n", .{});
    }

    pub fn unmarshall(this: *FieldDef) void {
        const type_name = this.base_type.zig_type_name();
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
    writer: std.fs.File.Writer,
    name: []const u8,
    mesg_num: u16,
    fields: std.ArrayList(FieldDef),

    pub fn init(this: *MessageDef, profile: *FITProfile, name: []const u8) !void {
        this.profile = profile;
        this.name = try profile.allocator.dupe(u8, name);
        this.mesg_num = try find_mesg_num(name);
        this.fields = std.ArrayList(FieldDef).init(profile.allocator);
    }

    fn print(this: *MessageDef, comptime fmt: []const u8, args: anytype) void {
        this.profile.print(fmt, args);
    }

    pub fn write(this: *MessageDef, file: std.fs.File) void {
        for (this.fields.items) |fld| {
            switch (fld.field_type) {
                .Aggregate => |components| {
                    comp_blk: for (components.items) |*c| {
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
                        const synthetic_fld = FieldDef.init(this, @truncate(500 + this.fields.items.len), c.name, switch (t) {
                            inline else => |t_| @tagName(t_),
                        }, false, scale, offset) catch |err|
                            std.debug.panic("Cannot add synthetic field to message {s}: {any}", .{ this.name, err });
                        this.fields.append(synthetic_fld) catch @panic("Memory allocation error");
                    }
                },
                else => {},
            }
        }
        this.writer = file.writer();
        this.print(
            \\pub const @"{s}" = struct {{
            \\    const M = @This();
            \\
            \\    record: DataRecord,
            \\
        , .{this.name});
        for (this.fields.items) |fld| {
            this.print("    @\"{s}\": ", .{fld.name});
            if (fld.is_array) {
                this.print("[]", .{});
            }
            switch (fld.field_type) {
                .BaseType => {
                    const type_name = if (fld.scale != 1.0 or fld.offset != 0.0 or std.mem.eql(u8, fld.unit, "semicircles")) "f32" else fld.base_type.zig_type_name();
                    this.print("{s},\n", .{type_name});
                },
                .Enumeration => |enumeration| this.print("@\"{s}\",\n", .{enumeration}),
                .Aggregate => {
                    this.print("{s},\n", .{fld.base_type.zig_type_name()});
                },
            }
        }
        this.print("\n", .{});
        for (this.fields.items) |*fld| {
            fld.assign();
        }
        this.print(
            \\
            \\    pub fn init(record_: DataRecord) !M {{
            \\        var ret: M = undefined;
            \\        ret.record = record_;
            \\
        , .{});

        this.print(
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
            \\
        , .{});
        this.print(
            \\}};
            \\
            \\
        , .{});
    }
};

const CSVIterator = struct {
    const T = u8;
    buffer: []const T,
    index: ?usize = 0,

    const Self = @This();

    fn findComma(self: *Self) ?usize {
        if (self.index == null) {
            return null;
        }
        const i = self.index.?;
        var quote: ?u8 = null;
        var backslash = false;
        for (self.buffer[i..], i..) |c, j| {
            if (backslash) {
                backslash = false;
                continue;
            }
            if (quote) |q| {
                switch (c) {
                    '\\' => backslash = true,
                    else => if (c == q) {
                        quote = null;
                    },
                }
            } else {
                switch (c) {
                    '\'', '"' => quote = c,
                    ',' => return j,
                    else => {},
                }
            }
        }
        return null;
    }

    /// Returns a slice of the first field. This never fails.
    /// Call this only to get the first field and then use `next` to get all subsequent fields.
    pub fn first(self: *Self) []const T {
        std.debug.assert(self.index.? == 0);
        return self.next().?;
    }

    /// Returns a slice of the next field, or null if splitting is complete.
    pub fn next(self: *Self) ?[]const T {
        const start = self.index orelse return null;
        const end = if (self.findComma()) |pos| blk: {
            self.index = pos + 1;
            break :blk pos;
        } else blk: {
            self.index = null;
            break :blk self.buffer.len;
        };
        return self.buffer[start..end];
    }

    /// Returns a slice of the next field, or null if splitting is complete.
    /// This method does not alter self.index.
    pub fn peek(self: *Self) ?[]const T {
        const start = self.index orelse return null;
        const end = if (self.findComma()) |pos| pos else self.buffer.len;
        return self.buffer[start..end];
    }

    /// Returns a slice of the remaining bytes. Does not affect iterator state.
    pub fn rest(self: Self) []const T {
        const end = self.buffer.len;
        const start = self.index orelse end;
        return self.buffer[start..end];
    }

    /// Resets the iterator to the initial slice.
    pub fn reset(self: *Self) void {
        self.index = 0;
    }
};

pub fn split_csv(line: []const u8) CSVIterator {
    return CSVIterator{
        .buffer = line,
    };
}

const FITProfile = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(TypeDef),
    messages: std.ArrayList(MessageDef),
    out_file: std.fs.File,
    current_type: ?*TypeDef = null,
    current_message: ?*MessageDef = null,

    pub fn init(allocator: std.mem.Allocator) !FITProfile {
        const out_filename = "src/fittypes.zig";
        const out_file = try std.fs.cwd().createFile(out_filename, .{});

        var ret = FITProfile{
            .allocator = allocator,
            .types = std.ArrayList(TypeDef).init(allocator),
            .messages = std.ArrayList(MessageDef).init(allocator),
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
        }) catch std.debug.print("zig fmt failed.\n", .{});
    }

    fn read_file(this: *FITProfile, filename: [:0]const u8, handler: fn (this: *FITProfile, fields: [][]const u8) void) !void {
        var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
        const path = try std.fs.realpathZ(filename, &path_buffer);
        const file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();
        var r = std.io.bufferedReader(file.reader());
        var linebuf: [4096]u8 = undefined;
        while (true) {
            const line_maybe = try r.reader().readUntilDelimiterOrEof(&linebuf, '\n');
            if (line_maybe == null) {
                break;
            }
            var line = line_maybe orelse "";
            line = std.mem.trim(u8, line, " \n\r\t");
            if (line.len == 0) {
                continue;
            }
            if (std.mem.startsWith(u8, line, "#")) {
                continue;
            }
            var it = split_csv(line);
            var token = it.next();
            var tokens = std.ArrayList([]const u8).init(this.allocator);
            defer tokens.deinit();
            while (token != null) : (token = it.next()) {
                try tokens.append(token.?);
            }
            handler(this, tokens.items);
        }
        handler(this, &[_][]const u8{});
    }

    fn print(this: *FITProfile, comptime fmt: []const u8, args: anytype) void {
        this.out_file.writer().print(fmt, args) catch @panic("Could not write to output file");
    }
};

pub fn handle_types(this: *FITProfile, fields: [][]const u8) void {
    if (fields.len == 0) {
        if (this.current_type) |current_type| {
            if (current_type.non_exhaustive) {
                this.print("    _,\n", .{});
            }
            this.print("}};\n\n", .{});
        }
        return;
    }

    if (fields[0].len != 0) {
        if (this.current_type) |current_type| {
            if (current_type.non_exhaustive) {
                this.print("    _,\n", .{});
            }
            this.print("}};\n\n", .{});
        }
        for (mapped_types) |mapped_type| {
            if (std.mem.eql(u8, fields[0], mapped_type.mapped_type)) {
                this.current_type = null;
                return;
            }
        }
        const base_type = FITBaseType.get(fields[1]);
        std.debug.assert(base_type != null);
        this.print("pub const @\"{s}\" = enum({s}) {{\n", .{ fields[0], FITBaseType.zig_type_name(base_type.?) });
        this.current_type = TypeDef.init(this, fields[0], base_type.?) catch @panic("Memory Allocation error");
    } else if (this.current_type != null and fields[2].len > 0) {
        const valstr = std.mem.trim(u8, fields[3], " \t");
        const val = std.fmt.parseInt(u64, valstr, 0) catch @panic("Invalid enum value integer value");
        this.print("    @\"{s}\" = {},\n", .{ fields[2], val });
        this.current_type.?.append(fields[2], val) catch @panic("Memory Allocation error");
    }
}

pub fn handle_messages(this: *FITProfile, tokens: [][]const u8) void {
    if (tokens.len == 0) {
        if (this.current_message) |msg| {
            msg.write(this.out_file);
        }
        this.print("pub const FITMessage = union(mesg_num) {{\n", .{});
        const mesg_num: *TypeDef = TypeDef.types.getPtr("mesg_num") orelse unreachable;

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
        return;
    }

    if (tokens[0].len != 0) {
        if (this.current_message) |msg| {
            msg.write(this.out_file);
        }
        this.current_message = this.messages.addOne() catch @panic("Memory Allocation error");
        this.current_message.?.init(this, tokens[0]) catch @panic("Memory allocation error");
    } else if (tokens[1].len > 0) {
        var msg = this.current_message orelse return;
        const field_num = std.fmt.parseInt(u16, tokens[1], 0) catch std.debug.panic("{s}.{s}: Invalid field number", .{ msg.name, tokens[2] });
        var fld: FieldDef = undefined;
        if (tokens[5].len == 0) {
            const scale = if (tokens[6].len > 0) std.fmt.parseFloat(f32, tokens[6]) catch
                std.debug.panic("{s}.{s}: Invalid scale '{s}'", .{ msg.name, tokens[2], tokens[6] }) else 1.0;
            const offset = if (tokens[7].len > 0) std.fmt.parseFloat(f32, tokens[7]) catch
                std.debug.panic("{s}.{s}: Invalid offset '{s}'", .{ msg.name, tokens[2], tokens[7] }) else 0.0;
            fld = FieldDef.init(msg, field_num, tokens[2], tokens[3], tokens[4].len > 0, scale, offset) catch |err|
                std.debug.panic("Cannot add field {s} to message {s}: {any}", .{ tokens[2], msg.name, err });
            if (tokens[8].len > 0) {
                fld.unit = this.allocator.dupe(u8, tokens[8]) catch std.debug.panic("Out of Memory", .{});
            }
            msg.fields.append(fld) catch @panic("Memory allocation error");
        } else {
            fld = FieldDef.init_aggregate(msg, field_num, tokens[2], tokens[3], tokens[4].len > 0) catch |err|
                std.debug.panic("Cannot add aggregate field {s} to message {s}: {any}", .{ tokens[2], msg.name, err });
            var components = &fld.field_type.Aggregate;
            const comps = if (tokens[5][0] == '"') tokens[5][1 .. tokens[5].len - 1] else tokens[5];
            var comp_it = std.mem.splitScalar(u8, comps, ',');
            while (comp_it.next()) |c| {
                components.append(Component{
                    .name = this.allocator.dupe(u8, c) catch std.debug.panic("Memory allocation error", .{}),
                }) catch |err|
                    std.debug.panic("Could not add aggregate component: {any}", .{err});
            }
            if (tokens[6].len > 0) {
                const scales = if (tokens[6][0] == '"') tokens[6][1 .. tokens[6].len - 1] else tokens[6];
                var scale_it = std.mem.splitScalar(u8, scales, ',');
                var ix: u8 = 0;
                while (scale_it.next()) |s| {
                    if (ix >= components.items.len) {
                        std.debug.panic("{s}.{s}: Overflow component scale '{s}'", .{ msg.name, tokens[2], s });
                    }
                    components.items[ix].scale = std.fmt.parseFloat(f32, s) catch std.debug.panic("{s}.{s}: Invalid component scale '{s}'", .{ msg.name, tokens[2], s });
                    ix += 1;
                }
                if (ix < components.items.len) {
                    std.debug.panic("{s}.{s}: Component scale underflow", .{ msg.name, tokens[2] });
                }
            }
            if (tokens[7].len > 0) {
                const offsets = if (tokens[7][0] == '"') tokens[7][1 .. tokens[7].len - 1] else tokens[7];
                var offset_it = std.mem.splitScalar(u8, offsets, ',');
                var ix: u8 = 0;
                while (offset_it.next()) |o| {
                    if (ix >= fld.field_type.Aggregate.items.len) {
                        std.debug.panic("{s}.{s}: Overflow component offset '{s}'", .{ msg.name, tokens[2], o });
                    }
                    components.items[ix].offset = std.fmt.parseFloat(f32, o) catch std.debug.panic("{s}.{s}: Invalid component offset '{s}'", .{ msg.name, tokens[2], o });
                    ix += 1;
                }
                if (ix < components.items.len) {
                    std.debug.panic("{s}.{s}: Component offset underflow", .{ msg.name, tokens[2] });
                }
            }
            if (tokens[9].len > 0) {
                const bits = if (tokens[9][0] == '"') tokens[9][1 .. tokens[9].len - 1] else tokens[9];
                var bits_it = std.mem.splitScalar(u8, bits, ',');
                var ix: u8 = 0;
                while (bits_it.next()) |b| {
                    if (ix >= fld.field_type.Aggregate.items.len) {
                        std.debug.panic("{s}.{s}: Overflow component bit size '{s}'", .{ msg.name, tokens[2], b });
                    }
                    components.items[ix].bits = std.fmt.parseInt(u8, b, 0) catch std.debug.panic("{s}.{s}: Invalid component bit size '{s}'", .{ msg.name, tokens[2], b });
                    ix += 1;
                }
                if (ix < components.items.len) {
                    std.debug.panic("{s}.{s}: Component bit size underflow", .{ msg.name, tokens[2] });
                }
            }
            if (tokens[8].len > 0) {
                fld.unit = this.allocator.dupe(u8, tokens[8]) catch std.debug.panic("Out of Memory", .{});
            }
            msg.fields.append(fld) catch std.debug.panic("Memory allocation error", .{});
        }
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var profile = try FITProfile.init(allocator);
    defer profile.deinit();
    try profile.read_file("profile-types.csv", handle_types);
    try profile.read_file("profile-messages.csv", handle_messages);
}
