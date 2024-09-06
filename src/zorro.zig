const std = @import("std");
const libC = @cImport(@cInclude("time.h"));
const pg = @import("pg");

pub const log = std.log.scoped(.zweattrails);

pub fn get_signed_type(comptime T: type) type {
    return switch (@typeInfo(T)) {
        inline .int => |i| if (i.signedness == .signed) T else @Type(.{ .int = .{ .bits = 2 * i.bits, .signedness = .signed } }),
        inline else => unreachable,
    };
}

pub fn get_matching(comptime T: type) type {
    const i = @typeInfo(T).int;
    return @Type(.{ .int = .{ .bits = i.bits, .signedness = if (i.signedness == .signed) .unsigned else .signed } });
}

fn eq(comptime a: []const u8, comptime b: []const u8) bool {
    if (a.len != b.len) return false;
    for (0..a.len) |ix| {
        if (a[ix] != b[ix]) return false;
    }
    return true;
}

pub const QueryArg = union(enum) {
    string: []const u8,
    int32: i32,
    uint32: u32,
    float: f32,
};

pub fn Query(comptime T: type) type {
    return union(enum) {
        Filter: struct {
            tables: []const u8 = &[_]u8{},
            expr: []const u8,
            args: []QueryArg,
        },
        Join: struct {
            table: []const u8,
            join_on: struct {
                left: []const u8,
                right: []const u8,
            },
        },

        pub const Entity: type = T;
        pub const Z = Zorro(Entity);

        pub fn render(q: []Query, z: *Z) void {
            for (q) |pred| {
                switch (pred) {
                    .Filter => |f| {
                        for (f.tables) |t| {
                            z.append(", zweattrails.{s} {s} ", .{ t, t });
                        }
                    },
                    else => {},
                }
            }
            for (q) |pred| {
                switch (pred) {
                    .Join => |j| {
                        z.append("JOIN zweattrails.{s} {s} ON {s}.{s} = {s}.{s} ", .{ j.table, j.table, z.table, j.join_on.left, j.table, j.join_on.right });
                    },
                    else => {},
                }
            }
            var where = false;
            for (q) |pred| {
                switch (pred) {
                    .Filter => |f| {
                        if (!where) {
                            z.append("WHERE ", .{});
                            where = true;
                        }
                        // FIXME: fix up param number
                        z.append("{s} ", .{f.expr});
                    },
                    else => {},
                }
            }
        }
    };
}

pub inline fn tablename_for_type(comptime T: type) []const u8 {
    const type_name = @typeName(T);
    var iter = std.mem.splitBackwardsScalar(u8, type_name, '.');
    return iter.first();
}

pub fn is_entity(comptime T: type) bool {
    if (@typeInfo(T) != .@"struct") {
        return false;
    }
    return @hasDecl(T, "_transient") and !T._transient;
}

pub fn assert_entity(comptime Entity: type) !void {
    if (@typeInfo(Entity) != .@"struct") {
        return error.EntityNotAStruct;
    }
    if (!@hasDecl(Entity, "_transient") or !Entity._transient) {
        return error.TransientEntity;
    }
}

pub const ForeignKeyType = enum {
    OneToOne,
    OneToMany,
};

pub fn is_foreign_key(comptime fld: std.builtin.Type.StructField) ?ForeignKeyType {
    return switch (fld) {
        .pointer => |p| {
            switch (p.size) {
                .One => if (is_entity(p.child)) .OneToOne else null,
                else => null,
            }
        },
        .@"struct" => |s| blk: {
            if (is_entity(s.type)) {
                break :blk null;
            }
            if (!@hasDecl(s.type, "append")) {
                break :blk null;
            }
            if (std.meta.fieldIndex(s.type, "items")) |ix| {
                switch (std.meta.fields(s.type)[ix]) {
                    .pointer => |p| {
                        break :blk if (p.size == .Slice and is_entity(p.child)) .OneToMany else null;
                    },
                    else => break :blk null,
                }
            }
        },
        else => null,
    };
}

pub fn Zorro(comptime T: type) type {
    return struct {
        db: Db,
        buf: std.ArrayList(u8),
        table: [64]u8 = undefined,
        id: ?i32 = null,
        obj: ?T = null,
        fields: usize = 0,
        fks: std.StringHashMap(i32),

        pub const Entity: type = T;
        pub const Z = Zorro(Entity);
        pub const Q = Query(Entity);

        const State = enum {
            None,
            Loading,
            Storing,
            Deleting,
        };
        var states: std.AutoHashMap(i32, State) = std.AutoHashMap(i32, State).empty;

        pub fn init(db: Db) !Z {
            if (states == undefined) {
                states = std.AutoHashMap(i32, State).init(db.allocator);
            }
            try assert_entity(Entity);
            const type_name = @typeName(Entity);
            var iter = std.mem.splitBackwardsScalar(u8, type_name, '.');
            var ret = .Z{
                .db = db,
                .buf = std.ArrayList(u8).init(db.allocator),
                .fks = std.StringHashMap(i32).init(db.allocator),
            };
            _ = std.mem.copyForwards(u8, &ret.table, iter.first());
            try ret.create_table();
            ret.reset();
            return ret;
        }

        pub fn get(id: ?i32, db: Db) !Z {
            const ret = try Z.init(db);
            ret.id = id;
            if (ret.id) |i| {
                _ = try states.fetchPut(i, .None);
            }
            return ret;
        }

        pub fn deinit(this: *Z) void {
            this.buf.deinit();
            if (this.id) |id| {
                states.remove(id);
            }
        }

        pub fn reset(this: *Z) void {
            this.buf.clearRetainingCapacity();
            this.fks.clearRetainingCapacity();
            this.fields = 0;
        }

        fn append(this: *Z, comptime fmt: []const u8, args: anytype) void {
            var tmp: [1024]u8 = undefined;
            std.debug.assert(std.fmt.count(fmt, args) < 1024);
            this.buf.appendSlice(std.fmt.bufPrint(&tmp, fmt, args) catch unreachable) catch unreachable;
        }

        fn state(this: Z) State {
            if (this.id) |id| {
                if (states.get(id)) |s| {
                    return s;
                }
            }
            return false;
        }

        fn set_id(this: *Z, id: ?i32) State {
            this.id = id;
            if (this.id) |i| {
                if (try states.fetchPut(i, .None) catch unreachable) |kv| {
                    return kv.value;
                }
            }
            return .None;
        }

        fn set_state(this: Z, s: State) void {
            if (this.id) |id| {
                states.put(id, s) catch unreachable;
            }
        }

        const Mode = enum {
            Insert,
            Update,
            Load,
        };

        inline fn sql_type(this: *Z, comptime Field: type) []const u8 {
            _ = this;
            switch (@typeInfo(Field)) {
                inline .int => {
                    switch (@typeInfo(get_signed_type(Field)).int.bits) {
                        inline 8, 16 => return "smallint",
                        inline 32 => return "integer",
                        inline 64 => return "bigint",
                        inline else => unreachable,
                    }
                },
                inline .pointer => return "integer",
                inline .float => |f| {
                    switch (f.bits) {
                        32 => return "real",
                        64 => return "double precision",
                        else => unreachable,
                    }
                },
                inline .bool => return "boolean",
                inline else => unreachable,
            }
        }

        inline fn build_sql(this: *Z, mode: Mode, name: []const u8, comptime Field: type, prefix: []const u8) !void {
            if (std.mem.eql(u8, name, "id")) {
                return;
            }
            const fld = @typeInfo(Field);
            if (is_foreign_key(fld)) |fk_type| {
                switch (fk_type) {
                    .OneToOne => switch (mode) {
                        .Update => this.append("{s}{s} = ${}", .{ prefix, name, this.fields }),
                        .Insert, .Load => this.append("{s}{s}", .{ prefix, name }),
                    },
                    .OneToMany => {},
                }
                return;
            }
            switch (fld) {
                inline .int, .float, .bool => {
                    if (this.fields > 0) {
                        this.append(",\n  ", .{});
                    }
                    this.fields += 1;
                    switch (mode) {
                        .Update => {
                            this.append("{s}{s} = ${}", .{ prefix, name, this.fields });
                            this.fields += 1;
                        },
                        .Insert => this.append("{s}{s}", .{ prefix, name }),
                        .Load => this.append("{s}.{s}{s}", .{ this.table, prefix, name }),
                    }
                },
                inline .pointer => |p| {
                    if (this.fields > 0) {
                        this.append(",\n  ", .{});
                    }
                    this.fields += 1;
                    switch (p.size) {
                        inline .Slice => {
                            switch (mode) {
                                .Update => {
                                    this.append("{s}{s} = ${}", .{ prefix, name, this.fields });
                                    this.fields += 1;
                                },
                                .Insert => this.append("{s}{s}", .{ prefix, name }),
                                .Load => this.append("{s}.{s}{s}", .{ this.table, prefix, name }),
                            }
                        },
                        inline .One => {},
                    }
                },
                inline .@"struct" => |s| {
                    if (!is_entity(Field)) {
                        return;
                    }
                    var buf: [64]u8 = undefined;
                    const struct_prefix = try std.fmt.bufPrint(&buf, "{s}{s}_", .{ prefix, name });
                    inline for (s.fields) |f| {
                        try this.build_sql(mode, f.name, f.type, struct_prefix);
                    }
                },
                inline else => unreachable,
            }
        }

        inline fn bind_fld(this: *Z, stmt: *pg.Stmt, obj: *Entity, name: []const u8, comptime FldType: type) !void {
            if (std.mem.eql(u8, name, "id")) {
                return;
            }
            const fld = @typeInfo(FldType);
            if (is_foreign_key(fld)) |fk_type| {
                switch (fk_type) {
                    .OneToOne => try stmt.bind(@field(obj.*, name).*.id),
                    .OneToMany => {},
                }
                return;
            }
            switch (fld) {
                inline .int => |i| {
                    switch (i.signedness) {
                        .unsigned => {
                            const SignedIntType = get_signed_type(FldType);
                            const UnsignedIntType = get_matching(SignedIntType);
                            try stmt.bind(@as(SignedIntType, @bitCast(@as(UnsignedIntType, @field(obj, name)))));
                        },
                        .signed => {
                            try stmt.bind(@field(obj, name));
                        },
                    }
                },
                inline .float, .bool => {
                    try stmt.bind(@field(obj, name));
                },
                inline .pointer => |p| {
                    switch (p.size) {
                        inline .Slice => try stmt.bind(@field(obj, name)),
                        inline else => {},
                    }
                },
                inline .@"struct" => |s| {
                    if (!is_entity(FldType)) {
                        return;
                    }
                    const fld_val: *FldType = &@field(obj, name);
                    inline for (s.fields) |f| {
                        try this.bind_fld(stmt, FldType, fld_val, f.name, f.type);
                    }
                },
                inline else => unreachable,
            }
        }

        fn create_column(this: *Z, prefix: []const u8, comptime FldType: type, name: []const u8) !void {
            if (std.mem.eql(u8, name, "id")) {
                return;
            }
            const fld = @typeInfo(FldType);
            if (is_foreign_key(fld)) |fk_type| {
                switch (fk_type) {
                    .OneToOne => {
                        const child_type = fld.pointer.child;
                        this.append(",\n  {s}{s} int references zweattrails.{s}", .{ prefix, name, tablename_for_type(child_type) });
                    },
                    .OneToMany => {},
                }
                return;
            }
            switch (fld) {
                inline .int, .float, .bool => {
                    this.append(",\n  {s}{s} {s}", .{ prefix, name, this.sql_type(FldType) });
                },
                inline .pointer => |p| {
                    switch (p.size) {
                        inline .Slice => {
                            std.debug.assert(p.child == u8);
                            this.append(",\n  {s}{s} text", .{ prefix, name });
                        },
                        inline .One => {},
                        inline else => unreachable,
                    }
                },
                inline .@"struct" => {
                    if (!is_entity(FldType)) {
                        return;
                    }
                    var buf: [64]u8 = undefined;
                    try this.create_columns(FldType, try std.fmt.bufPrint(&buf, "{s}{s}_", .{ prefix, name }));
                },
                inline else => unreachable,
            }
        }

        fn create_columns(this: *Z, prefix: []const u8) !void {
            std.debug.assert(@typeInfo(Entity) == .@"struct");
            inline for (@typeInfo(Entity).@"struct".fields) |fld| {
                try this.create_column(prefix, fld.type, fld.name);
            }
        }

        pub fn create_table(this: *Z) !void {
            const type_name = @typeName(Entity);
            if (this.db.types.contains(type_name)) {
                return;
            }
            this.reset();

            this.append(
                \\create table if not exists zweattrails.{s} (
                \\  id serial primary key
            , .{this.table});
            try this.create_columns("");
            this.append("\n)", .{});
            _ = try this.db.exec(this.buf.items, .{});
            try this.db.types.put(type_name, true);
        }

        pub fn persist(this: *Z, obj: *Entity) !void {
            if (obj.id) |id| {
                if (this.set_id(id) == .Storing) {
                    return;
                }
                this.append("update zweattrails.{s} set\n  ", .{this.table});
                inline for (@typeInfo(Entity).@"struct".fields) |fld| {
                    try this.build_sql(.Update, fld.name, fld.type, "");
                }
                this.append("\nwhere id = ${}\nreturning id", .{this.fields + 1});
            } else {
                this.append("insert into zweattrails.{s} (\n", .{this.table});
                inline for (@typeInfo(Entity).@"struct".fields) |fld| {
                    try this.build_sql(.Insert, fld.name, fld.type, "");
                }
                this.append("\n) values (\n", .{});
                for (0..this.fields) |ix| {
                    if (ix > 0) {
                        this.append(", ", .{});
                    }
                    this.append("  ${}", .{ix + 1});
                }
                this.append(")\nreturning id", .{});
            }
            {
                const conn = try this.db.pool.acquire();
                defer this.db.pool.release(conn);
                var stmt = try this.db.prepare(conn, this.buf.items);
                errdefer stmt.deinit();
                inline for (@typeInfo(T).@"struct".fields) |fld| {
                    try this.bind_fld(&stmt, T, obj, fld.name, fld.type);
                }
                if (obj.id) |id| {
                    try stmt.bind(id);
                }
                const result = try stmt.execute();
                defer result.deinit();
                const cnt = blk: {
                    var c: usize = 0;
                    while (try result.next()) |row| {
                        obj.id = row.get(?i32, c);
                        c += 1;
                    }
                    break :blk c;
                };
                switch (cnt) {
                    0 => return error.ObjNotFound,
                    1 => {},
                    else => return error.ObjIdNotUnique,
                }
                Tx.add(Entity, obj);
                _ = this.set_id(obj.id);
            }

            this.set_state(.Storing);
            defer this.set_state(.None);
            for (std.meta.fields(Entity)) |fld| {
                if (is_foreign_key(fld)) |fk_type| {
                    switch (fk_type) {
                        .OneToOne => {
                            const child_type = fld.pointer.child;
                            const child = @field(obj, fld.name);
                            var child_z = Zorro(child_type).get(child.id, this.db) catch continue;
                            defer child_z.deinit();
                            child_z.persist(@field(obj, fld.name));
                        },
                        .OneToMany => {
                            if (std.meta.fieldIndex(fld.type, "items")) |fi| {
                                const items_fld = std.meta.fields(fld.type)[fi];
                                const items_finfo = @typeInfo(items_fld.type);
                                if (items_finfo != .pointer or items_finfo.pointer.size != .Slice) {
                                    continue;
                                }
                                if (@typeInfo(items_finfo.pointer.child) != .@"struct") {
                                    continue;
                                }
                                const children = @field(obj, fld.name).items;
                                for (children) |c| {
                                    var child_z = Zorro(items_finfo.pointer.child).get(c.id, this.db) catch continue;
                                    defer child_z.deinit();
                                    child_z.persist(fld.type, c);
                                }
                            }
                        },
                    }
                }
            }
        }

        fn build_obj(this: *Z, row: pg.Row) !Entity {
            var value: Entity = undefined;

            inline for (std.meta.fields(Entity)) |field| {
                const name = field.name;
                if (std.mem.eql(u8, name, "id")) {
                    continue;
                }
                if (field.default_value) |dflt| {
                    @field(value, name) = @as(*align(1) const field.type, @ptrCast(dflt)).*;
                }
                if (is_foreign_key(field)) |fk_type| {
                    switch (fk_type) {
                        .One => this.fks.put(field.name, row.get(i32, this.field)),
                        .Many => {},
                    }
                    continue;
                }
                switch (@typeInfo(field.type)) {
                    inline .int => |i| {
                        switch (i.signedness) {
                            .unsigned => {
                                const SignedIntType = get_signed_type(field.type);
                                const UnsignedIntType = get_matching(SignedIntType);
                                @field(value, name) = @as(field.type, @truncate(@as(UnsignedIntType, @bitCast(row.get(SignedIntType, this.field)))));
                            },
                            .signed => {
                                @field(value, name) = row.get(field.type, this.field);
                            },
                        }
                        this.fields += 1;
                    },
                    inline .float, .bool => {
                        @field(value, name) = row.get(field.type, this.field);
                        this.fields += 1;
                    },
                    inline .pointer => |p| {
                        switch (p.size) {
                            inline .Slice => @field(value, name) = this.allocator.dupe(p.child, row.get(field.type, this.field)),
                            inline else => {},
                        }
                        this.fields += 1;
                    },
                    inline .@"struct" => {
                        if (!is_entity(field.type)) {
                            continue;
                        }
                        @field(value, name) = try this.build_obj(field.type, row);
                    },
                    inline else => unreachable,
                }
            }
        }

        pub fn load(this: *Z, id: i32) !Entity {
            const buf: [128]u8 = undefined;
            const objs = try this.query(&[_]Q{
                .{
                    .Filter = .{
                        .expr = try std.fmt.bufPrint(&buf, "{s}.id = $1", .{this.table}),
                        .args = &[_]QueryArg{.{ .int = id }},
                    },
                },
            });
            defer {
                if (@hasDecl(Entity, "deinit")) {
                    for (objs) |*o| {
                        o.deinit();
                    }
                }
                objs.deinit();
            }
            return switch (objs.items.len) {
                0 => error.ObjNotFound,
                1 => objs.pop(),
                else => error.ObjIdNotUnique,
            };
        }

        pub fn load_single_child(this: *Z, q: []Q, fld: std.builtin.Type.StructField) !void {
            const fk_id = this.fks.get(fld.name) orelse unreachable;
        }

        pub fn query(this: *Z, q: []Q) !std.ArrayList(Entity) {
            try this.create_table(Entity);
            this.reset();

            this.append("SELECT {s}.id\n  ", .{this.table});
            inline for (@typeInfo(Entity).@"struct".fields) |fld| {
                try this.build_sql(.Load, fld.name, fld.type);
            }
            this.append("FROM zweattrails.{s} {s} ", .{ this.table, this.table });
            Query.render(q, this);

            var conn = try this.db.pool.acquire();
            defer conn.release();
            var results = std.ArrayList(Z).init(this.allocator);
            errdefer results.deinit();
            {
                var stmt = try this.db.prepare(conn, this.buf.items);
                errdefer stmt.deinit();
                for (q) |pred| {
                    switch (pred) {
                        .Filter => |f| {
                            for (f.args) |arg| {
                                switch (arg) {
                                    .int32 => |i| stmt.bind(i),
                                    .uint32 => |i| stmt.bind(i),
                                    .string => |s| stmt.bind(s),
                                    .float => |flt| stmt.bind(flt),
                                }
                            }
                        },
                        else => {},
                    }
                }
                const result = try stmt.execute();
                defer result.deinit();
                while (try result.next()) |row| {
                    const id = row.get(i32, 0);
                    var obj = try this.build_obj(T, row);
                    obj.id = id;
                    results.append(obj);
                }
            }

            for (std.meta.fields(Entity)) |fld| {
                const finfo = @typeInfo(fld.type);
                if (is_foreign_key(finfo)) |fk_type| {
                    switch (fk_type) {
                        .OneToOne => {
                            const fk_id = 0;
                        },
                        .OneToMany => {},
                    }
                }

                if (finfo != .@"struct") {
                    continue;
                }
                if (std.meta.fieldIndex(fld.type, "items")) |fi| {
                    const items_fld = std.meta.fields(fld.type)[fi];
                    const items_finfo = @typeInfo(items_fld.type);
                    if (items_finfo != .pointer) {
                        continue;
                    }
                    const child_type = @typeInfo(items_finfo.pointer.child);
                    switch (items_finfo.pointer.size) {
                        .One => {
                            if (!is_entity(items_finfo.pointer.child)) {
                                continue;
                            }
                        },
                        .Slice => {
                            if (!is_entity(items_finfo.pointer.child)) {
                                continue;
                            }
                            const fk_name: [128]u8 = undefined;
                            try std.fmt.bufPrint(&fk_name, "{}_id", .{this.table});
                            for (child_type.@"struct".fields) |child_fld| {
                                if (std.mem.eql(u8, this.table, child_fld.name)) {
                                    const parent_ptr_type = @typeInfo(child_fld.type);
                                    if (parent_ptr_type == .pointer and parent_ptr_type.pointer.child == Entity and parent_ptr_type.pointer.size == .One) {
                                        const child_q = try this.db.allocator.alloc(Q, q.len + 1);
                                        defer this.db.allocator.free(child_q);
                                        for (q, 0..) |pred, ix| {
                                            child_q[ix + 1] = pred;
                                        }
                                        child_q[0] = .{
                                            .Join{
                                                .table = this.table,
                                                .join_on = .{
                                                    .left = fk_name,
                                                    .right = "id",
                                                },
                                            },
                                        };
                                        var child_z = try Zorro(items_finfo.pointer.child).init(this.db);
                                        defer child_z.deinit();
                                        const children = try child_z.query(&child_q);
                                        for (children) |child| {
                                            const fk = @field(child, this.table);
                                            for (ret) |*r| {
                                                if (r.id == fk) {
                                                    @field(r, finfo.name).append(child);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        },
                    }
                }
                return ret;
            }
        }
    };
}

pub const Db = struct {
    allocator: std.mem.Allocator,
    pool: *pg.Pool,
    err: ?[]const u8 = null,
    types: std.StringHashMap(bool),

    pub fn init(allocator: std.mem.Allocator, drop_everything: bool) !Db {
        const pool = try pg.Pool.init(allocator, .{ .size = 5, .connect = .{
            .port = 5432,
            .host = "127.0.0.1",
        }, .auth = .{
            .username = "zweattrails",
            .database = "zweattrails",
            .timeout = 10_000,
        } });
        var ret = Db{
            .allocator = allocator,
            .pool = pool,
            .types = std.StringHashMap(bool).init(allocator),
        };
        if (drop_everything) {
            _ = ret.exec("drop schema if exists zweattrails cascade", .{}) catch {
                return error.CouldNotDropSchema;
            };
        }
        _ = ret.exec("create schema if not exists zweattrails", .{}) catch {
            return error.CouldNotCreateSchema;
        };
        _ = ret.exec("set search_path to zweattrails", .{}) catch {
            return error.CouldNotSetSearchPath;
        };
        return ret;
    }

    pub fn deinit(this: *Db) void {
        _ = this.pool.deinit();
        this.types.deinit();
    }

    pub fn exec(this: *Db, sql: []const u8, values: anytype) !?i64 {
        var conn = try this.pool.acquire();
        defer this.pool.release(conn);
        std.log.debug("db.exec: {s}", .{sql});
        return conn.exec(sql, values) catch |e| {
            if (conn.err) |pge| {
                std.log.err("PG error: {s}", .{pge.message});
                this.err = pge.message;
            }
            return e;
        };
    }

    pub fn query(this: *Db, conn: *pg.Conn, sql: []const u8, values: anytype) !*pg.Result {
        std.log.debug("db.query: {s}", .{sql});
        return conn.query(sql, values) catch |e| {
            if (conn.err) |pge| {
                std.log.err("PG error: {s}", .{pge.message});
                this.err = pge.message;
            }
            return e;
        };
    }

    pub fn prepare(this: *Db, conn: *pg.Conn, sql: []const u8) !pg.Stmt {
        std.log.debug("db.prepare: {s}", .{sql});
        var stmt = try pg.Stmt.init(conn, .{});
        errdefer stmt.deinit();
        stmt.prepare(sql) catch |e| {
            if (conn.err) |pge| {
                std.log.err("PG error: {s}", .{pge.message});
                this.err = pge.message;
            }
            return e;
        };
        return stmt;
    }
};
