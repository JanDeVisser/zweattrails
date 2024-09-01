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

pub const SqlText = struct {
    db: Db,
    buf: std.ArrayList(u8),
    fields: usize = 0,

    pub fn init(db: Db) SqlText {
        return SqlText{
            .db = db,
            .buf = std.ArrayList(u8).init(db.allocator),
        };
    }

    pub fn deinit(this: *SqlText) void {
        this.buf.deinit();
    }

    pub fn reset(this: *SqlText) void {
        this.buf.clearRetainingCapacity();
        this.fields = 0;
    }

    fn append(this: *SqlText, comptime fmt: []const u8, args: anytype) void {
        var tmp: [1024]u8 = undefined;
        std.debug.assert(std.fmt.count(fmt, args) < 1024);
        this.buf.appendSlice(std.fmt.bufPrint(&tmp, fmt, args) catch unreachable) catch unreachable;
    }

    const Mode = enum {
        Insert,
        Update,
        Load,
    };

    inline fn sql_type(this: *SqlText, comptime T: type) []const u8 {
        _ = this;
        switch (@typeInfo(T)) {
            inline .int => {
                switch (@typeInfo(get_signed_type(T)).int.bits) {
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

    inline fn build_sql(this: *SqlText, mode: Mode, name: []const u8, comptime T: type, prefix: []const u8) !void {
        if (std.mem.eql(u8, name, "id")) {
            return;
        }
        switch (@typeInfo(T)) {
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
                    .Insert, .Load => this.append("{s}{s}", .{ prefix, name }),
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
                            .Insert, .Load => this.append("{s}{s}", .{ prefix, name }),
                        }
                    },
                    inline .One => {
                        switch (mode) {
                            .Update => this.append("{s}{s}_id = ${}", .{ prefix, name, this.fields }),
                            .Insert, .Load => this.append("{s}{s}_id", .{ prefix, name }),
                        }
                    },
                    else => unreachable,
                }
            },
            inline .@"struct" => |s| {
                if (!@hasDecl(T, "_transient") or T._transient) {
                    return;
                }
                if (@hasDecl(T, "build_sql")) {
                    T.build_sql(this, mode);
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

    inline fn bind_fld(this: *SqlText, stmt: *pg.Stmt, comptime ObjType: type, obj: *ObjType, name: []const u8, comptime FldType: type) !void {
        if (std.mem.eql(u8, name, "id")) {
            return;
        }
        switch (@typeInfo(FldType)) {
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
                    inline else => try stmt.bind(@field(obj.*, name).*.id),
                }
            },
            inline .@"struct" => |s| {
                if (!@hasDecl(FldType, "_transient") or FldType._transient) {
                    return;
                }
                if (@hasDecl(FldType, "bind")) {
                    @field(obj, name).bind(stmt);
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

    pub fn drop_table(this: *SqlText, comptime T: type) !void {
        this.reset();
        var conn = try this.db.pool.acquire();
        defer conn.deinit();
        const type_name = @typeName(T);
        var iter = std.mem.splitBackwardsScalar(u8, type_name, '.');
        const table_name = iter.first();

        this.append("drop table zweattrails.{s}", .{table_name});
        std.debug.print("drop_table:\n{s}\n", .{this.buf.items});
        _ = try conn.exec(this.buf.items, .{});
    }

    fn create_column(this: *SqlText, prefix: []const u8, comptime FldType: type, name: []const u8) !void {
        if (std.mem.eql(u8, name, "id")) {
            return;
        }
        switch (@typeInfo(FldType)) {
            inline .int, .float, .bool => {
                this.append(",\n  {s}{s} {s}", .{ prefix, name, this.sql_type(FldType) });
            },
            inline .pointer => |p| {
                switch (p.size) {
                    inline .Slice => {
                        std.debug.assert(p.child == u8);
                        this.append(",\n  {s}{s} text", .{ prefix, name });
                    },
                    inline .One => {
                        this.append(",\n  {s}{s}_id int references zweattrails.{s}", .{ prefix, name, name });
                    },
                    inline else => unreachable,
                }
            },
            inline .@"struct" => {
                std.debug.print("create_column {s}\n", .{name});
                if (!@hasDecl(FldType, "_transient") or FldType._transient) {
                    std.debug.print("transient\n", .{});
                    return;
                }
                var buf: [64]u8 = undefined;
                try this.create_columns(FldType, try std.fmt.bufPrint(&buf, "{s}{s}_", .{ prefix, name }));
            },
            inline else => unreachable,
        }
    }

    fn create_columns(this: *SqlText, comptime T: type, prefix: []const u8) !void {
        std.debug.assert(@typeInfo(T) == .@"struct");
        inline for (@typeInfo(T).@"struct".fields) |fld| {
            try this.create_column(prefix, fld.type, fld.name);
        }
    }

    pub fn create_table(this: *SqlText, comptime T: type) !void {
        const type_name = @typeName(T);
        if (this.db.types.contains(type_name)) {
            return;
        }
        this.reset();
        var iter = std.mem.splitBackwardsScalar(u8, type_name, '.');
        const table_name = iter.first();

        this.append(
            \\create table if not exists zweattrails.{s} (
            \\  id serial primary key
        , .{table_name});
        try this.create_columns(T, "");
        this.append("\n)", .{});
        _ = try this.db.exec(this.buf.items, .{});
        try this.db.types.put(type_name, true);
    }

    pub fn persist(this: *SqlText, comptime T: type, obj: *T) !void {
        std.debug.assert(@typeInfo(T) == .@"struct");
        std.debug.assert(!@hasDecl(T, "_transient") or !T._transient);
        try this.create_table(T);
        this.reset();
        const type_name = @typeName(T);
        var iter = std.mem.splitBackwardsScalar(u8, type_name, '.');
        const table_name = iter.first();

        if (obj.id) |_| {
            this.append("update zweattrails.{s} set\n  ", .{table_name});
            inline for (@typeInfo(T).@"struct".fields) |fld| {
                try this.build_sql(.Update, fld.name, fld.type, "");
            }
            this.append("\nwhere id = ${}\nreturning id", .{this.fields + 1});
        } else {
            this.append("insert into zweattrails.{s} (\n", .{table_name});
            inline for (@typeInfo(T).@"struct".fields) |fld| {
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
    }

    fn build_obj(this: *SqlText, comptime T: type, row: pg.Row) !T {
        var value: T = undefined;

        inline for (std.meta.fields(T)) |field| {
            const name = field.name;
            if (std.mem.eql(u8, name, "id")) {
                continue;
            }
            if (field.default_value) |dflt| {
                @field(value, name) = @as(*align(1) const field.type, @ptrCast(dflt)).*;
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
                    if (!@hasDecl(field.type, "_transient") or field.type._transient) {
                        continue;
                    }
                    @field(value, name) = try this.build_obj(field.type, row);
                },
                inline else => unreachable,
            }
        }
    }

    pub fn load(this: SqlText, comptime T: type, id: i32) !T {
        const objs = try this.query(T, "id", id);
        return switch (objs.items.len) {
            0 => blk: {
                objs.deinit();
                break :blk error.ObjNotFound;
            },
            1 => return objs,
            else => blk: {
                objs.deinit();
                break :blk error.ObjIdNotUnique;
            },
        };
    }

    pub fn query(this: SqlText, comptime T: type, column: []const u8, value: i32) !std.ArrayList(T) {
        std.debug.assert(@typeInfo(T) == .@"struct");
        std.debug.assert(!@hasDecl(T, "_transient") or !T._transient);
        try this.create_table(T);
        this.reset();
        const type_name = @typeName(T);
        var iter = std.mem.splitBackwardsScalar(u8, type_name, '.');
        const table_name = iter.first();

        this.append("select id\n  ", .{});
        inline for (@typeInfo(T).@"struct".fields) |fld| {
            try this.build_sql(.Load, fld.name, fld.type);
        }
        this.append(
            \\
            \\from zweattrails.{}
            \\where {} = $1
        , .{ table_name, column });

        var conn = try this.db.pool.acquire();
        defer conn.deinit();
        const result = try this.db.query(conn, this.buf.items, .{value});
        defer result.deinit();
        var ret = std.ArrayList(T).init(this.allocator);
        while (try result.next()) |row| {
            const id = row.get(i32, 0);
            var obj = try this.build_obj(T, row);
            obj.id = id;
            ret.append(obj);
        }
        return ret;
    }
};

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
