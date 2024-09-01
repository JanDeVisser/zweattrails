const std = @import("std");

pub const CSVRow = struct {
    fields: [][]const u8,

    pub fn init(fields: [][]const u8) CSVRow {
        return .{ .fields = fields };
    }

    pub fn len(this: CSVRow) usize {
        return this.fields.len;
    }

    pub fn empty(this: CSVRow) bool {
        return this.fields.len == 0;
    }

    pub fn has(this: CSVRow, fld: usize) bool {
        return (fld < this.fields.len) and (this.fields[fld].len > 0);
    }

    pub fn to_int(this: CSVRow, comptime T: type, fld: usize) !?T {
        if (!this.has(fld)) {
            return null;
        }
        return std.fmt.parseInt(T, std.mem.trim(u8, this.fields[fld], " \t"), 0) catch |err| return err;
    }

    pub fn to_int_must(this: CSVRow, comptime T: type, fld: usize) !T {
        if (!this.has(fld)) {
            return error.EmptyField;
        }
        return std.fmt.parseInt(T, std.mem.trim(u8, this.fields[fld], " \t"), 0) catch |err| return err;
    }

    pub fn to_int_with_default(this: CSVRow, comptime T: type, fld: usize, def: T) !T {
        if (!this.has(fld)) {
            return def;
        }
        return std.fmt.parseInt(T, std.mem.trim(u8, this.fields[fld], " \t"), 0) catch |err| return err;
    }

    pub fn to_int_array(this: CSVRow, comptime T: type, fld: usize, allocator: std.mem.Allocator) !?[]T {
        if (!this.has(fld)) {
            return null;
        }
        const values = if (this.fields[fld][0] == '"') this.fields[fld][1 .. this.fields[fld].len - 1] else this.fields[fld];
        var it = std.mem.splitScalar(u8, values, ',');
        var ix: u8 = 0;
        while (it.next()) |_| {
            ix += 1;
        }
        const ret = try allocator.alloc(T, ix);
        it.reset();
        ix = 0;
        while (it.next()) |s| {
            ret[ix] = try std.fmt.parseInt(T, s, 0);
            ix += 1;
        }
        return ret;
    }

    pub fn to_float(this: CSVRow, comptime T: type, fld: usize) !?T {
        if (!this.has(fld)) {
            return null;
        }
        return std.fmt.parseFloat(T, std.mem.trim(u8, this.fields[fld], " \t")) catch |err| return err;
    }

    pub fn to_float_must(this: CSVRow, comptime T: type, fld: usize) !T {
        if (!this.has(fld)) {
            return error.EmptyField;
        }
        return std.fmt.parseFloat(T, std.mem.trim(u8, this.fields[fld], " \t")) catch |err| return err;
    }

    pub fn to_float_with_default(this: CSVRow, comptime T: type, fld: usize, def: T) !T {
        if (!this.has(fld)) {
            return def;
        }
        return std.fmt.parseFloat(T, std.mem.trim(u8, this.fields[fld], " \t")) catch |err| return err;
    }

    pub fn to_float_array(this: CSVRow, comptime T: type, fld: usize, allocator: std.mem.Allocator) !?[]T {
        if (!this.has(fld)) {
            return null;
        }
        const values = if (this.fields[fld][0] == '"') this.fields[fld][1 .. this.fields[fld].len - 1] else this.fields[fld];
        var it = std.mem.splitScalar(u8, values, ',');
        var ix: u8 = 0;
        while (it.next()) |_| {
            ix += 1;
        }
        const ret = try allocator.alloc(T, ix);
        it.reset();
        ix = 0;
        while (it.next()) |s| {
            ret[ix] = try std.fmt.parseFloat(T, s);
            ix += 1;
        }
        return ret;
    }
};

pub const CSVIterator = struct {
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

pub fn read_file(comptime T: type, this: *T, filename: [:0]const u8, handler: fn (this: *T, row: CSVRow) void) !void {
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
        handler(this, CSVRow.init(tokens.items));
    }
    handler(this, CSVRow.init(&[_][]const u8{}));
}
