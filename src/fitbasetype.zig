const std = @import("std");

pub const FITBaseType = enum(u8) {
    @"enum" = 0x00,
    uint8 = 0x02,
    uint8z = 0x0A,
    sint8 = 0x01,
    uint16 = 0x84,
    uint16z = 0x8B,
    sint16 = 0x83,
    uint32 = 0x86,
    uint32z = 0x8C,
    sint32 = 0x85,
    uint64 = 0x8F,
    uint64z = 0x90,
    sint64 = 0x8E,
    float32 = 0x88,
    float64 = 0x89,
    string = 0x07,
    byte = 0x0D,
    bool = 0x91,

    pub fn zig_type(comptime fit_type: FITBaseType) type {
        switch (fit_type) {
            inline .@"enum", .uint8, .uint8z => return u8,
            inline .sint8 => return i8,
            inline .uint16, .uint16z => return u16,
            inline .sint16 => return i16,
            inline .uint32, .uint32z => return u32,
            inline .sint32 => return i32,
            inline .uint64, .uint64z => return u64,
            inline .sint64 => return i64,
            inline .float32 => return f32,
            inline .float64 => return f64,
            inline .string, .byte => return []u8,
            inline .bool => return bool,
        }
    }

    pub fn default_value(fit_type: FITBaseType) []const u8 {
        switch (fit_type) {
            inline .float32, .float64 => return "0.0",
            inline .bool => return "false",
            inline .string, .byte => return "",
            inline else => "0",
        }
    }

    pub fn zig_type_name(fit_type: FITBaseType) []const u8 {
        switch (fit_type) {
            inline .@"enum", .uint8, .uint8z => return "u8",
            inline .sint8 => return "i8",
            inline .uint16, .uint16z => return "u16",
            inline .sint16 => return "i16",
            inline .uint32, .uint32z => return "u32",
            inline .sint32 => return "i32",
            inline .uint64, .uint64z => return "u64",
            inline .sint64 => return "i64",
            inline .float32 => return "f32",
            inline .float64 => return "f64",
            inline .string, .byte => return "[]const u8",
            inline .bool => return "bool",
        }
    }

    pub fn get(type_name: []const u8) ?FITBaseType {
        inline for (@typeInfo(FITBaseType).@"enum".fields) |fld| {
            if (std.mem.eql(u8, fld.name, type_name)) {
                return @enumFromInt(fld.value);
            }
        }
        return null;
    }

    pub fn from_int(base_type_int: u8) ?FITBaseType {
        inline for (@typeInfo(FITBaseType).@"enum".fields) |fld| {
            if (fld.value == base_type_int) {
                return @enumFromInt(fld.value);
            }
        }
        return null;
    }
};
