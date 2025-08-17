const std = @import("std");

fn parseInt(T: type) fn ([:0]const u8) std.fmt.ParseIntError!T {
    return struct {
        fn parse(in: [:0]const u8) std.fmt.ParseIntError!T {
            return std.fmt.parseInt(T, in, 10);
        }
    }.parse;
}

fn parseFloat(T: type) fn ([:0]const u8) std.fmt.ParseFloatError!T {
    return struct {
        fn parse(in: [:0]const u8) std.fmt.ParseFloatError!T {
            return std.fmt.parseFloat(T, in);
        }
    }.parse;
}

fn parseStr(in: [:0]const u8) ![]const u8 {
    return in;
}

pub const parse_fns = .{
    .u8 = parseInt(u8),
    .u16 = parseInt(u16),
    .u32 = parseInt(u32),
    .u64 = parseInt(u64),
    .u128 = parseInt(u128),
    .i8 = parseInt(i8),
    .i16 = parseInt(i16),
    .i32 = parseInt(i32),
    .i64 = parseInt(i64),
    .i128 = parseInt(i128),
    .f16 = parseFloat(f16),
    .f32 = parseFloat(f32),
    .f64 = parseFloat(f64),
    .f128 = parseFloat(f128),
    .str = parseStr,
};
