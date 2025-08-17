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

fn parseStr(in: [:0]const u8) []const u8 {
    return in;
}

pub const default_parsers = .{
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

fn ExtendDefaultParsersType(user_parsers: anytype) type {
    comptime var total_fields = std.meta.fields(@TypeOf(default_parsers)).len +
        std.meta.fields(@TypeOf(user_parsers)).len;
    for (std.meta.fields(@TypeOf(user_parsers))) |field| {
        if (@hasField(@TypeOf(default_parsers), field.name)) {
            // The user-provided functions overwrite the existing parsers.
            total_fields -= 1;
        }
    }

    var struct_fields: [total_fields]std.builtin.Type.StructField = undefined;
    var index = 0;

    // Add all the default parsers that don't exist in `user_parsers`
    for (std.meta.fieldNames(@TypeOf(default_parsers))) |field_name| {
        if (!@hasField(@TypeOf(user_parsers), field_name)) {
            const field_type = @TypeOf(@field(default_parsers, field_name));
            var default_val = @field(default_parsers, field_name);
            struct_fields[index] = .{
                .name = field_name,
                .type = field_type,
                .default_value_ptr = @as(?*anyopaque, @ptrCast(&default_val)),
                .is_comptime = false,
                .alignment = @alignOf(field_type),
            };
            index += 1;
        }
    }

    // Add all the `user_parsers`
    for (std.meta.fieldNames(@TypeOf(user_parsers))) |field_name| {
        const field_type = @TypeOf(@field(user_parsers, field_name));
        var default_val = @field(user_parsers, field_name);
        struct_fields[index] = .{
            .name = field_name,
            .type = field_type,
            .default_value_ptr = @as(?*anyopaque, @ptrCast(&default_val)),
            .is_comptime = false,
            .alignment = @alignOf(field_type),
        };
        index += 1;
    }

    return @Type(.{ .@"struct" = .{
        .layout = .auto,
        .fields = struct_fields[0..],
        .decls = &.{},
        .is_tuple = false,
    } });
}

/// Extend the default parsing functions with your own struct.
/// `user_parsers` will overwrite any default parsers that have a matching name.
pub fn extendDefaultParsers(user_parsers: anytype) ExtendDefaultParsersType(user_parsers) {
    return ExtendDefaultParsersType(user_parsers){};
}
