const std = @import("std");

fn parseu8(in: []const u8) !?u8 {
    return std.fmt.parseInt(u8, in, 10);
}

fn parseTs(in: []const u8) !i64 {
    return std.fmt.parseInt(i64, in, 10);
}

fn parseTs2(in: []const u8) !i64 {
    return std.fmt.parseInt(i64, in, 10);
}
fn parseStr(in: []const u8) []const u8 {
    return in;
}

var parseFns = .{ .int = parseu8, .ts = parseTs, .ts2 = parseTs2, .str = parseStr };

fn flagTypesFromParseFns(fns: anytype) type {
    const FnsType = @TypeOf(fns);
    const fns_type_info = @typeInfo(FnsType);
    if (fns_type_info != .@"struct") {
        @compileError("expected tuple or struct argument, found " ++ @typeName(FnsType));
    }

    const all_fields = fns_type_info.@"struct".fields;
    var fields: [all_fields.len]std.builtin.Type.UnionField = undefined;
    var union_tags: [all_fields.len]std.builtin.Type.EnumField = undefined;
    for (all_fields, 0..) |field, i| {
        const ret_type = @typeInfo(field.type).@"fn".return_type.?;
        const ret_type_info = @typeInfo(ret_type);
        const ret_type_final = if (ret_type_info == .error_union)
            ret_type_info.error_union.payload
        else
            ret_type;
        fields[i] = std.builtin.Type.UnionField{
            .name = field.name,
            .type = ret_type_final,
            .alignment = 8,
        };
        union_tags[i] = .{
            .name = field.name,
            .value = i,
        };
    }
    return @Type(.{ .@"union" = .{
        .layout = .auto,
        .tag_type = @Type(.{ .@"enum" = .{
            .tag_type = u8,
            .fields = &union_tags,
            .decls = &.{},
            .is_exhaustive = true,
        } }),
        .fields = fields[0..all_fields.len],
        .decls = &.{},
    } });
}

const FlagTypes = flagTypesFromParseFns(parseFns);

fn Flag(parsers: anytype) type {
    return struct {
        parser: []const u8,
        raw: []const u8,
        value: ?flagTypesFromParseFns(parsers) = null,
        pub fn get_type(self: @This()) []const u8 {
            return self.parser;
        }
    };
}

const Param = struct {
    parser: [:0]const u8,
    name: [:0]const u8,
    default: ?[:0]const u8 = null,
};

/// Get the type of a param based on the parse function's return type.
/// If the return type is an error union, this extracts the payload type from
/// the error union.
/// If the return type is an optional, this will still return an optional.
fn getParamTypeErrorStripped(param: Param) type {
    const ret_type = @typeInfo(@TypeOf(@field(parseFns, param.parser))).@"fn".return_type.?;
    const ret_type_info = @typeInfo(ret_type);
    const ret_type_final = if (ret_type_info == .error_union)
        ret_type_info.error_union.payload
    else
        ret_type;
    return ret_type_final;
}

/// Determines whether the parser for the parameter has an error union return type.
fn paramParseFnErrors(param: Param) bool {
    return @typeInfo(@typeInfo(@TypeOf(@field(parseFns, param.parser))).@"fn".return_type.?) == .error_union;
}

/// Convert all the parameters into a struct based on the function signatures of the parsers
/// and the default values of the params.
fn ResultType(params: []const Param) type {
    var fields: [params.len]std.builtin.Type.StructField = undefined;
    for (0.., params) |i, param| {
        const pType = getParamTypeErrorStripped(param);
        // We use the parseFn with the default value string to determine the default value.
        // If the caller provided an invalid default value causing the parseFn to error,
        // we have to provide a nice compileError, or else it's very hard to figure out
        // what went wrong.
        const default_val_ptr = blk: {
            if (param.default) |d| {
                if (paramParseFnErrors(param)) {
                    break :blk @as(*const anyopaque, @ptrCast(&@field(parseFns, param.parser)(d))) catch {
                        @compileError(std.fmt.comptimePrint(
                            "Failed to compute default value using parser '{s}' using default value string '{s}'\n",
                            .{ param.parser, d },
                        ));
                    };
                } else {
                    break :blk @as(*const anyopaque, @ptrCast(&@field(parseFns, param.parser)(d)));
                }
            } else {
                // If they didn't provide a default value, we give a null default_val pointer.
                break :blk null;
            }
        };
        fields[i] = .{
            .name = param.name,
            .type = pType,
            .default_value_ptr = default_val_ptr,
            .is_comptime = false,
            .alignment = @alignOf(pType),
        };
    }
    return @Type(.{ .@"struct" = .{
        .layout = .auto,
        .fields = fields[0..],
        .decls = &.{},
        .is_tuple = false,
    } });
}

const Cli = struct {
    params: []Param,
};
test "cmd" {
    const param = Param{
        .parser = "int",
        .name = "ts",
    };
    const param2 = Param{ .parser = "str", .name = "ts2", .default = "19" };
    const Result = ResultType(&.{ param, param2 });
    const result = Result{.ts = 19};
    std.debug.print("{any}\n", .{result});
}
