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
        type: []const u8,
        raw: []const u8,
        value: ?flagTypesFromParseFns(parsers) = null,
        pub fn get_type(self: @This()) []const u8 {
            return self.type;
        }
    };
}

fn Cmd(parsers: anytype) type {
    return struct {
        flags: []const Flag(parsers),
    };
}
//
// test "Flag" {
//     const F = flagTypesFromParseFns(parseFns);
//     const u: F = .{ .int = 7 };
//     const v2: F = .{ .ts = 64 };
//     const v3: F = .{ .ts2 = 64 };
//     try std.testing.expectEqual(@TypeOf(v3.ts2), @TypeOf(v2.ts));
//     try std.testing.expectEqual(v2, v3);
//     _ = u;
// }

const cmd = Cmd(parseFns){
    .flags = &.{
        .{ .type = "int", .raw = "240" },
        .{ .type = "ts", .raw = "240" },
    },
};

const Param = struct {
    type: [:0]const u8,
    name: [:0]const u8,
    default: ?[:0]const u8,
};

fn getParamType(param: Param) type {
    const ret_type = @typeInfo(@TypeOf(@field(parseFns, param.type))).@"fn".return_type.?;
    const ret_type_info = @typeInfo(ret_type);
    const ret_type_final = if (ret_type_info == .error_union)
        ret_type_info.error_union.payload
    else
        ret_type;
    return ret_type_final;
}

fn paramParseFnErrors(param: Param) bool {
    return @typeInfo(@typeInfo(@TypeOf(@field(parseFns, param.type))).@"fn".return_type.?) == .error_union;
}

fn ResultType(params: []const Param) type {
    var fields: [params.len]std.builtin.Type.StructField = undefined;
    for (0.., params) |i, param| {
        const pType = getParamType(param);
        // The parseFn can return an error and/or an optional. This statement could thus give us
        // !?T.
        const result_val = blk: {
            if (param.default) |d| {
                if (paramParseFnErrors(param)) {
                    break :blk @field(parseFns, param.type)(d) catch {
                        @compileError(std.fmt.comptimePrint(
                            "Failed to compute default value using parse function for '{s}' using default value string '{s}'\n",
                            .{ param.type, d },
                        ));
                    };
                } else {
                    break :blk @field(parseFns, param.type)(d);
                }
            } else {
                break :blk null;
            }
        };
        // const default_val = if (@typeInfo(@TypeOf(result_val)) == .error_union) result_val catch {} else result_val;
        fields[i] = .{
            .name = param.name,
            .type = @TypeOf(result_val),
            .default_value_ptr = @ptrCast(&result_val),
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

test "cmd" {
    const param = Param{ .type = "int", .name = "ts", .default = "19" };
    const param2 = Param{ .type = "str", .name = "ts2", .default = "19" };
    const Result = ResultType(&.{param, param2});
    const result = Result{};
    std.debug.print("{any}\n", .{result});
    @compileLog(@TypeOf(result.ts));
    @compileLog(@TypeOf(result.ts2));
    //
    // const value = @field(parseFns, param.type)("27");
    // std.debug.print("flag.value is {any}\n", .{value});
}

