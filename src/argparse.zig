const std = @import("std");
const Allocator = std.mem.Allocator;

fn parseu8(in: [:0]const u8) !u8 {
    return std.fmt.parseInt(u8, in, 10);
}

fn parseu16(in: [:0]const u8) u16 {
    return std.fmt.parseInt(u16, in, 10);
}
fn parseTs(in: [:0]const u8) !i64 {
    return std.fmt.parseInt(i64, in, 10);
}

fn parseTs2(in: [:0]const u8) !i64 {
    return std.fmt.parseInt(i64, in, 10);
}
fn parseStr(in: [:0]const u8) []const u8 {
    return in;
}

var parseFns = .{ .int = parseu8, .ts = parseTs, .ts2 = parseTs2, .str = parseStr, .int16 = parseu16 };

/// Get the type of a param based on the parse function's return type.
/// If the return type is an error union, this extracts the payload type from
/// the error union.
/// If the return type is an optional, this will still return an optional.
fn getParamTypeErrorStripped(parser_name: [:0]const u8) type {
    const ret_type = @typeInfo(@TypeOf(@field(parseFns, parser_name))).@"fn".return_type.?;
    const ret_type_info = @typeInfo(ret_type);
    const ret_type_final = if (ret_type_info == .error_union)
        ret_type_info.error_union.payload
    else
        ret_type;
    return ret_type_final;
}

/// Determines whether the parser for the parameter has an error union return type.
fn parseFnErrors(parser_name: [:0]const u8) bool {
    return @typeInfo(@typeInfo(@TypeOf(@field(parseFns, parser_name))).@"fn".return_type.?) == .error_union;
}

fn array_contains_string(haystack: [1000][:0]const u8, needle: [:0]const u8, haystack_len: usize) bool {
    for (haystack[0..haystack_len]) |h| {
        if (std.mem.eql(u8, h, needle)) {
            return true;
        }
    }
    return false;
}

fn validate_command_structure(cmd: Command) void {
    const params = cmd.params orelse &.{};
    const flags = cmd.flags orelse &.{};
    const positionals = cmd.positionals orelse &.{};
    const subcommands = cmd.subcommands orelse &.{};

    var all_the_names: [1000][:0]const u8 = undefined;
    var all_the_names_len: usize = 0;
    var struct_names: [1000][:0]const u8 = undefined;
    var struct_names_len: usize = 0;

    for (params) |param| {
        if (param.long == null and param.short == null) {
            @compileError("Either a long or short specifier for the parameter must be specified");
        }
        if (param.long) |long| {
            if (array_contains_string(all_the_names, long, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{long}));
            }
            all_the_names[all_the_names_len] = long;
            all_the_names_len += 1;
        }
        if (param.short) |short| {
            if (array_contains_string(all_the_names, short, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{short}));
            }
            all_the_names[all_the_names_len] = short;
            all_the_names_len += 1;
        }
        if (array_contains_string(struct_names, param.name, struct_names_len)) {
            @compileError(std.fmt.comptimePrint("Found duplicate struct field name: {s}\n", .{param.name}));
        }
        struct_names[struct_names_len] = param.name;
        struct_names_len += 1;
    }

    for (flags) |flag| {
        if (flag.long == null and flag.short == null) {
            @compileError("Either a long or short specifier for the parameter must be specified");
        }
        if (flag.long) |long| {
            if (array_contains_string(all_the_names, long, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{long}));
            }
            all_the_names[all_the_names_len] = long;
            all_the_names_len += 1;
        }
        if (flag.short) |short| {
            if (array_contains_string(all_the_names, short, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{short}));
            }
            all_the_names[all_the_names_len] = short;
            all_the_names_len += 1;
        }
        if (array_contains_string(struct_names, flag.name, struct_names_len)) {
            @compileError(std.fmt.comptimePrint("Found duplicate struct field name: {s}\n", .{flag.name}));
        }
        struct_names[struct_names_len] = flag.name;
        struct_names_len += 1;
    }

    var num_positionals_many = 0;
    for (0.., positionals) |i, positional| {
        if (positional.value_count == .many) {
            num_positionals_many += 1;
            if (i != 0 and i != (positionals.len - 1)) {
                @compileError("Cannot have a positional that takes many values unless it's the first or last positional.");
            }
            if (num_positionals_many > 1) {
                @compileError("Cannot have multiple positionals that take more than one value");
            }
        }
        if (array_contains_string(struct_names, positional.name, struct_names_len)) {
            @compileError(std.fmt.comptimePrint("Found duplicate struct field name: {s}\n", .{positional.name}));
        }
        struct_names[struct_names_len] = positional.name;
        struct_names_len += 1;
    }

    for (subcommands) |subcommand| {
        if (array_contains_string(struct_names, subcommand.name, struct_names_len)) {
            @compileError(std.fmt.comptimePrint("Found duplicate struct field name: {s}\n", .{subcommand.name}));
        }
        struct_names[struct_names_len] = subcommand.name;
        struct_names_len += 1;
    }

    for (subcommands) |subcommand| {
        validate_command_structure(subcommand);
    }
}

fn param_or_positional_field(
    parser: [:0]const u8,
    name: [:0]const u8,
    value_count: ValueCount,
    required: bool,
    default: ?[:0]const u8,
) std.builtin.Type.StructField {
    var p_type = getParamTypeErrorStripped(parser);
    p_type = if (value_count == .one) p_type else []const p_type;
    p_type = if (required) p_type else ?p_type;

    // We use the parseFn with the default value string to determine the default value.
    // If the caller provided an invalid default value causing the parseFn to error,
    // we have to provide a nice compileError, or else it's very hard to figure out
    // what went wrong.
    const default_val_ptr = outer: {
        if (default) |d| {
            const default_val = blk: {
                if (parseFnErrors(parser)) {
                    break :blk @field(parseFns, parser)(d) catch {
                        @compileError(std.fmt.comptimePrint(
                            "Failed to compute default value using parser '{s}' using default value string '{s}'\n",
                            .{ parser, d },
                        ));
                    };
                } else {
                    break :blk @field(parseFns, parser)(d);
                }
            };
            if (value_count == .many) {
                const default_val_sl: p_type = &.{default_val};
                break :outer @as(*const anyopaque, @ptrCast(&default_val_sl));
            } else {
                break :outer @as(*const anyopaque, @ptrCast(&default_val));
            }
        } else {
            if (required) {
                // If they didn't provide a default value, we give a null default_val pointer.
                break :outer null;
            } else {
                const default_val: p_type = null;
                break :outer @as(*const anyopaque, @ptrCast(&default_val));
            }
        }
    };

    return .{
        .name = name,
        .type = p_type,
        .default_value_ptr = default_val_ptr,
        .is_comptime = false,
        .alignment = @alignOf(p_type),
    };
}

/// Convert all the parameters into a struct based on the function signatures of the parsers
/// and the default values of the params.
fn ResultType(cmd: Command) type {
    validate_command_structure(cmd);
    const params = cmd.params orelse &.{};
    const flags = cmd.flags orelse &.{};
    const positionals = cmd.positionals orelse &.{};
    // They can theoretically pass an empty slice, in which case we want 0 subcommands_len
    const subcommands_len = if (cmd.subcommands) |sub| @min(1, sub.len) else 0;
    const subcommands = cmd.subcommands orelse &.{};

    var fields: [params.len + flags.len + positionals.len + subcommands_len]std.builtin.Type.StructField = undefined;
    for (0.., params) |i, param| {
        fields[i] = param_or_positional_field(
            param.parser,
            param.name,
            param.value_count,
            param.required,
            param.default,
        );
    }

    for (params.len.., flags) |i, flag| {
        fields[i] = .{
            .name = flag.name,
            .type = bool,
            .default_value_ptr = &false,
            .is_comptime = false,
            .alignment = @alignOf(bool),
        };
    }

    for (params.len + flags.len.., positionals) |i, positional| {
        fields[i] = param_or_positional_field(
            positional.parser,
            positional.name,
            positional.value_count,
            positional.required,
            positional.default,
        );
    }

    if (subcommands_len > 0) {
        var subcommands_enum_fields: [subcommands_len]std.builtin.Type.EnumField = undefined;
        var subcommands_union_fields: [subcommands_len]std.builtin.Type.UnionField = undefined;
        for (0.., subcommands) |i, subcommand| {
            subcommands_enum_fields[i] = .{
                .name = subcommand.name,
                .value = i,
            };
            const sub_type = ResultType(subcommand);
            subcommands_union_fields[i] = .{
                .name = subcommand.name,
                .type = sub_type,
                .alignment = @alignOf(sub_type),
            };
        }

        const subcommands_enum = @Type(.{ .@"enum" = .{
            .tag_type = std.math.IntFittingRange(0, subcommands_len - 1),
            .fields = subcommands_enum_fields[0..],
            .decls = &.{},
            .is_exhaustive = true,
        } });

        const subcommands_tagged_union = @Type(.{ .@"union" = .{
            .layout = .auto,
            .tag_type = subcommands_enum,
            .decls = &.{},
            .fields = subcommands_union_fields[0..],
        } });

        fields[fields.len - 1] = .{
            .name = "subcommand",
            .type = subcommands_tagged_union,
            .default_value_ptr = null,
            .is_comptime = false,
            .alignment = @alignOf(subcommands_tagged_union),
        };
    }

    return @Type(.{ .@"struct" = .{
        .layout = .auto,
        .fields = fields[0..],
        .decls = &.{},
        .is_tuple = false,
    } });
}

const ValueCount = enum {
    one,
    many,
};

const Param = struct {
    /// The name of the param in the result struct
    name: [:0]const u8,
    /// The parser used to convert the argument string to the correct type
    parser: [:0]const u8,
    short: ?[:0]const u8 = null,
    long: ?[:0]const u8 = null,
    default: ?[:0]const u8 = null,
    description: ?[:0]const u8 = null,
    value_count: ValueCount = .one,
    required: bool = true,
};

const Flag = struct {
    /// The name of the flag in the result struct
    name: [:0]const u8,
    short: ?[:0]const u8 = null,
    long: ?[:0]const u8 = null,
    description: ?[:0]const u8 = null,
};

const Positional = struct {
    /// The name of the positional in the result struct
    name: [:0]const u8,
    /// The parser used to convert the argument string to the correct type
    parser: [:0]const u8,
    default: ?[:0]const u8 = null,
    description: ?[:0]const u8 = null,
    value_count: ValueCount = .one,
    required: bool = true,
    // TODO: default values? Not sure if that's a good idea...
};

const Command = struct {
    /// The name of the command or subcommand
    name: [:0]const u8,
    /// Parameters that take a value, which is computed using a parser
    params: ?[]const Param = null,
    /// Boolean flags. All default to false, and are true if the flag is specified on the
    /// command line
    flags: ?[]const Flag = null,
    /// Positional arguments. You can have more than one positional argument, but only
    /// the first or the last positional argument is allowed to take many values.
    positionals: ?[]const Positional = null,
    /// Subcommands, which must come after any params/flags/positionals in the parent command.
    /// In the resulting struct, the subcommands are a tagged union with the names
    /// corresponding to the names of the subcommands.
    subcommands: ?[]const Command = null,
    description: ?[:0]const u8 = null,
};

fn validate_parsers(parsers: anytype) void {
    const parsers_info = @typeInfo(@TypeOf(parsers));
    if (parsers_info != .@"struct") {
        @compileError("parsers must be a tuple that maps parser strings to parser functions");
    }
    const parser_fields = parsers_info.@"struct".fields;
    comptime var i = 0;
    inline while (i < parser_fields.len) : (i += 1) {
        const field = parser_fields[i];
        const field_info = @typeInfo(field.type);
        if (field_info != .@"fn") {
            @compileError("parsers must be a tuple that maps parser strings to parser functions");
        }
        const fn_params = field_info.@"fn".params;
        if (fn_params.len != 1) {
            @compileError(std.fmt.comptimePrint(
                "error for parser {s}: parser must be a function that takes [:0]const u8",
                .{field.name},
            ));
        }
        if (fn_params[0].type != [:0]const u8) {
            @compileError(std.fmt.comptimePrint(
                "error for parser {s}: parser must be a function that takes [:0]const u8. Found param type: {any}\n",
                .{ field.name, fn_params[0].type },
            ));
        }
        const ret_type = getParamTypeErrorStripped(field.name);
        if (@typeInfo(ret_type) == .optional) {
            @compileError(std.fmt.comptimePrint(
                "error: parser '{s}' cannot return an optional type. Found return type {any}\n",
                .{ field.name, ret_type },
            ));
        }
    }
}

fn parse(comptime cmd: Command, parsers: anytype, alloc: Allocator) !ResultType(cmd) {
    validate_parsers(parsers);
    // @compileLog(parsers_t);
    const result: ResultType(cmd) = undefined;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    return result;
}

test "cmd" {
    const param1 = Param{ .parser = "str", .name = "ts", .long = "--timestamp", .value_count = .many, .default = "hello :)", .required = false };
    const param2 = Param{ .parser = "int", .name = "ts2", .short = "-t", .default = "19" };
    const param3 = Param{
        .parser = "int16",
        .name = "ts3",
        .short = "-t2",
    };
    const param4 = Param{ .parser = "str", .name = "ts4", .long = "--timestamp", .value_count = .one, .default = "howdy :D" };
    const flag1 = Flag{ .name = "f", .short = "-f" };
    const pos1 = Positional{ .name = "pos", .parser = "int", .required = false };
    const sub = Command{
        .name = "main",
        .params = &.{param4},
        .positionals = &.{pos1},
        .subcommands = &.{},
        .flags = &.{flag1},
    };
    const cmd = Command{
        .name = "main",
        .params = &.{ param1, param2, param3 }, //, param3 },
        .flags = &.{flag1},
        .positionals = &.{pos1},
        .subcommands = &.{sub},
    };
    const Result = ResultType(cmd);
    const result: Result = .{ .subcommand = .{ .main = .{ .pos = 1, .f = true } }, .ts3 = 19 };
    // result.pos = 8;
    // result.subcommand = .{ .main = .{ .pos = 19 } };
    // result.ts = &.{"hi :D"};
    // const result2: Result = undefined;
    _ = try parse(cmd, parseFns, std.testing.allocator);
    std.debug.print("typeof ts: {any}, ts: {?s}\n", .{ @TypeOf(result.ts), result.ts });
    std.debug.print("typeof ts2: {any}, ts2: {d}\n", .{ @TypeOf(result.ts2), result.ts2 });
    std.debug.print("typeof ts3: {any}, ts3: {?}\n", .{ @TypeOf(result.ts3), result.ts3 });
    std.debug.print("typeof ts4: {any}, ts4: {s}\n", .{ @TypeOf(result.subcommand.main.ts4), result.subcommand.main.ts4 });
    std.debug.print("typeof pos: {any}, pos: {any}\n", .{ @TypeOf(result.pos), result.pos });
    // std.debug.print("ts: ", .{ @TypeOf(result.ts), result.ts });
    // std.debug.print("typeof ts: {any}, ts: {any}\n", .{ @TypeOf(result2.ts), result2.ts });
    // std.debug.print("{any}\n", .{@TypeOf(result.ts)});
}
