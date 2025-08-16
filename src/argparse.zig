const std = @import("std");
const Allocator = std.mem.Allocator;

fn parseu8(in: [:0]const u8) !u8 {
    return std.fmt.parseInt(u8, in, 10) catch {
        return 0;
    };
}

fn parseu16(in: [:0]const u8) !u16 {
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

/// Retrieve a parser, but give it a signature that errors so that we can always use "try" to get the
/// resulting type.
fn getParser(parsers: anytype, comptime parser_name: [:0]const u8) fn ([:0]const u8) anyerror!getParamTypeErrorStripped(parser_name) {
    return struct {
        fn parse(input: [:0]const u8) !getParamTypeErrorStripped(parser_name) {
            return @field(parsers, parser_name)(input);
        }
    }.parse;
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
        if (param.required and param.default != null) {
            @compileError(std.fmt.comptimePrint(
                "Param cannot be required and have a default value: {s}\n",
                .{param.name},
            ));
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
    parsers: anytype,
) std.builtin.Type.StructField {
    const p_type_raw = getParamTypeErrorStripped(parser);
    var p_type = if (value_count == .one) p_type_raw else std.ArrayListUnmanaged(p_type_raw);
    p_type = if (!required and value_count == .one and default == null) ?p_type else p_type;

    // We use the parseFn with the default value string to determine the default value.
    // If the caller provided an invalid default value causing the parseFn to error,
    // we have to provide a nice compileError, or else it's very hard to figure out
    // what went wrong.
    const default_val_ptr = outer: {
        if (value_count == .many) {
            // We can't put the default value in here. We do it in the parse function instead.
            const items: []p_type_raw = &.{};
            const default_val_list = p_type{ .items = items };
            break :outer @as(*const anyopaque, @ptrCast(&default_val_list));
        } else if (default) |d| {
            const default_val: p_type = getParser(parsers, parser)(d) catch {
                @compileError(std.fmt.comptimePrint(
                    "Failed to compute default value using parser '{s}' using default value string '{s}'\n",
                    .{ parser, d },
                ));
            };
            // if (std.mem.eql(u8, name, "ts4")) {
            //     @compileLog("set default val for ts4: {any}\n", .{default_val});
            //     @compileLog("type is", p_type);
            // }
            break :outer @as(*const anyopaque, @ptrCast(&default_val));
        } else if (required) {
            break :outer null;
        } else {
            const default_val: p_type = null;
            break :outer @as(*const anyopaque, @ptrCast(&default_val));
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
fn ResultType(cmd: Command, parsers: anytype) type {
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
            parsers,
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
            parsers,
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
            const sub_type = ResultType(subcommand, parsers);
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

        const subcommands_opt = @Type(.{.optional = .{
            .child = subcommands_tagged_union,
        }});

        fields[fields.len - 1] = .{
            .name = "subcommand",
            .type = subcommands_opt,
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
    /// Default value to use for the parameter. For parameters that take many values,
    /// this will populate a single entry in the list if no values are provided
    /// by the user.
    default: ?[:0]const u8 = null,
    description: ?[:0]const u8 = null,
    value_count: ValueCount = .one,
    /// Arg parsing will fail if a required parameter is not provided.
    /// A parameter cannot be required and specify a default value.
    /// For parameters that take a single value and are not required, the
    /// output type will be an optional.
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
    /// Default value to use for the parameter. For parameters that take many values,
    /// this will populate a single entry in the list if no values are provided
    /// by the user.
    default: ?[:0]const u8 = null,
    description: ?[:0]const u8 = null,
    value_count: ValueCount = .one,
    /// Arg parsing will fail if a required parameter is not provided.
    /// A parameter cannot be required and specify a default value.
    /// For parameters that take a single value and are not required, the
    /// output type will be an optional.
    required: bool = true,
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
                "error: parser cannot return an optional type. Found return type {any} for parser '{s}'\n",
                .{ ret_type, field.name },
            ));
        }
    }
}

const FlagType = enum {
    flag,
    param,
};

const CommonParamData = struct {
    value_count: ValueCount,
    name: [:0]const u8,
    param_type: FlagType,
};

fn matchesShortOrLong(arg: [:0]const u8, short: ?[:0]const u8, long: ?[:0]const u8) bool {
    if (short) |s| {
        if (std.mem.eql(u8, arg, s)) {
            return true;
        }
    }
    if (long) |l| {
        if (std.mem.eql(u8, arg, l)) {
            return true;
        }
    }
    return false;
}

fn parse(comptime cmd: Command, parsers: anytype, args: [][:0]const u8, alloc: Allocator) !ResultType(cmd, parsers) {
    var result: ResultType(cmd, parsers) = undefined;

    // Need to initialize all the things
    if (cmd.params) |params| {
        inline for (params) |param| {
            if (param.value_count == .many) {
                @field(result, param.name) = try std.ArrayListUnmanaged(getParamTypeErrorStripped(param.parser)).initCapacity(alloc, 0);
            } else if (param.default != null) {
                const default = try getParser(parsers, param.parser)(param.default.?);
                std.debug.print("setting default val {s} for param {s}\n", .{default, param.name});
                @field(result, param.name) = default;
            }
        }
    }

    if (cmd.positionals) |positionals| {
        inline for (positionals) |positional| {
            if (positional.value_count == .many) {
                @field(result, positional.name) = try std.ArrayListUnmanaged(getParamTypeErrorStripped(positional.parser)).initCapacity(alloc, 0);
            } else if (positional.default != null) {
                @field(result, positional.name) = positional.default.?;
            }
        }
    }

    if (cmd.flags) |flags| {
        inline for (flags) |flag| {
            @field(result, flag.name) = false;
        }
    }

    if (cmd.subcommands) |s| {
        if (s.len > 0) {
            result.subcommand = null;
        }
    }

    var i: usize = 0;
    wh: while (i < args.len) : (i += 1) {
        var arg = args[i];
        if (cmd.subcommands) |subcommands| {
            inline for (subcommands) |subcommand| {
                if (std.mem.eql(u8, arg, subcommand.name)) {
                    const subcommand_type = @typeInfo(@TypeOf(@field(result, "subcommand"))).optional.child;
                    @field(result, "subcommand") = @unionInit(subcommand_type, subcommand.name, try parse(subcommand, parsers, args[i + 1..], alloc));
                    // TODO: ensure all required fields are populated
                    break :wh;
                }
            }
        }

        if (cmd.flags) |flags| {
            inline for (flags) |flag| {
                if (matchesShortOrLong(arg, flag.short, flag.long)) {
                    @field(result, flag.name) = true;
                    continue :wh;
                }
            }
        }

        if (cmd.params) |params| {
            inline for (params) |param| {
                if (std.mem.eql(u8, param.name, "ts4")) {
                    std.debug.print("checking param ts4\n", .{});
                }
                if (matchesShortOrLong(arg, param.short, param.long)) {
                    i += 1;
                    if (i >= args.len) {
                        std.debug.print("Failed to get value for argument {s}\n", .{arg});
                        return error.InvalidArguments;
                        // break;
                    }
                    arg = args[i];
                    const val = try getParser(parsers, param.parser)(arg);
                    if (std.mem.eql(u8, param.name, "ts4")) {
                        std.debug.print("checking param ts4: got val {any}\n", .{val});
                    }
                    if (param.value_count == .one) {
                        if (std.mem.eql(u8, param.name, "ts4")) {
                            std.debug.print("checking param ts4: set val {any}\n", .{val});
                        }
                        @field(result, param.name) = val;
                    } else {
                        const list = &@field(result, param.name);
                        try list.append(alloc, try getParser(parsers, param.parser)(param.default.?));
                    continue :wh;
                }
            }
        }

        if (cmd.positionals) |positionals| {
            _ = positionals;
        }

        // if (flag_to_param_data.get(arg)) |pdata| {
        //     if (pdata.param_type == .flag) {
        //         @field(result, pdata.name) = true;
        //     } else if (pdata.param_type == .param) {
        //         i += 1;
        //         if (i >= args.len) {
        //             std.debug.print("Failed to get value for argument {s}\n", .{arg});
        //             return error.InvalidArguments;
        //             // break;
        //         }
        //         arg = args[i];
        //         @field(result, pdata) = try getParser(parsers, pdata.name)(arg);
        //     }
        // }
        //
        // if (cmd.params) |params| {
        //     inline for (params) |param| {
        //         const flags_to_check = &.{ param.short, param.long };
        //         inline for (flags_to_check) |flag_opt| {
        //             if (flag_opt) |flag| {
        //                 if (std.mem.eql(u8, flag, arg)) {
        //                     std.debug.print("found flag: {s}\n", .{flag});
        //                     i += 1;
        //                     if (i >= args.len) {
        //                         std.debug.print("Failed to get next arg for flag {s}\n", .{flag});
        //                         break;
        //                     }
        //                     arg = args[i];
        //                     const res_type = getParamTypeErrorStripped(param.parser);
        //                     const arg_parsed: res_type = try getParser(parsers, param.parser)(arg);
        //                     std.debug.print("parsed value: {any}\n", .{arg_parsed});
        //                     if (param.value_count == .many) {
        //                         var sl = @field(result, param.name);
        //                         try sl.append(alloc, arg_parsed);
        //                     } else {
        //                         std.debug.print("param {s} is not many\n", .{param.name});
        //                         @field(result, param.name) = arg_parsed;
        //                     }
        //                 }
        //             }
        //         }
        //     }
        }
    }

    if (cmd.params) |params| {
        inline for (params) |param| {
            if (param.value_count == .many) {
                std.debug.print("checking default for {s}\n", .{param.name});
                const list = &@field(result, param.name);
                if (list.items.len == 0 and param.default != null) {
                    std.debug.print("adding default value to param {s}\n", .{param.name});
                    try list.append(alloc, try getParser(parsers, param.parser)(param.default.?));
                    std.debug.print("appended value to {s}\n", .{param.name});
                } else {
                    std.debug.print("list len is {d}, default is {?s}\n", .{ list.items.len, param.default });
                }
            }
        }
    }
    return result;
}

test "cmd" {
    const param1 = Param{
        .parser = "str",
        .name = "ts",
        .long = "--timestamp",
        .value_count = .many,
        .default = "hello :)",
        .required = false,
    };
    const param2 = Param{ .parser = "int", .name = "ts2", .short = "-t" };
    const param3 = Param{
        .parser = "int16",
        .name = "ts3",
        .short = "-t2",
    };
    const param4 = Param{
        .parser = "str",
        .name = "ts4",
        .long = "--timestamp",
        .value_count = .one,
        .default = "howdy :D",
        .required = false,
    };
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
    const Result = ResultType(cmd, parseFns);
    var result: Result = .{
        .subcommand = .{ .main = .{
            .pos = 1,
            .f = true,
        } },
        .ts3 = 19,
        .ts2 = 9,
    };
    const res_sub: ResultType(sub, parseFns) = .{};
    std.debug.print("res sub ts4 is {any}\n", .{res_sub.ts4});
    // result.pos = 8;
    // result.subcommand = .{ .main = .{ .pos = 19 } };
    // result.ts = &.{"hi :D"};
    // const result2: Result = undefined;
    // if (result2.ts == undefined) {
    //     std.debug.print("val is undefined");
    //     // std.debug.print("flag: {any}\n", .{result2.ts.?.len});
    // }
    // result2.ts = &.{};
    // if (result2.ts == undefined) {
    //     std.debug.print("val is undefined");
    //     // std.debug.print("flag: {any}\n", .{result2.ts.?.len});
    // }
    //
    // // const args = try std.process.argsAlloc(std.testing.allocator);
    var args = [_][:0]const u8{ "-t", "1", "main" }; //  , "--timestamp", "19", "--timestamp", "17"};
    var res = try parse(cmd, parseFns, args[0..], std.testing.allocator);
    defer res.ts.deinit(std.testing.allocator);
    try result.ts.append(std.testing.allocator, "test");
    defer result.ts.deinit(std.testing.allocator);
    std.debug.print("ts is {any}\n", .{res.ts.items[0]});
    std.debug.print("ts is {any}\n", .{res.ts2});
    if (res.subcommand) |s| {
        std.debug.print("got subcommand: {any}\n", .{s});
    }
    // std.debug.print("ts is {any}\n", .{res.subcommand.?});
    // std.debug.print("typeof ts: {any}, ts: {any}\n", .{ @TypeOf(result.ts), result.ts });
    // std.debug.print("typeof ts2: {any}, ts2: {d}\n", .{ @TypeOf(result.ts2), result.ts2 });
    // std.debug.print("typeof ts3: {any}, ts3: {d}\n", .{ @TypeOf(result.ts3), result.ts3 });
    // std.debug.print("typeof ts4: {any}, ts4: {s}\n", .{ @TypeOf(result.subcommand.main.ts4), result.subcommand.main.ts4 });
    // std.debug.print("typeof pos: {any}, pos: {any}\n", .{ @TypeOf(result.pos), result.pos });
    // // std.debug.print("ts: ", .{ @TypeOf(result.ts), result.ts });
    // // std.debug.print("typeof ts: {any}, ts: {any}\n", .{ @TypeOf(result2.ts), result2.ts });
    // // std.debug.print("{any}\n", .{@TypeOf(result.ts)});
}
