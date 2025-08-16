const std = @import("std");
const Allocator = std.mem.Allocator;

const NumVals = enum {
    one,
    many,
};

const Param = struct {
    /// The name of the param in the result struct.
    name: [:0]const u8,
    /// The name of the parser used to convert the argument string to the correct type.
    parser: [:0]const u8,
    /// Short flag used to specify this parameter. One of short or long is required,
    /// and you can of course provide both.
    short: ?[:0]const u8 = null,
    /// Long flag used to specify this parameter.
    long: ?[:0]const u8 = null,
    /// Default value to use for the parameter. For parameters that take many values,
    /// this will populate a single entry in the list if no values are provided
    /// by the user.
    default: ?[:0]const u8 = null,
    /// Description that will show up in the help output.
    description: ?[:0]const u8 = null,
    /// Number of values this parameter can take (one or many). If it takes many,
    /// it will be translated into an ArrayListUnmanaged in the output.
    num_vals: NumVals = .one,
    /// Arg parsing will fail if a required parameter is not provided.
    /// If a parameter is required, you cannot specify a default value.
    /// For parameters that take a single value and are not required, the
    /// output type will be an optional.
    required: bool = true,
};

const Positional = struct {
    /// The name of the positional in the result struct
    name: [:0]const u8,
    /// The name of the parser used to convert the argument string to the correct type.
    parser: [:0]const u8,
    /// Default value to use for the parameter. For parameters that take many values,
    /// this will populate a single entry in the list if no values are provided
    /// by the user.
    default: ?[:0]const u8 = null,
    /// Description that will show up in the help output.
    description: ?[:0]const u8 = null,
    /// Number of values this parameter can take (one or many). If it takes many,
    /// it will be translated into an ArrayListUnmanaged in the output.
    /// Note that you can only have one positional parameter that takes many values
    /// in the same command.
    num_vals: NumVals = .one,
    /// Arg parsing will fail if a required parameter is not provided.
    /// If a parameter is required, you cannot specify a default value.
    /// For parameters that take a single value and are not required, the
    /// output type will be an optional.
    required: bool = true,
};

const Flag = struct {
    /// The name of the flag in the result struct.
    name: [:0]const u8,
    /// Short flag used to specify this parameter. One of short or long is required,
    /// and you can of course provide both.
    short: ?[:0]const u8 = null,
    /// Long flag used to specify this parameter.
    long: ?[:0]const u8 = null,
    /// Description that will show up in the help output.
    description: ?[:0]const u8 = null,
};

const Command = struct {
    /// The name of the command or subcommand. If this is a subcommand, it can be accessed
    /// from the parent like `cmd.subcommands.<name>`.
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
    /// Note that parsing of the parent command stops once a subcommand is detected.
    /// This means that subcommands will terminate parsing of many-valued parameters,
    /// for instance.
    /// Additionally, subcommands *are* allowed to define flags with the same short/long
    /// as parent commands. Any user input after the subcommand will be parsed into the subcommand.
    subcommands: ?[]const Command = null,
    /// Description that will show up in the help output.
    description: ?[:0]const u8 = null,
};

/// Get the return type of a parser given the parser's name.
/// If the return type is an error union, this extracts the payload type from
/// the error union.
fn getParserPayloadType(parsers: anytype, parser_name: [:0]const u8) type {
    const ret_type = @typeInfo(@TypeOf(@field(parsers, parser_name))).@"fn".return_type.?;
    const ret_type_info = @typeInfo(ret_type);
    return if (ret_type_info == .error_union)
        ret_type_info.error_union.payload
    else
        ret_type;
}

/// Returns the final param type in the result struct (which could be a slice or an optional)
fn getParamType(
    parsers: anytype,
    parser: [:0]const u8,
    num_vals: NumVals,
    required: bool,
    has_default: bool,
) type {
    var p_type = getParserPayloadType(parsers, parser);
    p_type = if (num_vals == .one) p_type else std.ArrayListUnmanaged(p_type);
    return if (!required and num_vals == .one and !has_default) ?p_type else p_type;
}

/// Convert all the parameters into a struct based on the function signatures of the parsers
/// and the default values of the params.
fn ResultType(cmd: Command, parsers: anytype) type {
    validateCommandStructure(cmd);
    const params = cmd.params orelse &.{};
    const flags = cmd.flags orelse &.{};
    const positionals = cmd.positionals orelse &.{};
    // They can theoretically pass an empty slice, in which case we want 0 additional fields for
    // the subcommands
    const subcommands_addtl_field = if (cmd.subcommands) |sub| @min(1, sub.len) else 0;
    const subcommands = cmd.subcommands orelse &.{};

    var fields: [params.len + flags.len + positionals.len + subcommands_addtl_field]std.builtin.Type.StructField = undefined;
    for (0.., params) |i, param| {
        const param_type = getParamType(
            parsers,
            param.parser,
            param.num_vals,
            param.required,
            param.default != null,
        );
        fields[i] = .{
            .name = param.name,
            .type = param_type,
            .default_value_ptr = null,
            .is_comptime = false,
            .alignment = @alignOf(param_type),
        };
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
        const positional_type = getParamType(
            parsers,
            positional.parser,
            positional.num_vals,
            positional.required,
            positional.default != null,
        );
        fields[i] = .{
            .name = positional.name,
            .type = positional_type,
            .default_value_ptr = null,
            .is_comptime = false,
            .alignment = @alignOf(positional_type),
        };
    }

    if (subcommands.len > 0) {
        var subcommands_enum_fields: [subcommands.len]std.builtin.Type.EnumField = undefined;
        var subcommands_union_fields: [subcommands.len]std.builtin.Type.UnionField = undefined;
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
            .tag_type = std.math.IntFittingRange(0, subcommands.len - 1),
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

        const subcommands_opt = @Type(.{ .optional = .{
            .child = subcommands_tagged_union,
        } });

        fields[fields.len - 1] = .{
            .name = "subcommands",
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

var parse_fns = .{
    .int = parseu8,
    .ts = parseTs,
    .ts2 = parseTs2,
    .str = parseStr,
    .int16 = parseu16,
};

/// Retrieve a parser, but give it a signature that errors so that we can always use "try" to get the
/// resulting type.
fn getParser(parsers: anytype, comptime parser_name: [:0]const u8) fn ([:0]const u8) anyerror!getParserPayloadType(parsers, parser_name) {
    return struct {
        fn parse(input: [:0]const u8) !getParserPayloadType(parsers, parser_name) {
            return @field(parsers, parser_name)(input);
        }
    }.parse;
}

fn arrayContainsString(haystack: [1000][:0]const u8, needle: [:0]const u8, haystack_len: usize) bool {
    for (haystack[0..haystack_len]) |h| {
        if (std.mem.eql(u8, h, needle)) {
            return true;
        }
    }
    return false;
}

/// Ensures all constraints about the command are enforced.
fn validateCommandStructure(cmd: Command) void {
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
            if (arrayContainsString(all_the_names, long, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{long}));
            }
            all_the_names[all_the_names_len] = long;
            all_the_names_len += 1;
        }
        if (param.short) |short| {
            if (arrayContainsString(all_the_names, short, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{short}));
            }
            all_the_names[all_the_names_len] = short;
            all_the_names_len += 1;
        }
        if (arrayContainsString(struct_names, param.name, struct_names_len)) {
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
            if (arrayContainsString(all_the_names, long, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{long}));
            }
            all_the_names[all_the_names_len] = long;
            all_the_names_len += 1;
        }
        if (flag.short) |short| {
            if (arrayContainsString(all_the_names, short, all_the_names_len)) {
                @compileError(std.fmt.comptimePrint("Found duplicate flag specifier: {s}\n", .{short}));
            }
            all_the_names[all_the_names_len] = short;
            all_the_names_len += 1;
        }
        if (arrayContainsString(struct_names, flag.name, struct_names_len)) {
            @compileError(std.fmt.comptimePrint("Found duplicate struct field name: {s}\n", .{flag.name}));
        }
        struct_names[struct_names_len] = flag.name;
        struct_names_len += 1;
    }

    var num_positionals_many = 0;
    for (positionals) |positional| {
        if (positional.num_vals == .many) {
            num_positionals_many += 1;
            if (num_positionals_many > 1) {
                @compileError("Cannot have multiple positionals that take more than one value");
            }
        }
        if (arrayContainsString(struct_names, positional.name, struct_names_len)) {
            @compileError(std.fmt.comptimePrint("Found duplicate struct field name: {s}\n", .{positional.name}));
        }
        struct_names[struct_names_len] = positional.name;
        struct_names_len += 1;
    }
    if (num_positionals_many > 0 and positionals.len > 1) {
        @compileError("Cannot have multiple positionals in the same command when one of them takes many values.");
    }

    for (subcommands) |subcommand| {
        if (arrayContainsString(struct_names, subcommand.name, struct_names_len)) {
            @compileError(std.fmt.comptimePrint("Found duplicate struct field name: {s}\n", .{subcommand.name}));
        }
        struct_names[struct_names_len] = subcommand.name;
        struct_names_len += 1;
    }

    for (subcommands) |subcommand| {
        validateCommandStructure(subcommand);
    }
}

fn validateParsers(parsers: anytype) void {
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

        const ret_type = getParserPayloadType(parsers, field.name);
        if (@typeInfo(ret_type) == .optional) {
            @compileError(std.fmt.comptimePrint(
                "error: parser cannot return an optional type. Found return type {any} for parser '{s}'\n",
                .{ ret_type, field.name },
            ));
        }
    }
}

fn strMatchesShortOrLong(arg: [:0]const u8, short: ?[:0]const u8, long: ?[:0]const u8) bool {
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

fn parseArgsWithParserValidation(
    comptime cmd: Command,
    parsers: anytype,
    args: []const [:0]const u8,
    alloc: Allocator,
    // Since we run this function recursively, we don't want to validate the parsers every single
    // time. This check avoids that.
    needs_parsers_validated: bool,
) !ResultType(cmd, parsers) {
    if (needs_parsers_validated) {
        validateParsers(parsers);
    }
    var result: ResultType(cmd, parsers) = undefined;
    const params = cmd.params orelse &.{};
    const flags = cmd.flags orelse &.{};
    const positionals = cmd.positionals orelse &.{};
    const subcommands = cmd.subcommands orelse &.{};

    var found_params = std.StringArrayHashMap(void).init(alloc);
    defer found_params.deinit();

    // Need to initialize all the things
    inline for (params) |param| {
        if (param.num_vals == .many) {
            @field(result, param.name) = try std.ArrayListUnmanaged(getParserPayloadType(parsers, param.parser)).initCapacity(alloc, 0);
        } else if (param.default != null) {
            const default = try getParser(parsers, param.parser)(param.default.?);
            @field(result, param.name) = default;
        } else if (!param.required and param.default == null) {
            @field(result, param.name);
        }
    }

    inline for (positionals) |positional| {
        if (positional.num_vals == .many) {
            @field(result, positional.name) = try std.ArrayListUnmanaged(getParserPayloadType(parsers, positional.parser)).initCapacity(alloc, 0);
        } else if (positional.default != null) {
            @field(result, positional.name) = positional.default.?;
        } else if (!positional.required and positional.default == null) {
            @field(result, positional.name) = null;
        }
    }

    inline for (flags) |flag| {
        @field(result, flag.name) = false;
    }

    if (subcommands.len > 0) {
        result.subcommands = null;
    }

    var i: usize = 0;
    var positional_ind: usize = 0;
    wh: while (i < args.len) : (i += 1) {
        var arg = args[i];
        // I would use hashmap lookups instead of all these loops, but can't do that due to comptime.
        inline for (subcommands) |subcommand| {
            if (std.mem.eql(u8, arg, subcommand.name)) {
                const subcommand_type = @typeInfo(@TypeOf(@field(result, "subcommands"))).optional.child;
                @field(result, "subcommands") = @unionInit(subcommand_type, subcommand.name, try parseArgsWithParserValidation(
                    subcommand,
                    parsers,
                    // I'm surprised this works even if i + 1 == args.len.
                    // slice[slice.len..slice.len] gives an empty slice, as does
                    // slice[slice.len..]
                    args[i + 1 ..],
                    alloc,
                    false,
                ));
                break :wh;
            }
        }

        inline for (flags) |flag| {
            if (strMatchesShortOrLong(arg, flag.short, flag.long)) {
                @field(result, flag.name) = true;
                continue :wh;
            }
        }

        inline for (params) |param| {
            if (strMatchesShortOrLong(arg, param.short, param.long)) {
                i += 1;
                if (i >= args.len) {
                    std.debug.print("Failed to get value for argument {s}\n", .{arg});
                    return error.InvalidArguments;
                }
                arg = args[i];
                const val = try getParser(parsers, param.parser)(arg);
                if (param.num_vals == .one) {
                    @field(result, param.name) = val;
                } else {
                    const list = &@field(result, param.name);
                    try list.append(alloc, val);
                }
                try found_params.put(param.name, {});
                continue :wh;
            }
        }

        if (positional_ind >= positionals.len) {
            std.debug.print("Unrecognized argument: {s}\n", .{arg});
            return error.UnrecognizedArgument;
        }

        // I hate this. I couldn't figure out another way to make it work with comptime.
        inline for (0.., positionals) |j, positional| {
            if (j == positional_ind) {
                const val = try getParser(parsers, positional.parser)(arg);
                if (positional.num_vals == .one) {
                    @field(result, positional.name) = val;
                    positional_ind += 1;
                    try found_params.put(positional.name, {});
                    break;
                } else {
                    const list = &@field(result, positional.name);
                    try list.append(alloc, val);
                    try found_params.put(positional.name, {});
                    break;
                }
            }
        }
    }

    // If we didn't get any values for params/positionals that take many values,
    // we can set the default values.
    inline for (params) |param| {
        if (param.num_vals == .many) {
            const list = &@field(result, param.name);
            if (list.items.len == 0 and param.default != null) {
                try list.append(alloc, try getParser(parsers, param.parser)(param.default.?));
            }
        }
    }

    inline for (positionals) |positional| {
        if (positional.num_vals == .many) {
            const list = &@field(result, positional.name);
            if (list.items.len == 0 and positional.default != null) {
                try list.append(alloc, try getParser(parsers, positional.parser)(positional.default.?));
            }
        }
    }

    // Validate all required fields
    for (params) |param| {
        if (param.required) {
            const exists = found_params.get(param.name);
            if (exists == null) {
                std.debug.print("Required parameter '{s}' not provided.\n", .{param.name});
                return error.RequiredParamNotProvided;
            }
        }
    }
    for (positionals) |positional| {
        if (positional.required) {
            const exists = found_params.get(positional.name);
            if (exists == null) {
                std.debug.print("Required parameter '{s}' not provided.\n", .{positional.name});
                return error.RequiredParamNotProvided;
            }
        }
    }

    return result;
}

fn parseArgs(
    comptime cmd: Command,
    parsers: anytype,
    /// Args should *not* include the executable.
    args: []const [:0]const u8,
    alloc: Allocator,
) !ResultType(cmd, parsers) {
    return parseArgsWithParserValidation(cmd, parsers, args, alloc, true);
}

fn parse(
    comptime cmd: Command,
    /// `parsers` should be a struct that maps the name of the parser to a parsing function.
    /// Each parsing function takes an argument as a [:0]const u8 and returns the type
    /// that you want the value to be parsed into. It's allowed to return an error.
    /// If it does return an error, the parser will simply `try` the result.
    parsers: anytype,
    alloc: Allocator,
) !ResultType(
    cmd,
    parsers,
) {
    const args = try std.process.argsAlloc(alloc);
    return parseArgs(cmd, parsers, args[1..], alloc);
}

test "cmd" {
    const param1 = Param{
        .parser = "str",
        .name = "ts",
        .long = "--timestamp",
        .num_vals = .many,
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
        .num_vals = .one,
        .default = "howdy :D",
        .required = false,
    };
    const flag1 = Flag{ .name = "f", .short = "-f" };
    // const pos1 = Positional{ .name = "pos", .parser = "int", .required = false };
    const pos2 = Positional{ .name = "pos2", .parser = "int", .required = false, .num_vals = .many };
    const sub = Command{
        .name = "main",
        .params = &.{param4},
        .positionals = &.{pos2},
        .subcommands = &.{},
        .flags = &.{flag1},
    };
    const cmd = Command{
        .name = "main",
        .params = &.{ param1, param2, param3 }, //, param3 },
        .flags = &.{flag1},
        .positionals = &.{pos2},
        .subcommands = &.{sub},
    };
    // const res_sub: ResultType(sub, parse_fns) = .{};
    // std.debug.print("res sub ts4 is {any}\n", .{res_sub.ts4});
    var args = [_][:0]const u8{ "-t", "1", "-t2", "1", "main" }; //  , "--timestamp", "19", "--timestamp", "17"};
    var res = try parseArgs(cmd, parse_fns, args[0..], std.testing.allocator);
    defer res.ts.deinit(std.testing.allocator);
    std.debug.print("ts is {any}\n", .{res.ts.items[0]});
    std.debug.print("ts is {any}\n", .{res.ts2});
    if (res.subcommands) |s| {
        std.debug.print("got subcommand: {any}\n", .{s});
    }
    const x: []const u8 = &.{1};
    std.debug.print("{any}\n", .{x[1..]});
}
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    const param1 = Param{
        .parser = "str",
        .name = "ts",
        .long = "--timestamp",
        .num_vals = .many,
        .default = "hello :)",
        .required = false,
    };
    const param2 = Param{
        .parser = "int",
        .name = "ts2",
        .short = "-ts2",
    };
    const param3 = Param{
        .parser = "int16",
        .name = "ts3",
        .short = "-ts3",
    };

    const param4 = Param{
        .parser = "str",
        .name = "ts",
        .long = "--timestamp",
        .num_vals = .one,
        .default = "howdy :D",
        .required = false,
    };

    const flag1 = Flag{ .name = "f", .short = "-f" };
    const pos1 = Positional{ .name = "pos1", .parser = "int", .required = false };
    const pos2 = Positional{ .name = "pos2", .parser = "int", .required = false };
    const pos3 = Positional{ .name = "pos3", .parser = "str", .required = false };
    const pos4 = Positional{
        .name = "pos4",
        .parser = "int",
        .required = false,
        .num_vals = .many,
        .default = "91",
    };
    const sub = Command{
        .name = "subcmd",
        .params = &.{param4},
        .positionals = &.{pos4},
        .subcommands = &.{},
        .flags = &.{flag1},
    };
    const cmd = Command{
        .name = "main",
        .params = &.{ param1, param2, param3 }, //, param3 },
        .flags = &.{flag1},
        .positionals = &.{ pos1, pos2, pos3 },
        .subcommands = &.{sub},
    };
    const res = try parse(cmd, parse_fns, alloc);
    std.debug.print("ts: {any}\n", .{res.ts});
    std.debug.print("pos1: {any}\n", .{res.pos1});
    std.debug.print("pos2: {any}\n", .{res.pos2});
    std.debug.print("pos3: {?s}\n", .{res.pos3});
    std.debug.print("subcmd ts: {s}\n", .{res.subcommands.?.subcmd.ts});
    std.debug.print("pos4: {any}\n", .{res.subcommands.?.subcmd.pos4});
}
