clipzig is a Zig Command Line Interface (CLI) parser that takes comptime definitions of a CLI
and uses them to parse command line arguments into a convenient struct.
Here is a concrete example:
```zig
const cmd_type: Command = .{
    .name = "math",
    .description = "A tool to help you do math.",
    .extra_info = "Use 'help <command>' to see help on any of the subcommands.",
    .flags = &.{.{
        .name = "round",
        .short = "-r",
        .long = "--round",
        .description = "Whether to round the result to the nearest integer",
    }},
    .params = &.{.{
        .name = "precision",
        .short = "-p",
        .parser = "u8",
        .description = "The number of decimal places to display in the final result for floating point calculations.",
        .required = false,
        .default = "3",
    }},
    .subcommands = &.{.{
        .name = "add",
        .description = "A subcommand for adding many numbers together",
        .positionals = &.{.{
            .name = "values",
            .num_vals = .many,
            .parser = "f64",
            .description = "values to add",
        }},
    }},
};
const args = &.{ "--round", "add", "1", "2", "3" };
const result = try parseArgs(cmd_type, default_parsers, args, std.testing.allocator);
defer result.deinit();

const cmd = result.parsed_cmd;

try std.testing.expect(cmd.round);
// The `-p` parameter wasn't provided, so it gets set to the (parsed) default value.
try std.testing.expectEqual(cmd.precision, 3);
// The `add` subcommand gets a `values` field because that's the name of that positional.
// Because it takes many values, the resulting type is an ArrayList(f64).
try std.testing.expectEqual(@TypeOf(cmd.subcommands.?.add.values), std.ArrayList(f64));
try std.testing.expectEqualSlices(f64, cmd.subcommands.?.add.values.items, &.{ 1, 2, 3 });
```
