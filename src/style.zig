// See: https://en.wikipedia.org/wiki/ANSI_escape_code#Select_Graphic_Rendition_parameters
pub const RED = "\x1b[38:5:1m";
pub const GREEN = "\x1b[38:5:2m";
pub const YELLOW = "\x1b[38:5:3m";
pub const BLUE = "\x1b[38:5:4m";
pub const MAGENTA = "\x1b[38:5:5m";
pub const CYAN = "\x1b[38:5:6m";
pub const WHITE = "\x1b[38:5:7m";
pub const GRAY = "\x1b[38:5:8m";
pub const RED_INT = "\x1b[38:5:9m";
pub const GREEN_INT = "\x1b[38:5:10m";
pub const YELLOW_INT = "\x1b[38:5:11m";
pub const BLUE_INT = "\x1b[38:5:12m";
pub const MAGENTA_INT = "\x1b[38:5:13m";
pub const CYAN_INT = "\x1b[38:5:14m";
pub const WHITE_INT = "\x1b[38:5:15m";

pub const RESET = "\x1b[38:0m";
pub const RESET_FG = "\x1b[38:39m";
pub const BOLD = "\x1b[38:1m";
pub const FAINT = "\x1b[38:2m";
pub const UNDERLINE = "\x1b[38:4m";
pub const NORMAL_INT = "\x1b[38:22m";
pub const NOT_UNDERLINE = "\x1b[38:24m";

const COLOR_ID = RESET_FG;
const COLOR_ID_HDR = RESET_FG;
const COLOR_CAT = RESET_FG;
const COLOR_DUR = CYAN;
const COLOR_MEAN = CYAN;
const COLOR_SIGMA = BLUE;
const COLOR_MIN = GREEN;
const COLOR_MAX = RED;
const COLOR_SYM = RESET_FG;
const COLOR_SUM = RESET_FG;

pub inline fn bold(comptime input: []const u8) []const u8 {
    return BOLD ++ input ++ NORMAL_INT;
}

pub inline fn underline(comptime input: []const u8) []const u8 {
    return UNDERLINE ++ input ++ NOT_UNDERLINE;
}

pub inline fn red(comptime input: []const u8) []const u8 {
    return RED ++ input ++ RESET_FG;
}

pub inline fn green(comptime input: []const u8) []const u8 {
    return GREEN ++ input ++ RESET_FG;
}

pub inline fn cyan(comptime input: []const u8) []const u8 {
    return CYAN ++ input ++ RESET_FG;
}

/// Pad the input by adding spaces on the right.
/// This function accounts for ANSI styling escape sequences when
/// calculating the width.
pub inline fn padRight(comptime input: []const u8, width: usize) []const u8 {
    return input ++ " " ** (width - strWidth(input));
}

/// Count the string width, ignoring ANSI styling escape sequences
pub inline fn strWidth(comptime input: []const u8) usize {
    var found_escape = false;
    var total_width = 0;
    for (input) |b| {
        if (b == 0x1b) {
            found_escape = true;
        }
        if (!found_escape) {
            total_width += 1;
        }
        if (found_escape and b == 'm') {
            found_escape = false;
        }
    }
    return total_width;
}
