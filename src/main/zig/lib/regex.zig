const std = @import("std");
const regexh = @cImport(
    @cInclude("regex.h")
);
const regex_t = [48]u8; // translate-c can't convert regex.regex_t properly, so this abhorrent hack is used instead
const Self = @This();
const ZeroTerminatedBuffer = [128:0]u8;

regex: regex_t = undefined,

pub fn compilez(pattern: [:0]const u8) Self {
    var self = Self {};
    _ = regexh.regcomp(@ptrCast(*regexh.regex_t, &self.regex), pattern, regexh.REG_EXTENDED);
    return self;
}

pub fn deinit(self: *Self) void {
    regexh.regfree(@ptrCast(*regexh.regex_t, &self.regex));
}

pub fn matches(self: *const Self, string: []const u8) bool {
    return self.matchez(&zeroTerminate(string));
}

pub fn matchez(self: *const Self, string: [:0]const u8) bool {
    return regexh.regexec(@ptrCast(*const regexh.regex_t, &self.regex), string, 0, null, 0) == 0;
}

fn zeroTerminate(string: []const u8) ZeroTerminatedBuffer {
    var buf: ZeroTerminatedBuffer = undefined;
    std.mem.copy(u8, &buf, string);
    buf[string.len] = 0;
    return buf;
}
