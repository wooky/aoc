const aoc = @import("../aoc.zig");
const std = @import("std");
const regex = @cImport(
    @cInclude("regex.h")
);

const Validation = struct {
    const regex_t = [48]u8; // translate-c can't convert regex.regex_t properly, so this abhorrent hack is used instead
    const Validators = std.StringHashMap(regex_t);

    validators: Validators,

    fn init(allocator: *std.mem.Allocator) !Validation {
        var validation = Validation { .validators = Validators.init(allocator) };
        return validation.addRegex("byr", "^(19[2-9][0-9]|200[0-2])$")
                         .addRegex("iyr", "^20(1[0-9]|20)$")
                         .addRegex("eyr", "^20(2[0-9]|30)$")
                         .addRegex("hgt", "^((1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in)$")
                         .addRegex("hcl", "^#[0-9a-f]{6}$")
                         .addRegex("ecl", "^(amb|blu|brn|gry|grn|hzl|oth)$")
                         .addRegex("pid", "^[0-9]{9}$");
    }

    fn deinit(self: *Validation) void {
        var iter = self.validators.iterator();
        while (iter.next()) |kv| {
            regex.regfree(@ptrCast(*regex.regex_t, &kv.value));
        }

        self.validators.deinit();
    }

    fn addRegex(self: *Validation, field: []const u8, pattern: [:0]const u8) Validation {
        var r: regex_t = undefined;
        _ = regex.regcomp(@ptrCast(*regex.regex_t, &r), pattern, regex.REG_EXTENDED);
        self.validators.putNoClobber(field, r) catch unreachable;
        return self.*;
    }

    fn validate(self: *const Validation, field: []const u8, value: []const u8) ?bool {
        const validator = self.validators.get(field) orelse return null;
        var buf: [15:0]u8 = undefined;
        std.mem.copy(u8, &buf, value);
        buf[value.len] = 0;
        return regex.regexec(@ptrCast(*const regex.regex_t, &validator), &buf, 0, null, 0) == 0;
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var validation = try Validation.init(problem.allocator);
    defer validation.deinit();

    var present: usize = 0;
    var valid: usize = 0;

    while (problem.group()) |group| {
        var fields_present: u8 = 0;
        var all_valid = true;
        var tokens = std.mem.tokenize(group, ": \n");
        while (tokens.next()) |field| {
            const value = tokens.next().?;
            if (validation.validate(field, value)) |validation_result| {
                fields_present += 1;
                all_valid = all_valid and validation_result;
            }
        }
        if (fields_present == 7) {
            present += 1;
            if (all_valid) {
                valid += 1;
            }
        }
    }

    return aoc.Solution { .p1 = present, .p2 = valid };
}
