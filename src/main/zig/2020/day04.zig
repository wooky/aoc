const aoc = @import("../aoc.zig");
const std = @import("std");

const Validation = struct {
    const Validators = std.StringHashMap(fn (*const Validation, []const u8)bool);
    const EyeColors = std.StringHashMap(void);

    validators: Validators,
    eye_colors: EyeColors,

    fn init(allocator: *std.mem.Allocator) !Validation {
        var validation = Validation { .validators = Validators.init(allocator), .eye_colors = EyeColors.init(allocator) };

        try validation.validators.putNoClobber("byr", birthYear);
        try validation.validators.putNoClobber("iyr", issueYear);
        try validation.validators.putNoClobber("eyr", expirationYear);
        try validation.validators.putNoClobber("hgt", height);
        try validation.validators.putNoClobber("hcl", hairColor);
        try validation.validators.putNoClobber("ecl", eyeColor);
        try validation.validators.putNoClobber("pid", passportId);

        try validation.eye_colors.putNoClobber("amb", {});
        try validation.eye_colors.putNoClobber("blu", {});
        try validation.eye_colors.putNoClobber("brn", {});
        try validation.eye_colors.putNoClobber("gry", {});
        try validation.eye_colors.putNoClobber("grn", {});
        try validation.eye_colors.putNoClobber("hzl", {});
        try validation.eye_colors.putNoClobber("oth", {});

        return validation;
    }

    fn deinit(self: *Validation) void {
        self.validators.deinit();
        self.eye_colors.deinit();
    }

    fn validate(self: *Validation, field: []const u8, value: []const u8) ?bool {
        const validator = self.validators.get(field) orelse return null;
        return validator(self, value);
    }

    fn birthYear(self: *const Validation, value: []const u8) bool {
        const year = std.fmt.parseInt(u16, value, 10) catch return false;
        return year >= 1920 and year <= 2002;
    }

    fn issueYear(self: *const Validation, value: []const u8) bool {
        const year = std.fmt.parseInt(u16, value, 10) catch return false;
        return year >= 2010 and year <= 2020;
    }

    fn expirationYear(self: *const Validation, value: []const u8) bool {
        const year = std.fmt.parseInt(u16, value, 10) catch return false;
        return year >= 2020 and year <= 2030;
    }

    fn height(self: *const Validation, value: []const u8) bool {
        const edge = value.len - 2;
        const h = std.fmt.parseInt(u8, value[0..edge], 10) catch return false;
        const units = value[edge..];
        if (std.mem.eql(u8, units, "cm")) {
            return h >= 150 and h <= 193;
        }
        if (std.mem.eql(u8, units, "in")) {
            return h >= 59 and h <= 76;
        }
        return false;
    }

    fn hairColor(self: *const Validation, value: []const u8) bool {
        if (value.len != 7 or value[0] != '#') {
            return false;
        }
        _ = std.fmt.parseInt(u24, value[1..], 16) catch return false;
        return true;
    }

    fn eyeColor(self: *const Validation, value: []const u8) bool {
        return self.eye_colors.contains(value);
    }

    fn passportId(self: *const Validation, value: []const u8) bool {
        if (value.len != 9) {
            return false;
        }
        _ = std.fmt.parseInt(u32, value[1..], 10) catch return false;
        return true;
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
