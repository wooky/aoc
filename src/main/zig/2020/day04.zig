const aoc = @import("../aoc.zig");
const std = @import("std");

const Validation = struct {
    const Validators = std.StringHashMap(aoc.Regex);

    validators: Validators,

    fn init(allocator: std.mem.Allocator) !Validation {
        var validation = Validation{ .validators = Validators.init(allocator) };
        return validation.addRegex("byr", "^(19[2-9][0-9]|200[0-2])$")
            .addRegex("iyr", "^20(1[0-9]|20)$")
            .addRegex("eyr", "^20(2[0-9]|30)$")
            .addRegex("hgt", "^((1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in)$")
            .addRegex("hcl", "^#[0-9a-f]{6}$")
            .addRegex("ecl", "^(amb|blu|brn|gry|grn|hzl|oth)$")
            .addRegex("pid", "^[0-9]{9}$").*;
    }

    fn deinit(self: *Validation) void {
        var iter = self.validators.iterator();
        while (iter.next()) |kv| {
            kv.value_ptr.deinit();
        }
        self.validators.deinit();
    }

    fn addRegex(self: *Validation, field: []const u8, pattern: [:0]const u8) *Validation {
        self.validators.putNoClobber(field, aoc.Regex.compilez(pattern)) catch unreachable;
        return self;
    }

    fn validate(self: *const Validation, field: []const u8, value: []const u8) ?bool {
        const validator = self.validators.get(field) orelse return null;
        return validator.matches(value);
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
        var tokens = std.mem.tokenize(u8, group, ": \n");
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

    return problem.solution(present, valid);
}
