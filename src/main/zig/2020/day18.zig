const aoc = @import("../aoc.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;

const ShuntingYard = struct {
    const OperatorStack = std.ArrayList(u8);
    const OutputStack = std.ArrayList(usize);

    operator_stack: OperatorStack,
    output_stack: OutputStack,

    fn init(allocator: Allocator) ShuntingYard {
        return ShuntingYard { 
            .operator_stack = OperatorStack.init(allocator),
            .output_stack = OutputStack.init(allocator),
        };
    }

    fn deinit(self: *ShuntingYard) void {
        self.operator_stack.deinit();
        self.output_stack.deinit();
    }

    fn eval(self: *ShuntingYard, line: []const u8, precedence: bool) !usize {
        for (line) |token| {
            if (token == ' ') {
                continue;
            }
            if (token >= '0' and token <= '9') {
                try self.output_stack.append(token - '0');
            }
            else if (token == '(') {
                try self.operator_stack.append(token);
            }
            else if (token == ')') {
                while (self.operator_stack.popOrNull()) |op| {
                    if (op == '(') {
                        break;
                    }
                    try self.processOp(op);
                }
            }
            else {
                while (self.operator_stack.popOrNull()) |op| {
                    if (op == '(' or (precedence and token == '+' and op == '*')) {
                        try self.operator_stack.append(op);
                        break;
                    }
                    try self.processOp(op);
                }
                try self.operator_stack.append(token);
            }
        }

        while (self.operator_stack.popOrNull()) |op| {
            try self.processOp(op);
        }
        return self.output_stack.pop();
    }

    fn processOp(self: *ShuntingYard, op: u8) !void {
        const rhs = self.output_stack.pop();
        const lhs = self.output_stack.pop();
        try self.output_stack.append(switch (op) {
            '+' => lhs + rhs,
            '*' => lhs * rhs,
            else => unreachable
        });
    }
};

pub fn run(problem: *aoc.Problem) !aoc.Solution {
    var shunting_yard = ShuntingYard.init(problem.allocator);
    defer shunting_yard.deinit();

    var res1: usize = 0;
    var res2: usize = 0;
    while (problem.line()) |line| {
        res1 += try shunting_yard.eval(line, false);
        res2 += try shunting_yard.eval(line, true);
    }

    return problem.solution(res1, res2);
}
