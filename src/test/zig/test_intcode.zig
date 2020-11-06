const Intcode = @import("aoc").aoc2019.Intcode;
const std = @import("std");

test "addition multiplication 1" {
    var intcode = try Intcode.init(std.testing.allocator, "1,9,10,3,2,3,11,0,99,30,40,50");
    defer intcode.deinit();
    _ = try intcode.run();
    std.testing.expectEqual(intcode.getMemory(0), 3500);
    std.testing.expectEqual(intcode.getMemory(3), 70);
}

test "addition multiplication 2" {
    var intcode = try Intcode.init(std.testing.allocator, "1,0,0,0,99");
    defer intcode.deinit();
    _ = try intcode.run();
    std.testing.expectEqual(intcode.getMemory(0), 2);
}

test "addition multiplication 3" {
    var intcode = try Intcode.init(std.testing.allocator, "2,3,0,3,99");
    defer intcode.deinit();
    _ = try intcode.run();
    std.testing.expectEqual(intcode.getMemory(3), 6);
}

test "addition multiplication 4" {
    var intcode = try Intcode.init(std.testing.allocator, "2,4,4,5,99,0");
    defer intcode.deinit();
    _ = try intcode.run();
    std.testing.expectEqual(intcode.getMemory(5), 9801);
}

test "addition multiplication 5" {
    var intcode = try Intcode.init(std.testing.allocator, "1,1,1,4,99,5,6,0,99");
    defer intcode.deinit();
    _ = try intcode.run();
    std.testing.expectEqual(intcode.getMemory(0), 30);
    std.testing.expectEqual(intcode.getMemory(4), 2);
}

test "jumps compares" {
    var intcode = try Intcode.init(std.testing.allocator, \\3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        \\1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        \\999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
    );
    defer intcode.deinit();

    var input: Intcode.TapeElement = 0;
    while (input < 20) : (input += 1) {
        intcode.input = input;
        const expected_output: Intcode.TapeElement = switch (std.math.order(input, 8)) {
            .lt => 999,
            .eq => 1000,
            .gt => 1001
        };
        std.testing.expectEqual(try intcode.run(), expected_output);
        intcode.reset();
    }
}
