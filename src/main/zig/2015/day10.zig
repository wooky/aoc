const problem = "1321131112";
const std = @import("std");

const Buffer = struct {
    buf: []u8, size: usize = 0, next: *Buffer = undefined,

    fn init() !Buffer {
        return Buffer { .buf = try std.heap.page_allocator.alloc(u8, 8388608) };
    }

    fn deinit(self: *Buffer) void {
        std.heap.page_allocator.free(self.buf);
    }
};

pub fn main() !void {
    var buf1 = try Buffer.init(); defer buf1.deinit();
    var buf2 = try Buffer.init(); defer buf2.deinit();
    buf1.next = &buf2; buf2.next = &buf1;
    std.mem.copy(u8, buf1.buf, problem);
    buf1.size = problem.len;

    var fourty: usize = undefined;
    var curr = &buf1;
    var i: u8 = 0;
    while (i < 50) : (i += 1) {
        var last_char: u8 = 0;
        var run: usize = 0;
        curr.next.size = 0;
        for (curr.buf[0..curr.size + 1]) |c| {
            if (c != last_char and run > 0) {
                const slice = try std.fmt.bufPrint(curr.next.buf[curr.next.size..curr.next.buf.len], "{}{c}", .{run, last_char});
                curr.next.size += slice.len;
                run = 0;
            }
            last_char = c;
            run += 1;
        }
        curr = curr.next;
        if (i == 39) {
            fourty = curr.size;
        }
    }

    std.debug.warn("{}\n{}\n", .{fourty, curr.size});
}
