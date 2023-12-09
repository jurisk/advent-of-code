const std = @import("std");
const ArrayList = std.ArrayList;

pub fn reverseList(comptime T: type, list: []const T) []const i64 {
    var reversed = ArrayList(i64).init(std.heap.page_allocator);
    defer reversed.deinit();

    var i: usize = list.len;
    while (i > 0) {
        i -= 1;
        reversed.append(list[i]) catch unreachable;
    }

    return reversed.toOwnedSlice() catch unreachable;
}