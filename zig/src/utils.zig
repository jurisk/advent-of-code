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

pub fn allEqual(comptime T: type, list: []const T, value: T) bool {
    for (list) |item| {
        if (item != value) {
            return false;
        }
    }
    return true;
}

pub fn calculateDifferences(allocator: *const std.mem.Allocator, list: []const i64) []i64 {
    if (list.len <= 1) return &[_]i64{};

    var differences = allocator.alloc(i64, list.len - 1) catch unreachable;

    for (list[1..], 0..) |item, i| {
        differences[i] = item - list[i];
    }

    return differences;
}
