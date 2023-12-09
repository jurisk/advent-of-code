const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;

const data = struct {
    const example = @embedFile("advent_2023_09_test.txt");
    const real = @embedFile("advent_2023_09_real.txt");
};

fn allEqual(list: []const i64, value: i64) bool {
    for (list) |item| {
        if (item != value) {
            return false;
        }
    }
    return true;
}

fn calculateDifferences(list: []const i64) []const i64 {
    var differences = ArrayList(i64).init(std.heap.page_allocator);
    // defer differences.deinit();

    if (list.len > 1) {
        for (list[1..], 0..) |item, i| {
            differences.append(item - list[i]) catch unreachable;
        }
    }

    return differences.items;
}

fn reverseList(list: []const i64) []const i64 {
    var reversed = ArrayList(i64).init(std.heap.page_allocator);
    defer reversed.deinit();

    var i: usize = list.len;
    while (i > 0) {
        i -= 1;
        reversed.append(list[i]) catch unreachable;
    }

    return reversed.toOwnedSlice() catch unreachable;
}

fn extrapolatedValue(list: []const i64) i64 {
    if (allEqual(list, 0)) {
        return 0;
    } else {
        const differences = calculateDifferences(list);
        return list[list.len - 1] + extrapolatedValue(differences);
    }
}

fn part1(input: [][]const i64) i64 {
    var sum: i64 = 0;
    for (input) |list| {
        sum += extrapolatedValue(list);
    }
    return sum;
}

fn part2(input: [][]const i64) i64 {
    var sum: i64 = 0;
    for (input) |list| {
        const reversed = reverseList(list);
        sum += extrapolatedValue(reversed);
    }
    return sum;
}

fn parseIntArray(line: []const u8) !ArrayList(i64) {
    var numbers = ArrayList(i64).init(std.heap.page_allocator);

    var it = mem.tokenizeAny(u8, line, " ");
    while (it.next()) |token| {
        const num = std.fmt.parseInt(i64, token, 10) catch |err| {
            std.debug.print("Failed to parse '{s}': {}\n", .{ token, err });
            unreachable;
        };
        try numbers.append(num);
    }

    return numbers;
}

fn parse(input: []const u8) ![][]const i64 {
    var lines = ArrayList([]const i64).init(std.heap.page_allocator);
    defer lines.deinit();

    const delimiters = [2]u8{ '\n', '\r' };
    var it = mem.tokenizeAny(u8, input, &delimiters);
    while (it.next()) |line| {
        const nums = try parseIntArray(line);
        // defer nums.deinit();
        try lines.append(nums.items);
    }

    return lines.toOwnedSlice();
}

fn solve(input: []const u8, f: *const fn ([][]const i64) i64) !i64 {
    const parsed = try parse(input);
    return f(parsed);
}

test "part 1 example" {
    const result = try solve(data.example, part1);
    try std.testing.expectEqual(@as(i64, 114), result);
}

test "part 1 real" {
    const result = try solve(data.real, part1);
    try std.testing.expectEqual(@as(i64, 1684566095), result);
}

test "part 2 example" {
    const result = try solve(data.example, part2);
    try std.testing.expectEqual(@as(i64, 2), result);
}

test "part 2 real" {
    const result = try solve(data.real, part2);
    try std.testing.expectEqual(@as(i64, 1136), result);
}

pub fn main() !void {
    const result1 = try solve(data.real, part1);
    std.debug.print("Part 1: {d}\n", .{result1});

    const result2 = try solve(data.real, part2);
    std.debug.print("Part 2: {d}\n", .{result2});
}
