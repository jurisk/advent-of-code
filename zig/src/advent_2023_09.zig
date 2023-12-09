const std = @import("std");
const ArrayList = std.ArrayList;
const mem = std.mem;
const utils = @import("utils.zig");
const reverseList = utils.reverseList;
const allEqual = utils.allEqual;
const calculateDifferences = utils.calculateDifferences;
const Allocator = std.mem.Allocator;

const data = struct {
    const example = @embedFile("advent_2023_09_test.txt");
    const real = @embedFile("advent_2023_09_real.txt");
};

fn extrapolatedValue(list: []const i64) i64 {
    if (mem.allEqual(i64, list, 0)) {
        return 0;
    } else {
        const differences = calculateDifferences(&std.heap.page_allocator, list);
        const result = list[list.len - 1] + extrapolatedValue(differences);
        std.heap.page_allocator.free(differences);
        return result;
    }
}

fn freeInputArray(allocator: Allocator, input: [][]const i64) void {
    for (input) |slice| {
        allocator.free(slice);
    }
    allocator.free(input);
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
        const reversed = reverseList(i64, list);
        sum += extrapolatedValue(reversed);
        std.heap.page_allocator.free(reversed);
    }
    return sum;
}

fn parseIntArray(line: []const u8) []const i64 {
    var numbers = ArrayList(i64).init(std.heap.page_allocator);
    defer numbers.deinit();

    var it = mem.tokenizeAny(u8, line, " ");
    while (it.next()) |token| {
        const num = std.fmt.parseInt(i64, token, 10) catch |err| {
            std.debug.print("Failed to parse '{s}': {}\n", .{ token, err });
            unreachable;
        };
        numbers.append(num) catch unreachable;
    }

    return numbers.toOwnedSlice() catch unreachable;
}

fn parse(allocator: Allocator, input: []const u8) ![][]const i64 {
    var lines = ArrayList([]const i64).init(allocator);
    defer lines.deinit();

    const delimiters = [2]u8{ '\n', '\r' };
    var it = mem.tokenizeAny(u8, input, &delimiters);
    while (it.next()) |line| {
        const nums = parseIntArray(line);
        try lines.append(nums);
    }

    return lines.toOwnedSlice();
}

fn solve(allocator: Allocator, input: []const u8, f: *const fn ([][]const i64) i64) !i64 {
    const parsed = try parse(allocator, input);
    const result = f(parsed);
    allocator.free(parsed);
    return result;
}

fn check(input: []const u8, f: *const fn ([][]const i64) i64, expected: i64) !void {
    const result = try solve(std.testing.allocator, input, f);
    try std.testing.expectEqual(@as(i64, expected), result);
}

test "part 1 example" {
    try check(data.example, part1, 114);
}

test "part 1 real" {
    try check(data.real, part1, 1684566095);
}

test "part 2 example" {
    try check(data.example, part2, 2);
}

test "part 2 real" {
    try check(data.real, part2, 1136);
}

pub fn main() !void {
    const result1 = try solve(std.heap.page_allocator, data.real, part1);
    std.debug.print("Part 1: {d}\n", .{result1});

    const result2 = try solve(std.heap.page_allocator, data.real, part2);
    std.debug.print("Part 2: {d}\n", .{result2});
}
