const std = @import("std");
pub const Y2023D09 = @import("advent_2023_09.zig");

test {
    std.testing.refAllDecls(@This());
}

pub fn main() !void {
    std.debug.print("Run `zig build test` instead\n", .{});
}
