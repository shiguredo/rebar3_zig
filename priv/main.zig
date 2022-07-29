const std = @import("std");

pub fn add(x: c_int, y: c_int) c_int {
    return x + y;
}

test "add" {
    try std.testing.expectEqual(add(1, 2), 3);
}
