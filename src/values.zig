const std = @import("std");

pub const Value = f64;

pub const ValueArray = std.ArrayListUnmanaged(Value);

pub fn print(value: Value, printer: *const fn (comptime []const u8, anytype) void) void {
    printer("{d}", .{value});
}
