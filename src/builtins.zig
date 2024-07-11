const std = @import("std");
const Value = @import("values.zig").Value;

pub fn clock(arg_count: u8, _: [*]Value) Value {
    std.debug.assert(arg_count == 0);
    return .{
        .number = @floatFromInt(std.time.timestamp()),
    };
}
