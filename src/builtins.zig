const std = @import("std");
const vals = @import("values.zig");
const Value = vals.Value;
const Manager = @import("memory.zig").Manager;
const NativeError = @import("Object.zig").NativeError;

const log = std.log.scoped(.builtins);

pub fn clock(arg_count: u8, _: [*]Value, _: *Manager) NativeError!Value {
    std.debug.assert(arg_count == 0);
    return .{
        .number = @floatFromInt(std.time.timestamp()),
    };
}

pub fn println(arg_count: u8, values: [*]Value, _: *Manager) NativeError!Value {
    std.debug.assert(arg_count == 1);
    const val = values[0];
    log.info("{any}\n", .{val});
    return vals.NIL_VAL;
}

fn floatToString(f: f64, manager: *Manager) !Value {
    var buf: [64]u8 = undefined;
    const str = std.fmt.bufPrint(buf[0..], "{d}", .{f}) catch unreachable;
    const str_obj = try manager.copy(str);
    return .{ .object = str_obj };
}

fn constToString(content: []const u8, manager: *Manager) !Value {
    const str = try manager.copy(content);
    return .{ .object = str };
}

pub fn toString(arg_count: u8, values: [*]Value, manager: *Manager) NativeError!Value {
    if (arg_count != 1) return error.WrongArity;
    const result = switch (values[0]) {
        .number => |i| try floatToString(i, manager),
        .boolean => |b| try constToString(if (b) "true" else "false", manager),
        .nil => try constToString("nil", manager),
        else => error.BadValue,
    };
    return result;
}
