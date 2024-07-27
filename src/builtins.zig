const std = @import("std");
const vals = @import("values.zig");
const Value = vals.Value;
const Manager = @import("memory.zig").Manager;
const NativeError = @import("Object.zig").NativeError;

const log = std.log.scoped(.builtins);

pub fn clock(arg_count: u8, _: [*]Value, _: *Manager) NativeError!Value {
    std.debug.assert(arg_count == 0);
    return vals.numToValue(@floatFromInt(std.time.timestamp()));
}

pub fn println(arg_count: u8, values: [*]Value, _: *Manager) NativeError!Value {
    std.debug.assert(arg_count == 1);
    const val = values[0];
    std.debug.print("{any}\n", .{val});
    return vals.NIL_VAL;
}

fn floatToString(f: f64, manager: *Manager) !Value {
    var buf: [64]u8 = undefined;
    const str = std.fmt.bufPrint(buf[0..], "{d}", .{f}) catch unreachable;
    const str_obj = try manager.copy(str);
    return vals.objectToValue(str_obj);
}

fn constToString(content: []const u8, manager: *Manager) !Value {
    const str = try manager.copy(content);
    return vals.objectToValue(str);
}

pub fn toString(arg_count: u8, values: [*]Value, manager: *Manager) NativeError!Value {
    if (arg_count != 1) return error.WrongArity;
    const val = values[0];
    if (vals.isNumber(val)) {
        return floatToString(vals.valueToNum(val), manager);
    }
    if (vals.isBool(val)) {
        return constToString(if (vals.asBool(val)) "true" else "false", manager);
    }
    if (vals.isNil(val)) {
        return constToString("nil", manager);
    }
    return error.BadValue;
}
