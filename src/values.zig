const std = @import("std");
const config = @import("config");
const Object = @import("Object.zig");
const debug = @import("debug.zig");

const TAG_NIL = 1;
const TAG_FALSE = 2;
const TAG_TRUE = 3;

pub const TRUE_VAL = if (config.nan_tagging) @as(Value, QUIET_NAN | TAG_TRUE) else Value{ .boolean = true };
pub const FALSE_VAL = if (config.nan_tagging) @as(Value, QUIET_NAN | TAG_FALSE) else Value{ .boolean = false };
pub const NIL_VAL = if (config.nan_tagging) @as(Value, QUIET_NAN | TAG_NIL) else Value{ .nil = {} };
pub const ValueTag = enum {
    nil,
    boolean,
    number,
    object,
};
const QUIET_NAN: u64 = 0x7ffc000000000000;
const SIGN_BIT: u64 = 0x8000000000000000;

pub fn numToValue(number: f64) Value {
    return if (config.nan_tagging) @bitCast(number) else .{
        .number = number,
    };
}
pub fn valueToNum(value: Value) f64 {
    return if (config.nan_tagging) @bitCast(value) else value.number;
}

pub fn asNumber(value: Value) f64 {
    return valueToNum(value);
}
pub fn isNumber(value: Value) bool {
    if (config.nan_tagging) {
        return value & QUIET_NAN != QUIET_NAN;
    } else {
        return switch (value) {
            .number => true,
            else => false,
        };
    }
}

pub fn isNil(value: Value) bool {
    if (config.nan_tagging) {
        return value == NIL_VAL;
    } else {
        return switch (value) {
            .nil => true,
            else => false,
        };
    }
}
pub fn asBool(value: Value) bool {
    if (config.nan_tagging) {
        return value == TRUE_VAL;
    } else {
        return value.boolean;
    }
}
pub fn boolToValue(b: bool) Value {
    return if (b) TRUE_VAL else FALSE_VAL;
}
pub fn isBool(value: Value) bool {
    if (config.nan_tagging) {
        return (value | 1) == TRUE_VAL;
    } else {
        return switch (value) {
            .boolean => true,
            else => false,
        };
    }
}

pub fn isObject(value: Value) bool {
    if (config.nan_tagging) {
        return (value & (QUIET_NAN | SIGN_BIT)) == (QUIET_NAN | SIGN_BIT);
    } else {
        return switch (value) {
            .object => true,
            else => false,
        };
    }
}
pub fn asObject(value: Value) *Object {
    if (config.nan_tagging) {
        const address = value & ~(SIGN_BIT | QUIET_NAN);
        return @ptrFromInt(address);
    } else {
        return value.object;
    }
}
pub fn objectToValue(obj: *Object) Value {
    if (config.nan_tagging) {
        return SIGN_BIT | QUIET_NAN | @intFromPtr(obj);
    } else {
        return .{ .object = obj };
    }
}
pub fn isObjectType(value: Value, tag: Object.ObjectType) bool {
    return isObject(value) and asObject(value).tag == tag;
}

pub fn isFalsey(value: Value) bool {
    if (isNil(value)) {
        return true;
    } else if (isBool(value)) {
        return if (asBool(value)) false else true;
    } else {
        return false;
    }
}
pub fn unwrap(value: Value) Value {
    return switch (value) {
        .object => |obj| switch (obj.tag) {
            .upvalue => obj.as(Object.Upvalue).closed,
            else => value,
        },
        else => |val| val,
    };
}
pub fn equal(a: Value, b: Value) bool {
    if (config.nan_tagging) {
        if (isNumber(a) and isNumber(b)) {
            return valueToNum(a) == valueToNum(b);
        }
        return a == b;
    } else {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .nil => true,
            .boolean => |val| val == b.boolean,
            .number => |val| val == b.number,
            .object => |obj| obj == b.object,
        };
    }
}

pub const Value = if (config.nan_tagging) u64 else union(ValueTag) {
    nil: void,
    boolean: bool,
    number: f64,
    object: *Object,

    pub fn format(value: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try debug.printValue(value, writer);
    }
};

pub const ValueArray = std.ArrayListUnmanaged(Value);

const Printer = *const fn (comptime []const u8, anytype) void;

pub fn print(value: Value, printer: Printer) void {
    if (config.nan_tagging) {
        if (isBool(value)) {
            printer("{}", .{asBool(value)});
        } else if (isNil(value)) {
            printer("nil", .{});
        } else if (isNumber(value)) {
            printer("{d}", .{valueToNum(value)});
        } else if (isObject(value)) {
            printObject(asObject(value), printer);
        }
    } else {
        switch (value) {
            .number => printer("{d}", .{value.number}),
            .boolean => printer("{}", .{value.boolean}),
            .nil => printer("nil", .{}),
            .object => printObject(value.object, printer),
        }
    }
}
pub fn write(value: Value, writer: anytype) anyerror!void {
    if (config.nan_tagging) {
        if (isBool(value)) {
            try writer.print("{}", .{asBool(value)});
        } else if (isNil(value)) {
            _ = try writer.write("nil");
        } else if (isNumber(value)) {
            try writer.print("{d}", .{valueToNum(value)});
        } else if (isObject(value)) {
            try writeObject(asObject(value), writer);
        }
    } else {
        switch (value) {
            .number => |val| try writer.print("{any}", .{val}),
            .boolean => |val| try writer.print("{}", .{val}),
            .nil => _ = try writer.write("nil"),
            .object => |obj| try writeObject(obj, writer),
        }
    }
}

pub fn printObject(object: *Object, printer: Printer) void {
    switch (object.tag) {
        .string => printer("{s}", .{object.as(Object.String).data}),
        .function => {
            const fun = object.as(Object.Function);
            if (fun.name) |name| {
                printer("<fn {s}>", .{name.data});
            } else {
                printer("<script>", .{});
            }
        },
        .native => {
            printer("<fn native>", .{});
        },
        .closure => {
            printer("<closure>", .{});
        },
        .upvalue => {
            printer("upvalue ", .{});
            print(object.as(Object.Upvalue).location.*, printer);
            // printer("", .{});
        },
        .class => {
            printer("{s}", .{object.as(Object.Class).name});
        },
        .instance => {
            printer("{s} instance", .{object.as(Object.Instance).class.name});
        },
        .bound_method => {
            printObject(&object.as(Object.BoundMethod).method.function.object, printer);
        },
    }
}
pub fn writeObject(object: *Object, writer: anytype) anyerror!void {
    switch (object.tag) {
        .string => try writer.print("{s}", .{object.as(Object.String).data}),
        .function => {
            const fun = object.as(Object.Function);
            if (fun.name) |name| {
                try writer.print("<fn {s}>", .{name.data});
            } else {
                try writer.print("<script>", .{});
            }
        },
        .native => {
            try writer.print("<fn native>", .{});
        },
        .closure => {
            try writer.print("<closure>", .{});
        },
        .upvalue => {
            try writer.print("upvalue ", .{});
            try write(object.as(Object.Upvalue).location.*, writer);
            // printer("", .{});
        },
        .class => {
            try writer.print("{s}", .{object.as(Object.Class).name});
        },
        .instance => {
            try writer.print("{s} instance", .{object.as(Object.Instance).class.name});
        },
        .bound_method => {
            try writeObject(&object.as(Object.BoundMethod).method.function.object, writer);
        },
    }
}
