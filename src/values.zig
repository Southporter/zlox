const std = @import("std");
const Object = @import("Object.zig");
const debug = @import("debug.zig");

pub const TRUE_VAL = Value{ .boolean = true };
pub const FALSE_VAL = Value{ .boolean = false };
pub const NIL_VAL = Value{ .nil = {} };
pub const ValueTag = enum {
    nil,
    boolean,
    number,
    object,
};
pub const Value = union(ValueTag) {
    nil: void,
    boolean: bool,
    number: f64,
    object: *Object,

    pub fn isNumber(value: *const Value) bool {
        return switch (value.*) {
            .number => true,
            else => false,
        };
    }
    pub fn isBoolean(value: *const Value) bool {
        return switch (value.*) {
            .boolean => true,
            else => false,
        };
    }
    pub fn isFalsey(value: *const Value) Value {
        return switch (value.*) {
            .nil => TRUE_VAL,
            .boolean => |val| if (val) FALSE_VAL else TRUE_VAL,
            else => FALSE_VAL,
        };
    }
    pub fn isString(value: *const Value) bool {
        return switch (value.*) {
            .object => |obj| obj.tag == .string,
            else => false,
        };
    }

    pub fn isInstance(value: *const Value) bool {
        return switch (value.*) {
            .object => |obj| obj.tag == .instance,
            else => false,
        };
    }
    pub fn unwrap(value: *const Value) Value {
        return switch (value.*) {
            .object => |obj| switch (obj.tag) {
                .upvalue => obj.as(Object.Upvalue).closed,
                else => value.*,
            },
            else => |val| val,
        };
    }
    pub fn equal(a: *const Value, b: Value) bool {
        if (std.meta.activeTag(a.*) != std.meta.activeTag(b)) return false;
        return switch (a.*) {
            .nil => true,
            .boolean => |val| val == b.boolean,
            .number => |val| val == b.number,
            .object => |obj| obj == b.object,
        };
    }

    pub fn format(value: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try debug.printValue(value, writer);
    }
};

pub const ValueArray = std.ArrayListUnmanaged(Value);

const Printer = *const fn (comptime []const u8, anytype) void;

pub fn print(value: Value, printer: Printer) void {
    switch (value) {
        .number => printer("{d}", .{value.number}),
        .boolean => printer("{}", .{value.boolean}),
        .nil => printer("nil", .{}),
        .object => printObject(value.object, printer),
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
    }
}
