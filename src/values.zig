const std = @import("std");
const Object = @import("Object.zig");

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
    pub fn equal(a: *const Value, b: Value) bool {
        if (std.meta.activeTag(a.*) != std.meta.activeTag(b)) return false;
        return switch (a.*) {
            .nil => true,
            .boolean => |val| val == b.boolean,
            .number => |val| val == b.number,
            .object => |obj| obj == b.object,
        };
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
        .string => printer("\"{s}\"", .{object.asString().data}),
        .function => {
            const fun = object.asFunction();
            if (fun.name) |name| {
                printer("<fn {s}>", .{name.data});
            } else {
                printer("<script>", .{});
            }
        },
    }
}
