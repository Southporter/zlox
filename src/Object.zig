const std = @import("std");
const Object = @This();
const assert = std.debug.assert;
const Chunk = @import("Chunk.zig");
const values = @import("values.zig");
const Value = values.Value;
const Manager = @import("memory.zig").Manager;

pub const ObjectType = enum {
    string,
    function,
    native,
    closure,
    upvalue,
};

tag: ObjectType,
next: ?*Object = null,
is_marked: bool = false,

pub fn format(object: Object, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;

    try writer.print("Object{{tag = {s}, has_next: {}, is_marked: {} }}", .{ @tagName(object.tag), object.next != null, object.is_marked });
}

pub fn equal(a: *Object, b: *Object) bool {
    if (a.tag != b.tag) return false;
    switch (a.tag) {
        .string => {
            const str_a = a.asString();
            const str_b = b.asString();
            return std.mem.eql(u8, str_a.data, str_b.data);
        },
        .function => {
            const fun_a = a.asFunction();
            const fun_b = b.asFunction();
            return fun_a.arity == fun_b.arity and std.mem.eql(u8, fun_a.name.data, fun_b.name.data);
        },
        .native => {
            return a.asNative().function == b.as(Native).function;
        },
        .closure => {
            return false;
        },
    }
}

pub fn as(object: *Object, comptime T: type) *T {
    const tag_name = @tagName(object.tag);
    const type_name = @typeName(T);
    assert(std.ascii.eqlIgnoreCase(tag_name, type_name[type_name.len - tag_name.len ..]));
    return @alignCast(@fieldParentPtr("object", object));
}
pub fn asString(object: *Object) *String {
    assert(object.tag == .string);
    return object.as(String);
}

pub fn asFunction(object: *Object) *Function {
    assert(object.tag == .function);
    return @alignCast(@fieldParentPtr("object", object));
}
pub fn asNative(object: *Object) *Native {
    assert(object.tag == .native);
    return @alignCast(@fieldParentPtr("object", object));
}

pub fn deinit(object: *Object, manager: *Manager) void {
    switch (object.tag) {
        .string => {
            const str = object.as(String);
            str.deinit(manager);
            manager.destroy(str);
        },
        .function => {
            const fun = object.as(Function);
            fun.deinit();
            manager.destroy(fun);
        },
        .native => {
            const native = object.as(Native);
            manager.destroy(native);
        },
        .closure => {
            const closure = object.as(Closure);
            closure.deinit(manager);
            manager.destroy(closure);
        },
        .upvalue => {
            const upvalue = object.as(Upvalue);
            manager.destroy(upvalue);
        },
    }
}

pub const String = struct {
    object: Object,
    data: []const u8,
    hash: u32,

    pub fn deinit(self: *String, manager: *Manager) void {
        manager.free(self.data);
    }

    pub fn format(string: String, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("String{{has_next: {}, is_marked: {}, hash: {d} }}", .{ string.object.next != null, string.object.is_marked, string.hash });
    }

    pub fn from(raw: []const u8) String {
        return .{
            .object = .{
                .tag = .string,
                .next = null,
            },
            .data = raw,
            .hash = hashString(raw),
        };
    }
    pub fn fromAlloc(raw: []const u8, allocator: *Manager) !*String {
        var new = try allocator.create(String);
        new.object.tag = .string;
        new.object.next = null;
        new.hash = hashString(raw);
        new.data = raw;
        return new;
    }

    pub fn copy(original: []const u8, allocator: *Manager) !*Object {
        var str = try allocator.create(String);
        str.object.tag = .string;
        str.object.next = null;
        str.data = try allocator.dupe(u8, original);
        str.hash = hashString(original);
        return &str.object;
    }
};

pub fn hashString(str: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (str) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}

pub const Function = struct {
    object: Object,
    arity: usize,
    upvalue_count: usize,
    chunk: Chunk,
    name: ?*String,

    pub fn init(allocator: *Manager) !*Function {
        var fun = try allocator.create(Function);
        fun.object = .{
            .tag = .function,
        };
        fun.arity = 0;
        fun.upvalue_count = 0;
        fun.chunk = try Chunk.init(allocator.inner());
        fun.name = null;
        return fun;
    }

    pub fn deinit(fun: *Function) void {
        fun.chunk.deinit();
    }
    pub fn format(function: Function, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        const has_next = function.object.next != null;

        try writer.print("Function[has_next: {any}, is_marked: {any}, arity: {d}, upvalue_count: {d}, name: {s}]", .{
            has_next, function.object.is_marked, function.arity, function.upvalue_count, function.name,
        });
    }
};

pub const NativeError = error{ WrongArity, BadValue } || Manager.Error;
pub const NativeFn = *const fn (u8, [*]Value, *Manager) NativeError!Value;

pub const Native = struct {
    object: Object,
    function: NativeFn,

    pub fn init(allocator: *Manager) !*Native {
        var native = try allocator.create(Native);
        native.object = .{
            .tag = .native,
        };
        return native;
    }

    pub fn deinit(_: *Native) void {}
    pub fn format(native: Native, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("Native{{has_next: {}, is_marked: {}  }}", .{ native.object.next != null, native.object.is_marked });
    }
};

pub const Closure = struct {
    object: Object,
    function: *Function,
    upvalues: []?*Upvalue,

    pub fn init(allocator: *Manager, function: *Function) !*Closure {
        var closure = try allocator.create(Closure);
        closure.object = .{
            .tag = .closure,
        };
        closure.function = function;
        closure.upvalues = try allocator.alloc(?*Upvalue, function.upvalue_count);
        for (closure.upvalues) |*upvalues| {
            upvalues.* = null;
        }
        return closure;
    }

    pub fn deinit(closure: *Closure, allocator: *Manager) void {
        allocator.free(closure.upvalues);
    }

    pub fn format(closure: Closure, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("Closure{{has_next: {}, is_marked: {}, function: {any} }}", .{ closure.object.next != null, closure.object.is_marked, closure.function });
    }
};

pub const Upvalue = struct {
    object: Object = .{
        .tag = .upvalue,
    },
    location: *Value,
    closed: Value,
    next: ?*Upvalue = null,

    pub fn init(allocator: *Manager, slot: *Value) !*Upvalue {
        var upvalue = try allocator.create(Upvalue);
        upvalue.object = .{ .tag = .upvalue };
        upvalue.location = slot;
        upvalue.next = null;
        upvalue.closed = values.NIL_VAL;
        return upvalue;
    }

    pub fn format(upvalue: Upvalue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("Upvalue{{has_next: {}, is_marked: {}, value: {any} }}", .{ upvalue.object.next != null, upvalue.object.is_marked, upvalue.closed });
    }
};
