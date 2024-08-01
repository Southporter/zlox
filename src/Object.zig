const std = @import("std");
const Object = @This();
const assert = std.debug.assert;
const Chunk = @import("Chunk.zig");
const values = @import("values.zig");
const Value = values.Value;
const Manager = @import("memory.zig").Manager;
const Table = @import("Table.zig");

pub const ObjectType = enum {
    string,
    function,
    native,
    closure,
    upvalue,
    class,
    instance,
    bound_method,
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
            const str_a = a.as(String);
            const str_b = b.as(String);
            return std.mem.eql(u8, str_a.data, str_b.data);
        },
        .function => {
            const fun_a = a.as(Function);
            const fun_b = b.as(Function);
            return fun_a.arity == fun_b.arity and std.mem.eql(u8, fun_a.name.data, fun_b.name.data);
        },
        .native => {
            return a.as(Native).function == b.as(Native).function;
        },
        .closure, .class, .instance, .bound_method => {
            return a == b;
        },
    }
}

pub fn as(object: *Object, comptime T: type) *T {
    assert(object.tag == T.tag());
    return @alignCast(@fieldParentPtr("object", object));
}

pub fn deinit(object: *Object, allocator: std.mem.Allocator) void {
    switch (object.tag) {
        .string => {
            const str = object.as(String);
            str.deinit(allocator);
            allocator.destroy(str);
        },
        .function => {
            const fun = object.as(Function);
            fun.deinit();
            allocator.destroy(fun);
        },
        .native => {
            const native = object.as(Native);
            allocator.destroy(native);
        },
        .closure => {
            const closure = object.as(Closure);
            closure.deinit(allocator);
            allocator.destroy(closure);
        },
        .upvalue => {
            const upvalue = object.as(Upvalue);
            allocator.destroy(upvalue);
        },
        .class => {
            const class = object.as(Class);
            class.deinit(allocator);
            allocator.destroy(class);
        },
        .instance => {
            const inst = object.as(Instance);
            inst.deinit(allocator);
            allocator.destroy(inst);
        },
        .bound_method => {
            const bound = object.as(BoundMethod);
            bound.deinit(allocator);
            allocator.destroy(bound);
        },
    }
}

pub const String = struct {
    object: Object,
    data: []const u8,
    hash: u32,

    pub fn tag() ObjectType {
        return .string;
    }

    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }

    pub fn format(string: String, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{string.data});
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
    pub fn fromAlloc(raw: []const u8, allocator: std.mem.Allocator) !*String {
        var new = try allocator.create(String);
        new.object.tag = .string;
        new.object.next = null;
        new.hash = hashString(raw);
        new.data = raw;
        return new;
    }

    pub fn copy(original: []const u8, allocator: std.mem.Allocator) !*Object {
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

    pub fn tag() ObjectType {
        return .function;
    }
    pub fn init(allocator: std.mem.Allocator) !*Function {
        var fun = try allocator.create(Function);
        fun.object = .{
            .tag = .function,
        };
        fun.arity = 0;
        fun.upvalue_count = 0;
        fun.chunk = try Chunk.init(allocator);
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
            has_next, function.object.is_marked, function.arity, function.upvalue_count, if (function.name) |name| name.data else "script",
        });
    }
};

pub const NativeError = error{ WrongArity, BadValue } || Manager.Error;
pub const NativeFn = *const fn (u8, [*]Value, *Manager) NativeError!Value;

pub const Native = struct {
    object: Object,
    function: NativeFn,

    pub fn tag() ObjectType {
        return .native;
    }

    pub fn init(allocator: std.mem.Allocator) !*Native {
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

    pub fn tag() ObjectType {
        return .closure;
    }

    pub fn init(allocator: std.mem.Allocator, function: *Function) !*Closure {
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

    pub fn deinit(closure: *Closure, allocator: std.mem.Allocator) void {
        allocator.free(closure.upvalues);
    }

    pub fn format(closure: Closure, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("Closure[has_next: {any}, is_marked: {any}, function: {s} ]", .{
            closure.object.next != null,
            closure.object.is_marked,
            if (closure.function.name) |name| name.data else "script",
        });
    }
};

pub const Upvalue = struct {
    object: Object = .{
        .tag = .upvalue,
    },
    location: *Value,
    closed: Value,
    next: ?*Upvalue = null,

    pub fn tag() ObjectType {
        return .upvalue;
    }
    pub fn init(allocator: std.mem.Allocator, slot: *Value) !*Upvalue {
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

pub const Class = struct {
    object: Object = .{
        .tag = .class,
    },
    name: *String,
    methods: Table,
    pub fn tag() ObjectType {
        return .class;
    }

    pub fn init(allocator: std.mem.Allocator, name: *String) !*Class {
        var class = try allocator.create(Class);
        class.object = .{ .tag = .class };
        class.name = name;
        class.methods = try Table.init(allocator);
        return class;
    }

    pub fn deinit(class: *Class, allocator: std.mem.Allocator) void {
        _ = allocator;
        class.methods.deinit();
    }

    pub fn format(class: Class, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}{{ }}", .{class.name.data});
    }
};

pub const Instance = struct {
    object: Object = .{
        .tag = .instance,
    },
    class: *Class,
    fields: Table,

    pub fn tag() ObjectType {
        return .instance;
    }

    pub fn init(allocator: std.mem.Allocator, class: *Class) !*Instance {
        var instance = try allocator.create(Instance);
        instance.object = .{ .tag = .instance };
        instance.class = class;
        instance.fields = try Table.init(allocator);

        return instance;
    }

    pub fn deinit(instance: *Instance, _: std.mem.Allocator) void {
        instance.fields.deinit();
    }

    pub fn format(instance: Instance, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} instance {{ }}", .{instance.class.name.data});
    }
};

pub const BoundMethod = struct {
    object: Object = .{
        .tag = .bound_method,
    },
    receiver: Value,
    method: *Closure,

    pub fn tag() ObjectType {
        return .bound_method;
    }

    pub fn init(allocator: std.mem.Allocator, receiver: Value, method: *Closure) !*BoundMethod {
        var bound = try allocator.create(BoundMethod);
        bound.object = .{ .tag = .bound_method };
        bound.receiver = receiver;
        bound.method = method;

        return bound;
    }

    pub fn deinit(_: *BoundMethod, _: std.mem.Allocator) void {}
};
