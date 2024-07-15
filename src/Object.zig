const std = @import("std");
const Object = @This();
const assert = std.debug.assert;
const Chunk = @import("Chunk.zig");
const values = @import("values.zig");
const Value = values.Value;

pub const ObjectType = enum {
    string,
    function,
    native,
    closure,
    upvalue,
};

tag: ObjectType,
next: ?*Object = null,

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
    }
}

pub const String = struct {
    object: Object,
    data: []const u8,
    hash: u32,

    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
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
};

pub const NativeFn = *const fn (u8, [*]Value) Value;

pub const Native = struct {
    object: Object,
    function: NativeFn,

    pub fn init(allocator: std.mem.Allocator, function: NativeFn) !*Native {
        var native = try allocator.create(Native);
        native.object = .{
            .tag = .native,
        };
        native.function = function;

        return native;
    }

    pub fn deinit(_: *Native) void {}
};

pub const Closure = struct {
    object: Object,
    function: *Function,
    upvalues: []?*Upvalue,

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
};

pub const Upvalue = struct {
    object: Object = .{
        .tag = .upvalue,
    },
    location: *Value,
    closed: Value,
    next: ?*Upvalue = null,

    pub fn init(allocator: std.mem.Allocator, slot: *Value) !*Upvalue {
        var upvalue = try allocator.create(Upvalue);
        upvalue.object = .{ .tag = .upvalue };
        upvalue.location = slot;
        upvalue.next = null;
        upvalue.closed = values.NIL_VAL;
        return upvalue;
    }
};
