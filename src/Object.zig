const std = @import("std");
const Object = @This();
const assert = std.debug.assert;
const Chunk = @import("Chunk.zig");
const Value = @import("values.zig").Value;

pub const ObjectType = enum {
    string,
    function,
    native,
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
    }
}
pub fn asString(object: *Object) *String {
    assert(object.tag == .string);
    return @alignCast(@fieldParentPtr("object", object));
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
            const str = object.asString();
            str.deinit(allocator);
            allocator.destroy(str);
        },
        .function => {
            const fun = object.asFunction();
            fun.deinit();
            allocator.destroy(fun);
        },
        .native => {
            const native = object.asNative();
            allocator.destroy(native);
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

    pub fn from(raw: []const u8) !String {
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
    chunk: Chunk,
    name: ?*String,

    pub fn init(allocator: std.mem.Allocator) !*Function {
        var fun = try allocator.create(Function);
        fun.object = .{
            .tag = .function,
        };
        fun.arity = 0;
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
