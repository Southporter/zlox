const std = @import("std");
const Object = @This();
const assert = std.debug.assert;

pub const ObjectType = enum {
    string,
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
    }
}
pub fn asString(object: *Object) *String {
    assert(object.tag == .string);
    return @alignCast(@fieldParentPtr("object", object));
}

pub fn deinit(object: *Object, allocator: std.mem.Allocator) void {
    switch (object.tag) {
        .string => {
            const str = object.asString();
            allocator.free(str.data);
            allocator.destroy(str);
        },
    }
}

pub const String = struct {
    object: Object,
    data: []u8,

    pub fn copy(original: []const u8, allocator: std.mem.Allocator) !*Object {
        var str = try allocator.create(String);
        str.object.tag = .string;
        str.data = try allocator.dupe(u8, original);
        return &str.object;
    }

    pub fn concat(a: *const String, b: *const String, allocator: std.mem.Allocator) !*Object {
        var new = try allocator.create(String);
        new.object.tag = .string;
        new.data = try allocator.alloc(u8, a.data.len + b.data.len);
        @memcpy(new.data[0..a.data.len], a.data);
        @memcpy(new.data[a.data.len..], b.data);
        return &new.object;
    }
};
