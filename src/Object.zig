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
            str.deinit(allocator);
            allocator.destroy(str);
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
            .object = .{ .tag = .string },
            .data = raw,
            .hash = hashString(raw),
        };
    }
    pub fn fromAlloc(raw: []const u8, allocator: std.mem.Allocator) !*String {
        var new = try allocator.create(String);
        new.object.tag = .string;
        new.hash = hashString(raw);
        new.data = raw;
        return new;
    }

    pub fn copy(original: []const u8, allocator: std.mem.Allocator) !*Object {
        var str = try allocator.create(String);
        str.object.tag = .string;
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
