const std = @import("std");
const Object = @This();
const assert = std.debug.assert;

pub const ObjectType = enum {
    string,
};

tag: ObjectType,

pub fn asString(object: *Object) *String {
    assert(object.tag == .string);
    return @alignCast(@fieldParentPtr("object", object));
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
};
