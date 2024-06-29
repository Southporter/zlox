const std = @import("std");

const Chunk = @This();
const values = @import("values.zig");
const ValueArray = values.ValueArray;
const Value = values.Value;

pub const Opcode = enum {
    @"return",
    constant,
};

code: []u8,
count: usize = 0,
constants: ValueArray,
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) !Chunk {
    return .{
        .code = try allocator.alloc(u8, 8),
        .constants = ValueArray.init(allocator),
        .allocator = allocator,
    };
}
pub fn deinit(chunk: *Chunk) void {
    chunk.constants.deinit();
    chunk.allocator.free(chunk.code);
    chunk.count = 0;
}

pub fn write(chunk: *Chunk, byte: u8) !void {
    if (chunk.code.len < chunk.count + 1) {
        try chunk.grow();
    }
    chunk.code[chunk.count] = byte;
    chunk.count += 1;
}

test "Write and reallocate" {
    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    for (0..18) |i| {
        try chunk.write(@truncate(i));
    }
    try std.testing.expectEqual(chunk.count, 18);
    try std.testing.expectEqual(chunk.code.len, 32);
    try std.testing.expectEqualSlices(u8, &[18]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 }, chunk.code[0..chunk.count]);
}

fn grow(chunk: *Chunk) !void {
    const new_cap = if (chunk.code.len < 8) 8 else chunk.code.len * 2;
    chunk.code = try chunk.allocator.realloc(chunk.code, new_cap);
}

pub fn addConstant(chunk: *Chunk, value: Value) !void {
    try chunk.constants.append(value);
}
