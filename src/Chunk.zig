const std = @import("std");

const Chunk = @This();

pub const Opcode = enum {
    @"return",
};

code: []u8 = undefined,
count: usize = 0,
capacity: usize,
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) !Chunk {
    return .{
        .code = try allocator.alloc(u8, 8),
        .capacity = 8,
        .allocator = allocator,
    };
}
pub fn deinit(chunk: *Chunk) void {
    chunk.allocator.free(chunk.code);
    chunk.count = 0;
    chunk.capacity = 0;
}

pub fn write(chunk: *Chunk, byte: u8) !void {
    if (chunk.capacity < chunk.count + 1) {
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
    try std.testing.expectEqual(chunk.capacity, 32);
    try std.testing.expectEqualSlices(u8, &[18]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 }, chunk.code[0..chunk.count]);
}

fn grow(chunk: *Chunk) !void {
    const old_cap = chunk.capacity;
    chunk.capacity = if (old_cap < 8) 8 else old_cap * 2;
    // const old_code = chunk.code;
    chunk.code = try chunk.allocator.realloc(chunk.code, chunk.capacity);
    // @memcpy(chunk.code[0..old_cap], old_code);
}
