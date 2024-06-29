const std = @import("std");
const testing = std.testing;

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test {
    _ = @import("Chunk.zig");
    _ = @import("VM.zig");
    _ = @import("debug.zig");
}
