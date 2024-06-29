const std = @import("std");
const Chunk = @import("Chunk.zig");

const log = std.log.scoped(.debugger);

pub fn disassembleChunk(chunk: *Chunk, name: []const u8, writer: anytype) !void {
    try writer.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count) : (offset = try disassembleInstruction(chunk, offset, writer)) {}
}

fn disassembleInstruction(chunk: *Chunk, offset: usize, writer: anytype) !usize {
    try writer.print("{d:0>4} ", .{offset});

    return switch (@as(Chunk.Opcode, @enumFromInt(chunk.code[offset]))) {
        .@"return" => simpleInstruction("OP_RETURN", offset, writer),
    };
}

fn simpleInstruction(name: []const u8, offset: usize, writer: anytype) !usize {
    try writer.print("{s}\n", .{name});
    return offset + 1;
}

test "Simple dissassembly" {
    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    try chunk.write(0);

    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var writer = stream.writer();

    try disassembleChunk(&chunk, "test", &writer);
    try std.testing.expectEqualSlices(u8, buf[0..(11 + 5 + 10)],
        \\== test ==
        \\0000 OP_RETURN
        \\
    );
}
