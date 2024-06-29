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
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        _ = try writer.write("   | ");
    } else {
        try writer.print("{d:>4} ", .{chunk.lines.items[offset]});
    }

    return switch (@as(Chunk.Opcode, @enumFromInt(chunk.code[offset]))) {
        .@"return" => simpleInstruction("OP_RETURN", offset, writer),
        .constant => constantInstruction(chunk, offset, writer),
    };
}

fn simpleInstruction(name: []const u8, offset: usize, writer: anytype) !usize {
    try writer.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(chunk: *Chunk, offset: usize, writer: anytype) !usize {
    const index = chunk.code[offset + 1];
    const constant = chunk.constants.items[index];
    try writer.print("{s:<16} {d:>4} {any}\n", .{ "OP_CONSTANT", index, constant });
    return offset + 2;
}

test "Simple dissassembly" {
    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    try chunk.write(0, 123);
    try chunk.write(1, 123);
    try chunk.write(0, 123);
    try chunk.addConstant(3.14);

    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var writer = stream.writer();

    try disassembleChunk(&chunk, "test", &writer);

    const output =
        \\== test ==
        \\0000  123 OP_RETURN
        \\0001    | OP_CONSTANT         0 3.14e0
        \\
    ;
    try std.testing.expectEqualSlices(u8, output, buf[0..output.len]);
}
