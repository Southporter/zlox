const std = @import("std");
const Chunk = @import("Chunk.zig");

const log = std.log.scoped(.debugger);

pub const LogWriter = struct {
    pub fn print(_: *const LogWriter, comptime format: []const u8, args: anytype) !void {
        log.debug(format, args);
    }

    pub fn write(_: *const LogWriter, comptime out: []const u8) !void {
        log.debug(out, .{});
    }
};

pub const Writer = LogWriter{};

pub fn disassembleChunk(chunk: *Chunk, name: []const u8, writer: anytype) !void {
    try writer.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count) : (offset = try disassembleInstruction(chunk, offset, writer)) {}
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize, writer: anytype) !usize {
    try writer.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        _ = try writer.write("   | ");
    } else {
        try writer.print("{d:>4} ", .{chunk.lines.items[offset]});
    }

    return switch (@as(Chunk.Opcode, @enumFromInt(chunk.code[offset]))) {
        .@"return" => simpleInstruction("OP_RETURN", offset, writer),
        .constant => constantInstruction("OP_CONSTANT", chunk, offset, writer),
        .define_global => constantInstruction("OP_DEFINE_GLOBAL", chunk, offset, writer),
        .get_global => constantInstruction("OP_GET_GLOBAL", chunk, offset, writer),
        .set_global => constantInstruction("OP_SET_GLOBAL", chunk, offset, writer),
        .print => simpleInstruction("OP_PRINT", offset, writer),
        .pop => simpleInstruction("OP_POP", offset, writer),
        .true => simpleInstruction("OP_TRUE", offset, writer),
        .false => simpleInstruction("OP_FALSE", offset, writer),
        .nil => simpleInstruction("OP_NIL", offset, writer),
        .negate => simpleInstruction("OP_NEGATE", offset, writer),
        .not => simpleInstruction("OP_NOT", offset, writer),
        .equal => simpleInstruction("OP_EQUAL", offset, writer),
        .greater => simpleInstruction("OP_GREATER", offset, writer),
        .less => simpleInstruction("OP_LESS", offset, writer),
        .add => simpleInstruction("OP_ADD", offset, writer),
        .subtract => simpleInstruction("OP_SUBTRACT", offset, writer),
        .multiply => simpleInstruction("OP_MULTIPLY", offset, writer),
        .divide => simpleInstruction("OP_DIVIDE", offset, writer),
    };
}

fn simpleInstruction(name: []const u8, offset: usize, writer: anytype) !usize {
    try writer.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize, writer: anytype) !usize {
    const index = chunk.code[offset + 1];
    const constant = chunk.constants.items[index];
    try writer.print("{s:<16} {d:>4} ", .{ name, index });
    switch (constant) {
        .number => |val| try writer.print("{any}\n", .{val}),
        .boolean => |val| try writer.print("{}\n", .{val}),
        .nil => _ = try writer.write("nil\n"),
        .object => |obj| {
            switch (obj.tag) {
                .string => try writer.print("{s}\n", .{obj.asString().data}),
            }
        },
    }
    return offset + 2;
}

test "Simple dissassembly" {
    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    try chunk.writeOp(.@"return", 123);
    try chunk.writeOp(.constant, 123);
    const pi_index = try chunk.addConstant(.{ .number = 3.14 });
    try chunk.write(pi_index, 123);
    try chunk.writeOp(.negate, 124);
    try chunk.writeOp(.add, 124);
    try chunk.writeOp(.subtract, 124);
    try chunk.writeOp(.multiply, 124);
    try chunk.writeOp(.divide, 124);
    try chunk.writeOp(.nil, 125);
    try chunk.writeOp(.true, 125);
    try chunk.writeOp(.false, 125);
    try chunk.writeOp(.equal, 125);
    try chunk.writeOp(.greater, 125);
    try chunk.writeOp(.less, 125);
    try chunk.writeOp(.define_global, 126);
    const answer_index = try chunk.addConstant(.{ .number = 42 });
    try chunk.write(answer_index, 126);

    var buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var writer = stream.writer();

    try disassembleChunk(&chunk, "test", &writer);

    const output =
        \\== test ==
        \\0000  123 OP_RETURN
        \\0001    | OP_CONSTANT         0 3.14e0
        \\0003  124 OP_NEGATE
        \\0004    | OP_ADD
        \\0005    | OP_SUBTRACT
        \\0006    | OP_MULTIPLY
        \\0007    | OP_DIVIDE
        \\0008  125 OP_NIL
        \\0009    | OP_TRUE
        \\0010    | OP_FALSE
        \\0011    | OP_EQUAL
        \\0012    | OP_GREATER
        \\0013    | OP_LESS
        \\0014  126 OP_DEFINE_GLOBAL    1 4.2e1
        \\
    ;
    try std.testing.expectEqualSlices(u8, output, buf[0..output.len]);
}
