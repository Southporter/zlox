const std = @import("std");

const Chunk = @This();
const values = @import("values.zig");
const ValueArray = values.ValueArray;
const Value = values.Value;

pub const Opcode = enum(u8) {
    @"return",
    print,
    pop,
    jump,
    jump_if_false,
    loop,
    call,
    closure,
    constant, //0x08
    define_global,
    get_global,
    set_global,
    get_local,
    set_local,
    get_upvalue,
    set_upvalue,
    close_upvalue, // 0x10
    nil,
    true,
    false,
    negate,
    not,
    equal,
    greater,
    less,
    add,
    subtract,
    multiply,
    divide,
};

code: []u8,
count: usize = 0,
constants: ValueArray,
lines: std.ArrayListUnmanaged(usize),
arena: std.heap.ArenaAllocator,

pub fn init(child_allocator: std.mem.Allocator) !Chunk {
    var arena = std.heap.ArenaAllocator.init(child_allocator);
    return .{
        .code = try arena.allocator().alloc(u8, 8),
        .constants = ValueArray{},
        .lines = std.ArrayListUnmanaged(usize){},
        .arena = arena,
    };
}
pub fn deinit(chunk: *Chunk) void {
    chunk.arena.deinit();
}

pub fn allocator(chunk: *Chunk) std.mem.Allocator {
    return chunk.arena.allocator();
}

pub fn writeOp(chunk: *Chunk, op: Opcode, line: usize) !void {
    return chunk.write(@intFromEnum(op), line);
}

pub fn write(chunk: *Chunk, byte: u8, line: usize) !void {
    if (chunk.code.len < chunk.count + 1) {
        try chunk.grow();
    }
    chunk.code[chunk.count] = byte;
    try chunk.lines.append(chunk.allocator(), line);
    chunk.count += 1;
}

test "Write and reallocate" {
    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    for (0..18) |i| {
        try chunk.write(@truncate(i), i / 10);
    }
    try std.testing.expectEqual(chunk.count, 18);
    try std.testing.expectEqual(chunk.code.len, 32);
    try std.testing.expectEqualSlices(u8, &[18]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 }, chunk.code[0..chunk.count]);
    try std.testing.expectEqualSlices(usize, &[18]usize{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1 }, chunk.lines.items[0..chunk.count]);
}

fn grow(chunk: *Chunk) !void {
    const new_cap = if (chunk.code.len < 8) 8 else chunk.code.len * 2;
    chunk.code = try chunk.allocator().realloc(chunk.code, new_cap);
}

pub fn addConstant(chunk: *Chunk, value: Value) !u8 {
    try chunk.constants.append(chunk.allocator(), value);
    const index = chunk.constants.items.len - 1;
    if (index > std.math.maxInt(u8)) {
        return error.ConstantOverflow;
    }
    return @intCast(index);
}

pub fn getConstant(chunk: *Chunk, index: usize) Value {
    return chunk.constants.items[index];
}
