const std = @import("std");
const log = std.log.scoped(.vm);

const VM = @This();
const Compiler = @import("Compiler.zig");
const Chunk = @import("Chunk.zig");
const Object = @import("Object.zig");
const values = @import("values.zig");
const Value = values.Value;
const debug = @import("debug.zig");
const StringPool = @import("StringPool.zig");
const Table = @import("Table.zig");

const STACK_MAX = 256 * FRAMES_MAX;
const FRAMES_MAX = 64;

stack: [STACK_MAX]Value = undefined,
stackTop: usize = 0,
allocator: std.mem.Allocator,
objects: ?*Object = null,
strings: StringPool,
globals: Table,
frames: [FRAMES_MAX]CallFrame,
frame_count: usize = 0,

const CallFrame = struct {
    function: *Object.Function,
    ip: [*]u8,
    slots: []Value,

    fn readByte(frame: *CallFrame) u8 {
        frame.ip += 1;
        return frame.ip[0];
    }
    fn readShort(frame: *CallFrame) u16 {
        var val: u16 = @as(u16, frame.ip[1]) << 8;
        val |= frame.ip[2];
        frame.ip += 2;
        return val;
    }
    fn readConstant(frame: *CallFrame) Value {
        const index = frame.readByte();
        return frame.function.chunk.getConstant(index);
    }
    fn printConstants(frame: *CallFrame) void {
        log.debug("           ", .{});
        for (frame.function.chunk.constants.items) |constant| {
            log.debug("| ", .{});
            values.print(constant, log.debug);
            log.debug(" |", .{});
        }
        log.debug("\n", .{});
    }

    fn runtimeError(frame: *CallFrame, comptime fmt: []const u8, args: anytype) void {
        log.err(fmt, args);
        const line = frame.function.chunk.lines.items[@intFromPtr(frame.ip) - 1 - @intFromPtr(frame.function.chunk.code.ptr)];
        log.err("[line {d}] in script\n", .{line});
    }
};

pub fn init(allocator: std.mem.Allocator) VM {
    return .{
        .allocator = allocator,
        .strings = StringPool.init(allocator),
        .globals = Table.init(allocator) catch unreachable,
        .frames = undefined,
    };
}

pub fn deinit(vm: *VM) void {
    vm.stackTop = 0;
    vm.frame_count = 0;
    var object = vm.objects;
    while (object) |obj| {
        object = obj.next;
        obj.deinit(vm.allocator);
    }
    vm.strings.deinit();
    vm.globals.deinit();
}

pub const Error = error{ CompileError, RuntimeError } || std.mem.Allocator.Error;

pub fn interpret(vm: *VM, input: []const u8) Error!Value {
    var compiler: Compiler = try Compiler.init(&vm.strings, vm.allocator, .script);
    const func = compiler.compile(input) catch return error.CompileError;
    defer func.object.deinit(vm.allocator);

    return try vm.interpretFunction(func);
}

pub fn interpretFunction(vm: *VM, function: *Object.Function) Error!Value {
    vm.push(.{ .object = &function.object });
    vm.frames[0] = .{
        .function = function,
        .ip = function.chunk.code.ptr,
        .slots = vm.stack[0..],
    };
    vm.frame_count = 1;
    return vm.run();
}

pub fn run(vm: *VM) Error!Value {
    var frame = vm.frames[vm.frame_count - 1];
    var ret = values.NIL_VAL;
    frame.printConstants();
    while (true) : (frame.ip += 1) {
        vm.printStack();
        _ = debug.disassembleInstruction(&frame.function.chunk, @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.ptr), debug.Writer) catch {};
        const op: Chunk.Opcode = @enumFromInt(frame.ip[0]);
        switch (op) {
            .@"return" => {
                if (vm.stackTop > 0) {
                    std.debug.print("Returning value on top: {any}\n", .{vm.stack[vm.stackTop - 1]});
                    return vm.pop();
                } else {
                    std.debug.print("Returning last value: {any}\n", .{ret});
                    return ret;
                }
            },
            .jump_if_false => {
                const offset = frame.readShort();
                if (vm.peek(0).isFalsey().boolean) frame.ip += offset;
            },
            .jump => {
                const offset = frame.readShort();
                frame.ip += offset;
            },
            .loop => {
                const offset = frame.readShort();
                frame.ip -= offset;
            },
            .constant => {
                vm.push(frame.readConstant());
            },
            .define_global => {
                const constant = frame.readConstant();
                const name = constant.object.asString();
                std.debug.print("Defining global: {s}\n", .{name.data});
                _ = try vm.globals.set(name, vm.peek(0));
                _ = vm.pop();
            },
            .get_global => {
                const name = frame.readConstant().object.asString();
                const value = vm.globals.get(name);
                std.debug.print("Got global for {s}: {any}\n", .{ name.data, value });
                values.print(value.?, std.debug.print);

                if (value) |val| {
                    vm.push(val);
                } else {
                    frame.runtimeError("Undefined variable '{s}'.", .{name.data});
                    return error.RuntimeError;
                }
            },
            .set_global => {
                const name = frame.readConstant().object.asString();
                const is_new = try vm.globals.set(name, vm.peek(0));
                if (is_new) {
                    _ = vm.globals.delete(name);
                    frame.runtimeError("Undefined variable '{s}'.", .{name.data});
                    return error.RuntimeError;
                }
            },
            .get_local => {
                const slot = frame.readByte();
                vm.push(frame.slots[slot]);
            },
            .set_local => {
                const slot = frame.readByte();
                frame.slots[slot] = vm.peek(0);
            },
            .print => {
                values.print(vm.pop(), log.info);
                log.info("\n", .{});
            },
            .pop => ret = vm.pop(),
            .true => vm.push(values.TRUE_VAL),
            .false => vm.push(values.FALSE_VAL),
            .nil => vm.push(values.NIL_VAL),
            .negate => switch (vm.peek(0)) {
                .number => {
                    var value = vm.get(0);
                    value.number = -value.number;
                },
                .boolean, .nil, .object => {
                    frame.runtimeError("Operand must be a number.", .{});
                    return error.RuntimeError;
                },
            },
            .not => vm.push(vm.pop().isFalsey()),
            .equal => {
                const b = vm.pop();
                const a = vm.pop();
                vm.push(.{ .boolean = a.equal(b) });
            },
            .add => {
                if (vm.peek(0).isString() and vm.peek(1).isString()) {
                    try vm.concatenate();
                } else if (vm.peek(0).isNumber() and vm.peek(1).isNumber()) {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .number = a.number + b.number });
                } else {
                    frame.runtimeError("Operands must be two numbers or two strings.", .{});
                    return error.RuntimeError;
                }
            },
            .subtract, .multiply, .divide, .less, .greater => {
                if (!vm.peek(0).isNumber() or !vm.peek(1).isNumber()) {
                    frame.runtimeError("Operands must be numbers.", .{});
                    return error.RuntimeError;
                }
                const b = vm.pop();
                const a = vm.pop();
                const result: Value = switch (op) {
                    .subtract => .{ .number = a.number - b.number },
                    .multiply => .{ .number = a.number * b.number },
                    .divide => .{ .number = a.number / b.number },
                    .less => .{ .boolean = a.number < b.number },
                    .greater => .{ .boolean = a.number > b.number },
                    else => unreachable,
                };
                vm.push(result);
            },
        }
    }
}

fn push(vm: *VM, value: Value) void {
    vm.stack[vm.stackTop] = value;
    vm.stackTop += 1;
}

fn pop(vm: *VM) Value {
    vm.stackTop -= 1;
    return vm.stack[vm.stackTop];
}

fn peek(vm: *VM, distance: usize) Value {
    return vm.stack[vm.stackTop - 1 - distance];
}
fn get(vm: *VM, distance: usize) *Value {
    return &vm.stack[vm.stackTop - 1 - distance];
}

fn concatenate(vm: *VM) !void {
    const b = vm.pop();
    const a = vm.pop();
    const str_a = a.object.asString();
    const str_b = b.object.asString();

    var new_raw = try vm.allocator.alloc(u8, str_a.data.len + str_b.data.len);
    @memcpy(new_raw[0..str_a.data.len], str_a.data);
    @memcpy(new_raw[str_a.data.len..], str_b.data);
    if (vm.strings.find(new_raw)) |interned| {
        vm.push(.{ .object = &interned.object });
        vm.allocator.free(new_raw);
    } else {
        const str = try Object.String.fromAlloc(new_raw, vm.allocator);
        try vm.strings.set(str, values.NIL_VAL);
        const obj = &str.object;
        str.object.next = vm.objects;
        vm.objects = obj;
        vm.push(.{ .object = obj });
    }
}

fn printStack(vm: *VM) void {
    log.debug("          ", .{});
    var i: usize = 0;
    while (i < vm.stackTop) : (i += 1) {
        log.debug("[ ", .{});
        values.print(vm.stack[i], log.debug);
        log.debug(" ]", .{});
    }
    log.debug("\n", .{});
}

test "basic run" {
    var vm = VM.init(std.testing.allocator);
    var function = try Object.Function.init(std.testing.allocator);
    defer function.object.deinit(std.testing.allocator);
    var chunk = &function.chunk;
    const index = try chunk.addConstant(.{ .number = 3.14 });
    try chunk.writeOp(.constant, 0);
    try chunk.write(index, 0);
    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpretFunction(function);
    try std.testing.expectEqual(3.14, res.number);
}

test "basic arithmatic" {
    var vm = VM.init(std.testing.allocator);
    var function = try Object.Function.init(std.testing.allocator);
    defer function.object.deinit(std.testing.allocator);
    var chunk = &function.chunk;

    const pi = 3.1415926;
    try chunk.writeOp(.constant, 0);
    const pi_index = try chunk.addConstant(.{ .number = pi });
    try chunk.write(pi_index, 0);
    try chunk.writeOp(.constant, 0);
    const two_index = try chunk.addConstant(.{ .number = 2.0 });
    try chunk.write(two_index, 0);
    try chunk.writeOp(.multiply, 0);
    try chunk.writeOp(.constant, 0);
    try chunk.write(two_index, 0);
    try chunk.writeOp(.divide, 0);
    try chunk.writeOp(.constant, 0);
    try chunk.write(two_index, 0);
    try chunk.writeOp(.add, 0);
    try chunk.writeOp(.constant, 0);
    try chunk.write(two_index, 0);
    try chunk.writeOp(.subtract, 0);
    try chunk.writeOp(.negate, 0);
    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpretFunction(function);
    try std.testing.expectEqual(-pi, res.number);
}

test "boolean logic" {
    var vm = VM.init(std.testing.allocator);
    var function = try Object.Function.init(std.testing.allocator);
    defer function.object.deinit(std.testing.allocator);
    var chunk = &function.chunk;

    try chunk.writeOp(.true, 0);
    try chunk.writeOp(.not, 0);
    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpretFunction(function);
    try std.testing.expectEqual(Value{ .boolean = false }, res);
}
test "Comparison: less" {
    var vm = VM.init(std.testing.allocator);
    var function = try Object.Function.init(std.testing.allocator);
    defer function.object.deinit(std.testing.allocator);
    var chunk = &function.chunk;

    const two = 2.0;
    try chunk.writeOp(.constant, 0);
    const two_index = try chunk.addConstant(.{ .number = two });
    try chunk.write(two_index, 0);
    const three = 3.0;
    try chunk.writeOp(.constant, 0);
    const three_index = try chunk.addConstant(.{ .number = three });
    try chunk.write(three_index, 0);

    try chunk.writeOp(.less, 0);

    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpretFunction(function);
    try std.testing.expectEqual(Value{ .boolean = true }, res);
}
test "Comparison: greater" {
    var vm = VM.init(std.testing.allocator);
    var function = try Object.Function.init(std.testing.allocator);
    defer function.object.deinit(std.testing.allocator);
    var chunk = &function.chunk;

    const two = 2.0;
    try chunk.writeOp(.constant, 0);
    const two_index = try chunk.addConstant(.{ .number = two });
    try chunk.write(two_index, 0);
    const three = 3.0;
    try chunk.writeOp(.constant, 0);
    const three_index = try chunk.addConstant(.{ .number = three });
    try chunk.write(three_index, 0);

    try chunk.writeOp(.greater, 0);

    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpretFunction(function);
    try std.testing.expectEqual(Value{ .boolean = false }, res);
}
test "Equality" {
    var vm = VM.init(std.testing.allocator);
    var function = try Object.Function.init(std.testing.allocator);
    defer function.object.deinit(std.testing.allocator);
    var chunk = &function.chunk;

    const two = 2.0;
    try chunk.writeOp(.constant, 0);
    const two_index = try chunk.addConstant(.{ .number = two });
    try chunk.write(two_index, 0);
    const three = 3.0;
    try chunk.writeOp(.constant, 0);
    const three_index = try chunk.addConstant(.{ .number = three });
    try chunk.write(three_index, 0);

    try chunk.writeOp(.equal, 0);
    try chunk.writeOp(.false, 0);
    try chunk.writeOp(.equal, 0);
    try chunk.writeOp(.nil, 0);
    try chunk.writeOp(.equal, 0);

    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpretFunction(function);
    try std.testing.expectEqual(Value{ .boolean = false }, res);
}
test "Chapter 18 end" {
    const input =
        \\ return !(5 - 4 > 3 * 2 == !nil);
        \\
    ;
    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqual(Value{ .boolean = true }, res);
}

test "Chapter 19" {
    const input =
        \\ return "Hello" + " " + "World!";
        \\
    ;
    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    const str_res = res.object.asString();
    try std.testing.expectEqualStrings("Hello World!", str_res.data);
    try std.testing.expectEqual(res.object, vm.objects.?);
}

test "Chapter 20: String equality" {
    const input =
        \\ return ("He" + "llo") == "Hello";
        \\
    ;

    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expect(res.boolean);
    // Because of interning, the "Hello" created by concatenation
    // will reference the same string as the "Hello" constant.
    // It should not be added to the objects tracking list
    try std.testing.expectEqual(null, vm.objects);
}

test "Chapter 21: Globals" {
    const input =
        \\ var breakfast = "beignets";
        \\ var beverage = "cafe au lait";
        \\ breakfast = "beignets with " + beverage;
        \\
        \\ return breakfast;
        \\
    ;
    const expected = "beignets with cafe au lait";

    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqualStrings(expected, res.object.asString().data);
    try std.testing.expect(vm.objects != null);
    // try std.testing.expectEqual(null, vm.objects.?.next);
}

test "Chapter 23: If true" {
    const input =
        \\ var result;
        \\ if (true) {
        \\     result = "true";
        \\ } else {
        \\     result = "false";
        \\ }
        \\ return result;
        \\
    ;
    // const expected = "true";

    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqual(Object.ObjectType.string, res.object.tag);
    std.debug.print("RES!!! == {x}", .{@intFromPtr(res.object)});
    // try std.testing.expectEqualStrings(expected, res.object.asString().data);
}

test "Chapter 23: If false" {
    const input =
        \\ var result;
        \\ if (false) {
        \\     result = "true";
        \\ } else {
        \\     result = "false";
        \\ }
        \\ return result;
        \\
    ;
    // const expected = "false";

    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    std.debug.print("RES!!! == {any}\n", .{res.object.tag});
    try std.testing.expectEqual(Object.ObjectType.string, res.object.tag);
    // try std.testing.expectEqualStrings(expected, res.object.asString().data);
}
test "Chapter 23: If no else" {
    const input =
        \\ var result;
        \\ if (false) {
        \\     result = "true";
        \\ }
        \\ return result;
        \\
    ;
    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqual(values.NIL_VAL, res);
}

test "Chapter 23: while" {
    const input =
        \\ var result = 1;
        \\ while (result < 100) {
        \\    result = result * 2;
        \\ }
        \\ return result;
        \\
    ;
    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqual(128, res.number);
}
