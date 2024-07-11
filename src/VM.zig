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
const builtins = @import("builtins.zig");

const STACK_MAX = 256 * FRAMES_MAX;
const FRAMES_MAX = 64;

stack: [STACK_MAX]Value = undefined,
stack_top: [*]Value = undefined,
arena: std.heap.ArenaAllocator = undefined,
objects: ?*Object = null,
strings: StringPool = undefined,
globals: Table = undefined,
frames: [FRAMES_MAX]CallFrame = undefined,
frame_count: usize = 0,

const CallFrame = struct {
    function: *Object.Function,
    ip: [*]u8,
    slots: [*]Value,

    fn readByte(frame: *CallFrame) u8 {
        defer frame.ip += 1;
        return frame.ip[0];
    }
    fn readShort(frame: *CallFrame) u16 {
        var val: u16 = @as(u16, frame.ip[0]) << 8;
        val |= frame.ip[1];
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

    fn getName(frame: *CallFrame) []const u8 {
        if (frame.function.name) |name| {
            return name.data;
        }
        return "script";
    }
};

pub fn init(vm: *VM, allocator: std.mem.Allocator) void {
    vm.arena = std.heap.ArenaAllocator.init(allocator);
    const arena_allocator = vm.arena.allocator();
    vm.strings = StringPool.init(arena_allocator);
    vm.globals = Table.init(arena_allocator) catch unreachable;
    vm.stack_top = vm.stack[0..STACK_MAX].ptr;
    vm.objects = null;
    vm.frame_count = 0;
}

pub fn addNatives(vm: *VM) !void {
    try vm.defineNative("clock", builtins.clock);
}

pub fn deinit(vm: *VM) void {
    vm.stack_top = vm.stack[0..STACK_MAX].ptr;
    vm.frame_count = 0;
    // var object = vm.objects;
    // while (object) |obj| {
    //     object = obj.next;
    //     obj.deinit(vm.arena.allocator());
    // }
    // vm.strings.deinit();
    // vm.globals.deinit();
    vm.arena.deinit();
}

pub const Error = error{ CompileError, RuntimeError } || std.mem.Allocator.Error;

pub fn interpret(vm: *VM, input: []const u8) Error!Value {
    var compiler: Compiler = try Compiler.init(&vm.strings, vm.arena.allocator(), .script);
    const func = compiler.compile(input) catch return error.CompileError;
    return try vm.interpretFunction(func);
}

pub fn interpretFunction(vm: *VM, function: *Object.Function) Error!Value {
    vm.push(.{ .object = &function.object });
    if (!vm.call(function, 0)) {
        return error.RuntimeError;
    }
    return vm.run();
}

pub fn run(vm: *VM) Error!Value {
    var frame = &vm.frames[vm.frame_count - 1];

    while (true) {
        vm.printStack(log.debug);
        // try debug.disassembleChunk(&frame.function.chunk, frame.getName(), debug.Writer);
        _ = debug.disassembleInstruction(&frame.function.chunk, @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.ptr), debug.Writer) catch {};
        const op: Chunk.Opcode = @enumFromInt(frame.readByte());
        switch (op) {
            .@"return" => {
                const result = vm.pop();
                vm.frame_count -= 1;
                if (vm.frame_count == 0) {
                    // _ = vm.pop();
                    return result;
                }
                vm.stack_top = frame.slots;
                vm.push(result);
                frame = &vm.frames[vm.frame_count - 1];
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
            .call => {
                const arg_count = frame.readByte();
                if (!vm.callValue(vm.peek(arg_count), arg_count)) {
                    return error.RuntimeError;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .constant => {
                vm.push(frame.readConstant());
            },
            .define_global => {
                const constant = frame.readConstant();
                const name = constant.object.asString();
                _ = try vm.globals.set(name, vm.peek(0));
                _ = vm.pop();
            },
            .get_global => {
                const name = frame.readConstant().object.asString();
                const value = vm.globals.get(name);

                if (value) |val| {
                    vm.push(val);
                } else {
                    vm.runtimeError("Undefined variable '{s}'.", .{name.data});
                    return error.RuntimeError;
                }
            },
            .set_global => {
                const name = frame.readConstant().object.asString();
                const is_new = try vm.globals.set(name, vm.peek(0));
                if (is_new) {
                    _ = vm.globals.delete(name);
                    vm.runtimeError("Undefined variable '{s}'.", .{name.data});
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
            .pop => _ = vm.pop(),
            .true => vm.push(values.TRUE_VAL),
            .false => vm.push(values.FALSE_VAL),
            .nil => vm.push(values.NIL_VAL),
            .negate => switch (vm.peek(0)) {
                .number => {
                    var value = vm.get(0);
                    value.number = -value.number;
                },
                .boolean, .nil, .object => {
                    vm.runtimeError("Operand must be a number.", .{});
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
                    vm.runtimeError("Operands must be two numbers or two strings.", .{});
                    return error.RuntimeError;
                }
            },
            .subtract, .multiply, .divide, .less, .greater => {
                if (!vm.peek(0).isNumber() or !vm.peek(1).isNumber()) {
                    vm.runtimeError("Operands must be numbers.", .{});
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
    vm.stack_top[0] = value;
    vm.stack_top += 1;
}

fn pop(vm: *VM) Value {
    vm.stack_top -= 1;
    return vm.stack_top[0];
}

fn peek(vm: *VM, distance: usize) Value {
    const value_ptr = vm.stack_top - 1 - distance;
    return value_ptr[0];
}
fn get(vm: *VM, distance: usize) *Value {
    const value_ptr = vm.stack_top - 1 - distance;
    return &value_ptr[0];
}

fn callValue(vm: *VM, callee: Value, arg_count: u8) bool {
    if (std.meta.activeTag(callee) == .object) {
        switch (callee.object.tag) {
            .function => return vm.call(callee.object.asFunction(), arg_count),
            .native => {
                const native = callee.object.asNative();
                const args = vm.stack_top - arg_count;
                const result = native.function(arg_count, args);
                vm.stack_top -= arg_count + 1;
                vm.push(result);
                return true;
            },
            else => {},
        }
    }
    vm.runtimeError("Can only call functions and classes.", .{});
    return false;
}

fn call(vm: *VM, function: *Object.Function, arg_count: u8) bool {
    if (arg_count != function.arity) {
        vm.runtimeError("Expected {d} arguments but got {d}.\n", .{ function.arity, arg_count });
        return false;
    }
    if (vm.frame_count == FRAMES_MAX) {
        vm.runtimeError("Stack overflow.\n", .{});
        return false;
    }
    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.function = function;
    frame.ip = function.chunk.code.ptr;
    frame.slots = vm.stack_top - 1 - arg_count;
    return true;
}

fn concatenate(vm: *VM) !void {
    const b = vm.pop();
    const a = vm.pop();
    const str_a = a.object.asString();
    const str_b = b.object.asString();

    var allocator = vm.arena.allocator();
    var new_raw = try allocator.alloc(u8, str_a.data.len + str_b.data.len);
    @memcpy(new_raw[0..str_a.data.len], str_a.data);
    @memcpy(new_raw[str_a.data.len..], str_b.data);

    vm.push(.{ .object = try vm.copyString(new_raw, allocator) });
}

fn copyString(vm: *VM, string: []const u8, allocator: std.mem.Allocator) !*Object {
    if (vm.strings.find(string)) |interned| {
        allocator.free(string);
        return &interned.object;
    } else {
        const str = try Object.String.fromAlloc(string, allocator);
        try vm.strings.set(str, values.NIL_VAL);
        const obj = &str.object;
        str.object.next = vm.objects;
        vm.objects = obj;
        return obj;
    }
}

fn defineNative(vm: *VM, name: []const u8, func: Object.NativeFn) !void {
    const allocator = vm.arena.allocator();
    vm.push(.{ .object = try vm.copyString(name, allocator) });
    const native = try Object.Native.init(allocator, func);
    vm.push(.{ .object = &native.object });
    std.debug.assert(try vm.globals.set(vm.stack[0].object.asString(), vm.stack[1]));
    _ = vm.pop();
    _ = vm.pop();
}

fn runtimeError(vm: *VM, comptime fmt: []const u8, args: anytype) void {
    log.err(fmt, args);

    var i = vm.frame_count;
    while (i > 0) : (i -= 1) {
        const frame = vm.frames[i - 1];
        const fun = frame.function;
        const instruction = @intFromPtr(frame.ip) - @intFromPtr(fun.chunk.code.ptr);
        const name = if (fun.name) |n| n.data else "script";
        log.err("[line {d}] in {s}()\n", .{ fun.chunk.lines.items[instruction], name });
    }
}

fn printStack(vm: *VM, logger: *const fn (comptime msg: []const u8, args: anytype) void) void {
    logger("          ", .{});
    var i: [*]Value = vm.stack[0..STACK_MAX].ptr;
    while (@intFromPtr(i) < @intFromPtr(vm.stack_top)) : (i += 1) {
        logger("[ ", .{});
        values.print(i[0], logger);
        logger(" ]", .{});
    }
    logger("\n", .{});
}

test "basic run" {
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    var function = try Object.Function.init(std.testing.allocator);
    defer function.object.deinit(std.testing.allocator);
    var chunk = &function.chunk;
    const index = try chunk.addConstant(.{ .number = 3.14 });
    try chunk.writeOp(.constant, 0);
    try chunk.write(index, 0);
    try chunk.writeOp(.@"return", 0);
    try chunk.writeOp(.pop, 0);
    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpretFunction(function);
    try std.testing.expectEqual(3.14, res.number);
}

test "basic arithmatic" {
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqual(Value{ .boolean = true }, res);
}

test "Chapter 19" {
    const input =
        \\ return "Hello" + " " + "World!";
        \\
    ;
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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

    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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

    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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
    const expected = "true";

    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    std.debug.print("RES!!! == {s}\n", .{@tagName(std.meta.activeTag(res))});
    try std.testing.expectEqual(Object.ObjectType.string, res.object.tag);
    try std.testing.expectEqualStrings(expected, res.object.asString().data);
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

    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    std.debug.print("RES!!! == {s}\n", .{@tagName(std.meta.activeTag(res))});
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
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
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
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqual(128, res.number);
}

test "Chapter 23: normal fun" {
    const input =
        \\ fun add(a, b, c) {
        \\    return a + b + c;
        \\ }
        \\
        \\ return add(1, 2, 3);
        \\
    ;
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqual(6, res.number);
}

test "Chapter 23: bad fun" {
    const input =
        \\ fun a() { b(); }
        \\ fun b() { c(); }
        \\ fun c() {
        \\    c("too", "many");
        \\ }
        \\
        \\ a();
        \\
    ;
    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();
    const res = vm.interpret(input);
    try std.testing.expectError(error.RuntimeError, res);
}
