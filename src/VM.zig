const std = @import("std");
const std_builtin = @import("builtin");
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
const Manager = @import("memory.zig").Manager;

const STACK_MAX = 256 * FRAMES_MAX;
const FRAMES_MAX = 64;

stack: [STACK_MAX]Value = undefined,
stack_top: [*]Value = undefined,
manager: Manager,
root_compiler: Compiler,
openUpvalues: ?*Object.Upvalue = null,
globals: Table = undefined,
frames: [FRAMES_MAX]CallFrame = undefined,
frame_count: usize = 0,

const CallFrame = struct {
    closure: *Object.Closure,
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
        return frame.closure.function.chunk.getConstant(index);
    }
    fn printConstants(frame: *CallFrame) void {
        log.debug("           ", .{});
        for (frame.closure.function.chunk.constants.items) |constant| {
            log.debug("| ", .{});
            values.print(constant, log.debug);
            log.debug(" |", .{});
        }
        log.debug("\n", .{});
    }

    fn getName(frame: *CallFrame) []const u8 {
        if (frame.closure.function.name) |name| {
            return name.data;
        }
        return "script";
    }
    fn chunk(frame: *CallFrame) *Chunk {
        return &frame.closure.function.chunk;
    }
};

pub fn init(vm: *VM, allocator: std.mem.Allocator) void {
    vm.manager.init(allocator);
    vm.globals = Table.init(&vm.manager) catch unreachable;
    vm.stack_top = vm.stack[0..STACK_MAX].ptr;
    vm.openUpvalues = null;
    vm.frame_count = 0;
}

pub fn addNatives(vm: *VM) !void {
    try vm.defineNative("clock", builtins.clock);
    try vm.defineNative("toString", builtins.toString);
}

pub fn deinit(vm: *VM) void {
    vm.stack_top = vm.stack[0..STACK_MAX].ptr;
    vm.frame_count = 0;
    // var object = vm.objects;
    // while (object) |obj| {
    //     object = obj.next;
    //     obj.deinit(vm.manager.inner());
    // }
    // vm.globals.deinit();
    vm.manager.deinit();
}

pub const Error = error{ CompileError, RuntimeError } || Manager.Error || Object.NativeError;

pub fn interpret(vm: *VM, input: []const u8) Error!Value {
    vm.root_compiler = try Compiler.init(&vm.manager, .script);
    const func = vm.root_compiler.compile(input) catch return error.CompileError;
    return try vm.interpretFunction(func);
}

pub fn interpretFunction(vm: *VM, function: *Object.Function) Error!Value {
    vm.push(.{ .object = &function.object });
    const closure = try vm.manager.allocClosure(function);
    _ = vm.pop();
    vm.push(.{ .object = &closure.object });
    if (!vm.call(closure, 0)) {
        return error.RuntimeError;
    }
    return vm.run();
}

pub fn run(vm: *VM) Error!Value {
    var frame = &vm.frames[vm.frame_count - 1];

    try debug.disassembleChunk(frame.chunk(), frame.getName(), debug.Writer);
    while (true) {
        // vm.printStack(log.debug);
        _ = debug.disassembleInstruction(frame.chunk(), @intFromPtr(frame.ip) - @intFromPtr(frame.chunk().code.ptr), debug.Writer) catch {};
        const op: Chunk.Opcode = @enumFromInt(frame.readByte());
        switch (op) {
            .@"return" => {
                const result = vm.pop();
                vm.closeUpvalues(&frame.slots[0]);
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
                if (!try vm.callValue(vm.peek(arg_count), arg_count)) {
                    return error.RuntimeError;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .closure => {
                const fun = frame.readConstant().object.as(Object.Function);
                const closure = try vm.manager.allocClosure(fun);
                vm.push(.{ .object = &closure.object });
                for (closure.upvalues) |*upvalue| {
                    const is_local = frame.readByte();
                    const index = frame.readByte();
                    if (is_local == 1) {
                        upvalue.* = try vm.captureUpvalue(&(frame.slots + index)[0]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
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
            .get_upvalue => {
                const slot = frame.readByte();
                vm.push(frame.closure.upvalues[slot].?.location.*);
            },
            .set_upvalue => {
                const slot = frame.readByte();
                frame.closure.upvalues[slot].?.location.* = vm.peek(0);
            },
            .close_upvalue => {
                vm.closeUpvalues(&(vm.stack_top - 1)[0]);
                _ = vm.pop();
            },
            .print => {
                values.print(vm.pop(), log.warn);
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

fn callValue(vm: *VM, callee: Value, arg_count: u8) !bool {
    if (std.meta.activeTag(callee) == .object) {
        switch (callee.object.tag) {
            .closure => return vm.call(callee.object.as(Object.Closure), arg_count),
            .native => {
                const native = callee.object.asNative();
                const args = vm.stack_top - arg_count;
                const result = try native.function(arg_count, args, &vm.manager);
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

fn call(vm: *VM, closure: *Object.Closure, arg_count: u8) bool {
    if (arg_count != closure.function.arity) {
        vm.runtimeError("Expected {d} arguments but got {d}.\n", .{ closure.function.arity, arg_count });
        return false;
    }
    if (vm.frame_count == FRAMES_MAX) {
        vm.runtimeError("Stack overflow.\n", .{});
        return false;
    }
    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.closure = closure;
    frame.ip = closure.function.chunk.code.ptr;
    frame.slots = vm.stack_top - 1 - arg_count;
    return true;
}

fn captureUpvalue(vm: *VM, local: *Value) !*Object.Upvalue {
    var prevUpvalue: ?*Object.Upvalue = null;
    var upvalue = vm.openUpvalues;
    while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
        prevUpvalue = upvalue;
        upvalue = upvalue.?.next;
    }

    if (upvalue != null and upvalue.?.location == local) {
        return upvalue.?;
    }
    var created = try vm.manager.allocUpvalue(local);
    created.next = upvalue;
    if (prevUpvalue) |prev| {
        prev.next = created;
    } else {
        vm.openUpvalues = created;
    }
    return created;
}

fn closeUpvalues(vm: *VM, last: *Value) void {
    while (vm.openUpvalues != null and @intFromPtr(vm.openUpvalues.?.location) >= @intFromPtr(last)) {
        var upvalue = vm.openUpvalues.?;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;

        vm.openUpvalues = upvalue.next;
    }
}

fn concatenate(vm: *VM) !void {
    const str_b = vm.peek(0).object.asString();
    const str_a = vm.peek(1).object.asString();

    const new_str = try vm.manager.concat(str_a.data, str_b.data);

    _ = vm.pop();
    _ = vm.pop();
    vm.push(.{ .object = new_str });
}

fn defineNative(vm: *VM, name: []const u8, func: Object.NativeFn) !void {
    vm.push(.{ .object = try vm.manager.copy(name) });
    const native = try vm.manager.allocObject(Object.Native);
    native.as(Object.Native).function = func;
    vm.push(.{ .object = native });
    std.debug.assert(try vm.globals.set(vm.stack[0].object.asString(), vm.stack[1]));
    _ = vm.pop();
    _ = vm.pop();
}

fn runtimeError(vm: *VM, comptime fmt: []const u8, args: anytype) void {
    const logger = if (std_builtin.is_test) log.warn else log.err;
    logger(fmt, args);

    var i = vm.frame_count;
    while (i > 0) : (i -= 1) {
        const frame = vm.frames[i - 1];
        const fun = frame.closure.function;
        const instruction = @intFromPtr(frame.ip) - @intFromPtr(fun.chunk.code.ptr);
        const name = if (fun.name) |n| n.data else "script";
        logger("[line {d}] in {s}()\n", .{ fun.chunk.lines.items[instruction], name });
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
    defer vm.deinit();
    var function = try Object.Function.init(&vm.manager);
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
    defer vm.deinit();
    var function = try Object.Function.init(&vm.manager);
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
    defer vm.deinit();
    var function = try Object.Function.init(&vm.manager);
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
    defer vm.deinit();
    var function = try Object.Function.init(&vm.manager);
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
    defer vm.deinit();
    var function = try Object.Function.init(&vm.manager);
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
    defer vm.deinit();
    var function = try Object.Function.init(&vm.manager);
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
    try std.testing.expectEqual(res.object, vm.manager.objects.?);
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
    try std.testing.expect(null != vm.manager.objects.?.next);
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
    try std.testing.expect(vm.manager.objects != null);
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
    const expected = "false";

    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();
    const res = try vm.interpret(input);
    try std.testing.expectEqualStrings(expected, res.object.asString().data);
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

test "Chapter 25: Closure upvalues" {
    const input =
        \\ fun outer() {
        \\   var a = 1;
        \\   var b = 2;
        \\   fun middle() {
        \\     var c = 3;
        \\     var d = 4;
        \\     fun inner() {
        \\       return a + c + b + d;
        \\     }
        \\
        \\     return inner;
        \\   }
        \\   return middle;
        \\ }
        \\ var mid = outer();
        \\ var inner = mid();
        \\ var res = inner();
        \\ return res;
        \\
    ;

    var vm: VM = undefined;
    vm.init(std.testing.allocator);
    defer vm.deinit();

    const res = try vm.interpret(input);
    try std.testing.expectEqual(10, res.number);
}
