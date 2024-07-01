const std = @import("std");
const log = std.log.scoped(.vm);

const VM = @This();
const Chunk = @import("Chunk.zig");
const values = @import("values.zig");
const Value = values.Value;
const debug = @import("debug.zig");

const STACK_MAX = 256;

ip: usize = 0,
chunk: *Chunk,
stack: [STACK_MAX]Value = undefined,
stackTop: usize = 0,

pub fn init() VM {
    return .{
        .chunk = undefined,
    };
}

pub fn deinit(vm: *VM) void {
    vm.stackTop = 0;
    vm.ip = 0;
}

pub const Error = error{ CompileError, RuntimeError };

pub fn interpret(vm: *VM, chunk: *Chunk) Error!Value {
    vm.chunk = chunk;
    vm.ip = 0;
    return vm.run();
}

fn run(vm: *VM) Error!Value {
    while (true) : (vm.ip += 1) {
        vm.printStack();
        _ = debug.disassembleInstruction(vm.chunk, vm.ip, debug.Writer) catch {};
        const op: Chunk.Opcode = @enumFromInt(vm.chunk.code[vm.ip]);
        switch (op) {
            .@"return" => {
                const val = vm.pop();
                values.print(val, log.debug);
                return val;
            },
            .constant => {
                vm.ip += 1;
                const index = vm.chunk.code[vm.ip];
                const constant = vm.chunk.getConstant(index);
                vm.push(constant);
            },
            .negate => {
                vm.push(-vm.pop());
            },
            .add, .subtract, .multiply, .divide => {
                const b = vm.pop();
                const a = vm.pop();
                const result = switch (op) {
                    .add => a + b,
                    .subtract => a - b,
                    .multiply => a * b,
                    .divide => a / b,
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
    var vm = VM.init();
    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    try chunk.write(1, 0);
    try chunk.write(0, 0);
    try chunk.addConstant(3.14);
    try chunk.write(0, 0);

    const res = try vm.interpret(&chunk);
    try std.testing.expectEqual(3.14, res);
}

test "basic arithmatic" {
    var vm = VM.init();
    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    const pi = 3.1415926;
    try chunk.writeOp(.constant, 0);
    try chunk.write(0, 0);
    try chunk.addConstant(pi);
    try chunk.writeOp(.constant, 0);
    try chunk.write(1, 0);
    try chunk.addConstant(2.0);
    try chunk.writeOp(.multiply, 0);
    try chunk.writeOp(.constant, 0);
    try chunk.write(1, 0);
    try chunk.writeOp(.divide, 0);
    try chunk.writeOp(.constant, 0);
    try chunk.write(1, 0);
    try chunk.writeOp(.add, 0);
    try chunk.writeOp(.constant, 0);
    try chunk.write(1, 0);
    try chunk.writeOp(.subtract, 0);
    try chunk.writeOp(.negate, 0);
    try chunk.writeOp(.@"return", 0);

    const res = try vm.interpret(&chunk);
    try std.testing.expectEqual(-pi, res);
}
