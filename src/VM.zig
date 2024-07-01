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

pub fn interpret(vm: *VM, chunk: *Chunk) Error!void {
    vm.chunk = chunk;
    vm.ip = 0;
    return vm.run();
}

fn run(vm: *VM) Error!void {
    while (true) : (vm.ip += 1) {
        vm.printStack();
        _ = debug.disassembleInstruction(vm.chunk, vm.ip, debug.Writer) catch {};
        const op: Chunk.Opcode = @enumFromInt(vm.chunk.code[vm.ip]);
        switch (op) {
            .@"return" => {
                const val = vm.pop();
                values.print(val, log.debug);
                return;
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
    try chunk.addConstant(3.14156);
    try chunk.write(0, 0);

    try vm.interpret(&chunk);
}
