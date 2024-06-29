const std = @import("std");
const log = std.log.scoped(.vm);

const VM = @This();
const Chunk = @import("Chunk.zig");
const values = @import("values.zig");

ip: usize = 0,
chunk: *Chunk,

pub fn init() VM {
    return .{
        .chunk = undefined,
    };
}

pub fn deinit() void {}

pub const Error = error{ CompileError, RuntimeError };

pub fn interpret(vm: *VM, chunk: *Chunk) Error!void {
    vm.chunk = chunk;
    vm.ip = 0;
    return vm.run();
}

fn run(vm: *VM) Error!void {
    while (true) : (vm.ip += 1) {
        const op: Chunk.Opcode = @enumFromInt(vm.chunk.code[vm.ip]);
        switch (op) {
            .@"return" => return,
            .constant => {
                vm.ip += 1;
                const index = vm.chunk.code[vm.ip];
                const constant = vm.chunk.getConstant(index);
                values.print(constant, log.debug);
            },
        }
    }
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
