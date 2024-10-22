const std = @import("std");
const VM = @import("VM.zig");
const Compiler = @import("Compiler.zig");
const Chunk = @import("Chunk.zig");
const values = @import("values.zig");

const log = std.log.scoped(.root);

pub fn repl(allocator: std.mem.Allocator) !void {
    var out = std.io.getStdOut().writer();
    var in = std.io.getStdIn().reader();
    var line: [2048]u8 = undefined;

    var vm: VM = undefined;
    vm.init(allocator);
    defer vm.deinit();
    try vm.addNatives();
    // var compiler = try Compiler.init(&vm.manager, .script);
    while (true) {
        _ = try out.write("> ");
        const count = try in.read(&line);
        if (std.ascii.eqlIgnoreCase(line[0..count], "quit\n")) {
            return;
        }
        // const res = compiler.compile(line[0..count]) catch continue;
        // const val = vm.interpretFunction(res) catch continue;
        const val = vm.interpret(line[0..count]) catch |err| blk: {
            switch (err) {
                error.RuntimeError => {
                    log.err("RuntimeError\n", .{});
                    break :blk values.NIL_VAL;
                },
                error.CompileError => {
                    log.err("CompileError\n", .{});
                    break :blk values.NIL_VAL;
                },
                else => return,
            }
        };
        values.print(val, log.info);
    }
}

pub fn runFile(filename: []const u8, allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    const input = try file.readToEndAlloc(allocator, 1024 * 1024 * 1024);
    var vm: VM = undefined;
    vm.init(allocator);
    defer vm.deinit();
    try vm.addNatives();
    _ = vm.interpret(input) catch |err| {
        switch (err) {
            error.RuntimeError => std.process.exit(70),
            error.CompileError => std.process.exit(65),
            else => {},
        }
    };
    // values.print(val, log.info);
}

test {
    _ = @import("Chunk.zig");
    _ = @import("debug.zig");
    _ = @import("Compiler.zig");
    _ = @import("Table.zig");
    _ = @import("VM.zig");
}
