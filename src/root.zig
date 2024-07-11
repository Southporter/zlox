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

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var vm: VM = undefined;
    vm.init(arena.allocator());
    var compiler = try Compiler.init(&vm.strings, arena.allocator(), .script);
    while (true) {
        _ = try out.write("> ");
        const count = try in.read(&line);
        const res = compiler.compile(line[0..count]) catch continue;
        const val = vm.interpretFunction(res) catch continue;
        values.print(val, log.info);
    }
}

pub fn runFile(filename: []const u8, allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    const input = try file.readToEndAlloc(allocator, 1024 * 1024 * 1024);
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer _ = arena.reset(.free_all);
    var vm: VM = undefined;
    vm.init(arena.allocator());
    defer vm.deinit();
    try vm.addNatives();
    const val = try vm.interpret(input);
    values.print(val, log.info);
}

test {
    _ = @import("Chunk.zig");
    _ = @import("debug.zig");
    _ = @import("Compiler.zig");
    _ = @import("Table.zig");
    _ = @import("VM.zig");
}
