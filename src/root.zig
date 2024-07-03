const std = @import("std");
const VM = @import("VM.zig");

pub fn repl(allocator: std.mem.Allocator) !void {
    var out = std.io.getStdOut().writer();
    var in = std.io.getStdIn().reader();
    var line: [2048]u8 = undefined;

    var arena = std.heap.ArenaAllocator.init(allocator);
    while (true) {
        try out.write("> ");
        const count = try in.read(&line);
        var vm = VM.init(arena.allocator());
        vm.interpret(line[0..count]);
        _ = arena.reset();
    }
}

pub fn runFile(filename: []const u8, allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    const input = file.readToEndAlloc(allocator, 1024 * 1024 * 1024);
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer _ = arena.reset();
    var vm = VM.init(arena.allocator());
    try vm.interpret(input);
}

test {
    _ = @import("Chunk.zig");
    _ = @import("VM.zig");
    _ = @import("debug.zig");
    _ = @import("Compiler.zig");
}
