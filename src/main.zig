const std = @import("std");
const zlox = @import("zlox");
const tracer = @import("tracer");
const builtin = @import("builtin");

pub const tracer_impl = tracer.spall;

pub const std_options = .{
    .logFn = logFn,
    .log_level = if (builtin.mode == .Debug) .debug else .info,
};

pub fn logFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;
    // Ignore all non-error logging from sources other than
    // .my_project, .nice_library and the default
    // const scope_prefix = "(" ++ switch (scope) {
    //     .my_project, .nice_library, std.log.default_log_scope => @tagName(scope),
    //     else => if (@intFromEnum(level) <= @intFromEnum(std.log.Level.err))
    //         @tagName(scope)
    //     else
    //         return,
    // } ++ "): ";

    // const prefix = "[" ++ comptime level.asText() ++ "] " ++ scope_prefix;
    //
    switch (level) {
        .err => {
            std.debug.lockStdErr();
            defer std.debug.unlockStdErr();

            const writer = std.io.getStdErr().writer();
            nosuspend writer.print(format, args) catch return;
        },
        else => {
            // Print the message to stderr, silently ignoring any errors
            const stdout = std.io.getStdOut();
            const writer = stdout.writer();
            nosuspend writer.print(format, args) catch return;
        },
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.skip();
    const file = args.next();
    const extra = args.next();
    if (extra) |_| {
        _ = try std.io.getStdOut().write(
            \\Usage: zlox [path]
            \\
        );
    }

    try tracer.init();
    defer tracer.deinit();

    try tracer.init_thread(null);
    defer tracer.deinit_thread();
    if (file) |f| {
        try zlox.runFile(f, allocator);
    } else {
        try zlox.repl(allocator);
    }
}
