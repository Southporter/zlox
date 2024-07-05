const std = @import("std");
const Compiler = @This();
const Scanner = @import("Scanner.zig");
const Chunk = @import("Chunk.zig");
const values = @import("values.zig");
const Object = @import("Object.zig");

parser: Parser,
chunk: *Chunk = undefined,

const Parser = struct {
    scanner: Scanner,
    previous: ?Scanner.Token = null,
    current: ?Scanner.Token = null,
    had_error: bool = false,
    panic_mode: bool = false,

    const log = std.log.scoped(.parser);

    pub fn advance(parser: *Parser) void {
        parser.previous = parser.current;

        while (true) {
            parser.current = parser.scanner.next();
            if (parser.current.?.tag != .@"error") break;
            parser.errorAtCurrent(parser.current.?.raw);
        }
    }

    pub fn consume(parser: *Parser, tag: Scanner.TokenType, message: []const u8) void {
        if (parser.current.?.tag == tag) {
            parser.advance();
            return;
        }

        parser.errorAtCurrent(message);
    }

    fn errorAtCurrent(parser: *Parser, message: []const u8) void {
        parser.errorAt(parser.current.?, message);
    }

    fn errorAt(parser: *Parser, token: Scanner.Token, message: []const u8) void {
        if (parser.panic_mode) return;
        parser.panic_mode = true;

        log.err("[line {d}] Error", .{token.line});
        if (token.tag == .eof) {
            log.err(" at end", .{});
        } else if (token.tag == .@"error") {
            // Nothing.
        } else {
            log.err(" at '{s}'", .{token.raw});
        }

        log.err(": {s}\n", .{message});
        parser.had_error = true;
    }

    fn err(parser: *Parser, message: []const u8) void {
        parser.errorAt(parser.previous.?, message);
    }
};

const Precedence = enum {
    none,
    assignment,
    @"or",
    @"and",
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,

    fn increment(current: Precedence) Precedence {
        return switch (current) {
            .none => .assignment,
            .assignment => .@"or",
            .@"or" => .@"and",
            .@"and" => .equality,
            .equality => .comparison,
            .comparison => .term,
            .term => .factor,
            .factor => .unary,
            .unary => .call,
            .call => .primary,
            .primary => .primary,
        };
    }

    fn greater(current: Precedence, next: Precedence) bool {
        return @intFromEnum(current) > @intFromEnum(next);
    }
};

pub const CompileError = error{ConstantOverflow} || std.mem.Allocator.Error || std.fmt.ParseFloatError;

const ParseFn = *const fn (*Compiler) CompileError!void;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .none,
};

const rules = blk: {
    var map = std.EnumArray(Scanner.TokenType, ParseRule).initFill(.{});
    map.set(.left_paren, ParseRule{
        .prefix = grouping,
    });
    map.set(.minus, ParseRule{
        .prefix = unary,
        .infix = binary,
        .precedence = .term,
    });
    map.set(.plus, ParseRule{
        .infix = binary,
        .precedence = .term,
    });
    map.set(.slash, ParseRule{
        .infix = binary,
        .precedence = .factor,
    });
    map.set(.star, ParseRule{
        .infix = binary,
        .precedence = .factor,
    });
    map.set(.number, ParseRule{
        .prefix = number,
        .precedence = .primary,
    });
    map.set(.string, ParseRule{
        .prefix = string,
        .precedence = .primary,
    });
    map.set(.true, ParseRule{
        .prefix = literal,
    });
    map.set(.false, ParseRule{
        .prefix = literal,
    });
    map.set(.nil, ParseRule{
        .prefix = literal,
    });
    map.set(.bang, ParseRule{
        .prefix = unary,
    });
    map.set(.bang_equal, ParseRule{
        .infix = binary,
        .precedence = .equality,
    });
    map.set(.equal_equal, ParseRule{
        .infix = binary,
        .precedence = .equality,
    });
    map.set(.greater, ParseRule{
        .infix = binary,
        .precedence = .comparison,
    });
    map.set(.greater_equal, ParseRule{
        .infix = binary,
        .precedence = .comparison,
    });
    map.set(.less, ParseRule{
        .infix = binary,
        .precedence = .comparison,
    });
    map.set(.less_equal, ParseRule{
        .infix = binary,
        .precedence = .comparison,
    });

    break :blk map;
};

fn getRule(tag: Scanner.TokenType) ParseRule {
    return rules.get(tag);
}

fn parsePrecedence(compiler: *Compiler, precedence: Precedence) CompileError!void {
    compiler.parser.advance();
    const prefix_fn = getRule(compiler.parser.previous.?.tag).prefix;
    if (prefix_fn) |prefix| {
        try prefix(compiler);
    } else {
        compiler.parser.err("Expect expression.");
        return;
    }

    while (getRule(compiler.parser.current.?.tag).precedence.greater(precedence)) {
        compiler.parser.advance();
        const infix_fn = getRule(compiler.parser.previous.?.tag).infix;
        if (infix_fn) |infix| {
            try infix(compiler);
        } else {
            compiler.parser.err("Expected infix.");
        }
    }
}

fn currentChunk(compiler: *Compiler) *Chunk {
    return compiler.chunk;
}

pub fn compile(compiler: *Compiler, source: []const u8, chunk: *Chunk) !bool {
    compiler.chunk = chunk;
    compiler.parser = .{
        .scanner = Scanner{ .source = source },
    };
    compiler.parser.advance();
    try compiler.expression();
    compiler.parser.consume(.eof, "Expected end of expression");
    try compiler.endCompilation();
    return !compiler.parser.had_error;
}

fn endCompilation(compiler: *Compiler) !void {
    return compiler.emitOp(.@"return");
}

fn emitOp(compiler: *Compiler, op: Chunk.Opcode) !void {
    try compiler.currentChunk().writeOp(op, compiler.parser.previous.?.line);
}
fn emitByte(compiler: *Compiler, byte: u8) !void {
    try compiler.currentChunk().write(byte, compiler.parser.previous.?.line);
}
fn emitOpAndArg(compiler: *Compiler, op: Chunk.Opcode, arg: u8) !void {
    try compiler.emitOp(op);
    try compiler.emitByte(arg);
}
fn emitOpOp(compiler: *Compiler, op: Chunk.Opcode, op2: Chunk.Opcode) !void {
    try compiler.emitOp(op);
    try compiler.emitOp(op2);
}

fn emitConstant(compiler: *Compiler, constant: values.Value) !void {
    try compiler.emitOpAndArg(.constant, try compiler.makeConstant(constant));
}

fn makeConstant(compiler: *Compiler, constant: values.Value) !u8 {
    const index = compiler.currentChunk().addConstant(constant) catch |e| cth: {
        switch (e) {
            error.ConstantOverflow => {
                compiler.parser.err("Too many constants in one chunk.");
                break :cth 0;
            },
            else => return e,
        }
    };
    return index;
}

fn expression(compiler: *Compiler) CompileError!void {
    try compiler.parsePrecedence(.assignment);
}

fn number(compiler: *Compiler) CompileError!void {
    const val = try std.fmt.parseFloat(f64, compiler.parser.previous.?.raw);
    try compiler.emitConstant(.{ .number = val });
}

fn string(compiler: *Compiler) CompileError!void {
    try compiler.emitConstant(.{ .object = try compiler.copyString() });
}

fn copyString(compiler: *Compiler) !*Object {
    const original = compiler.parser.previous.?.raw;
    return try Object.String.copy(original[1 .. original.len - 1], compiler.currentChunk().allocator());
}

fn literal(compiler: *Compiler) CompileError!void {
    return switch (compiler.parser.previous.?.tag) {
        .false => compiler.emitOp(.false),
        .true => compiler.emitOp(.true),
        .nil => compiler.emitOp(.nil),
        else => unreachable,
    };
}

fn grouping(compiler: *Compiler) CompileError!void {
    try compiler.expression();
    compiler.parser.consume(.right_paren, "Expect ')' after expression.");
}

fn unary(compiler: *Compiler) CompileError!void {
    const tag = compiler.parser.previous.?.tag;
    try compiler.parsePrecedence(.unary);
    switch (tag) {
        .minus => try compiler.emitOp(.negate),
        .bang => try compiler.emitOp(.not),
        else => unreachable,
    }
}

fn binary(compiler: *Compiler) CompileError!void {
    const tag = compiler.parser.previous.?.tag;

    const rule = getRule(tag);
    try compiler.parsePrecedence(rule.precedence.increment());
    switch (tag) {
        .plus => try compiler.emitOp(.add),
        .minus => try compiler.emitOp(.subtract),
        .star => try compiler.emitOp(.multiply),
        .slash => try compiler.emitOp(.divide),
        .bang_equal => try compiler.emitOpOp(.equal, .not),
        .equal_equal => try compiler.emitOp(.equal),
        .greater => try compiler.emitOp(.greater),
        .greater_equal => try compiler.emitOpOp(.less, .not),
        .less => try compiler.emitOp(.less),
        .less_equal => try compiler.emitOpOp(.greater, .not),
        else => unreachable,
    }
}

pub fn compileAndDump(compiler: *Compiler, source: []const u8, writer: anytype) !void {
    _ = compiler;
    var scanner = Scanner{ .source = source };
    var line: usize = 0;
    while (scanner.next()) |tok| {
        if (tok.line > line) {
            try writer.print("{d:>4} ", .{tok.line});
            line = tok.line;
        } else {
            _ = try writer.write("   | ");
        }
        if (tok.tag == .eof) {
            try writer.print("{s:<12} ''\n", .{@tagName(tok.tag)});
            return;
        }
        try writer.print("{s:<12} '{s}'\n", .{ @tagName(tok.tag), tok.raw });
    }
}

test "Scanner" {
    _ = @import("Scanner.zig");
}

test "Basic compilation" {
    const source =
        \\print 1 + 2;
        \\
    ;
    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var writer = stream.writer();
    var compiler: Compiler = undefined;
    try compiler.compileAndDump(source, &writer);

    const output =
        \\   1 print        'print'
        \\   | number       '1'
        \\   | plus         '+'
        \\   | number       '2'
        \\   | semicolon    ';'
        \\   2 eof          ''
    ;
    try std.testing.expectEqualSlices(u8, output, buf[0..output.len]);
}

test "Chunk compilation" {
    const source =
        \\1 + 2 * 3 / 4
        \\
    ;

    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var compiler: Compiler = undefined;
    const success = try compiler.compile(source, &chunk);
    try std.testing.expectEqual(true, success);

    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.constant), 2,
        @intFromEnum(Chunk.Opcode.multiply),
        @intFromEnum(Chunk.Opcode.constant), 3,
        @intFromEnum(Chunk.Opcode.divide),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, chunk.code[0..chunk.count]);
    try std.testing.expectEqualSlices(values.Value, &[_]values.Value{.{ .number = 1}, .{ .number = 2}, .{ .number = 3}, .{ .number = 4}}, chunk.constants.items);
}
test "Chapter 17 Challenge 1" {
    const source =
        \\(-1 + 2) * 3 - -4
        \\
    ;

    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var compiler: Compiler = undefined;
    const success = try compiler.compile(source, &chunk);
    try std.testing.expectEqual(true, success);

    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.negate),
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.constant), 2,
        @intFromEnum(Chunk.Opcode.multiply),
        @intFromEnum(Chunk.Opcode.constant), 3,
        @intFromEnum(Chunk.Opcode.negate),
        @intFromEnum(Chunk.Opcode.subtract),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, chunk.code[0..chunk.count]);
    try std.testing.expectEqualSlices(values.Value, &[_]values.Value{.{ .number = 1}, .{ .number = 2}, .{ .number = 3}, .{ .number = 4}}, chunk.constants.items);
}
