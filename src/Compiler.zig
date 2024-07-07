const std = @import("std");
const Compiler = @This();
const Chunk = @import("Chunk.zig");
const Parser = @import("Parser.zig");
const Scanner = @import("Scanner.zig");
const values = @import("values.zig");
const Object = @import("Object.zig");
const StringPool = @import("StringPool.zig");

const log = std.log.scoped(.compiler);

parser: Parser,
chunk: *Chunk = undefined,
string_pool: *StringPool,

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

    fn lessOrEqual(current: Precedence, next: Precedence) bool {
        return @intFromEnum(current) <= @intFromEnum(next);
    }
};

pub const CompileError = error{ConstantOverflow} || std.mem.Allocator.Error || std.fmt.ParseFloatError;

const ParseFn = *const fn (*Compiler, bool) CompileError!void;

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
    map.set(.identifier, ParseRule{
        .prefix = variable,
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
    const can_assign = precedence.lessOrEqual(.assignment);
    log.debug("(compiler) Can assign? {} assignment > {s}\n", .{ can_assign, @tagName(precedence) });
    const prefix_fn = getRule(compiler.parser.previous.?.tag).prefix;
    log.debug("(compiler) Parsing prefix: {s} = {any}\n", .{ @tagName(compiler.parser.previous.?.tag), prefix_fn });
    if (prefix_fn) |prefix| {
        try prefix(compiler, can_assign);
    } else {
        compiler.parser.err("Expect expression.");
        return;
    }

    while (getRule(compiler.parser.current.?.tag).precedence.greater(precedence)) {
        compiler.parser.advance();
        const infix_fn = getRule(compiler.parser.previous.?.tag).infix;
        log.debug("(compiler) Parsing infix: {s} = {any}\n", .{ @tagName(compiler.parser.previous.?.tag), infix_fn });
        if (infix_fn) |infix| {
            try infix(compiler, can_assign);
        } else if (can_assign and compiler.parser.match(.equal)) {
            compiler.parser.err("Invalid assignment target.");
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

    while (!compiler.parser.match(.eof)) {
        try compiler.declaration();

        if (compiler.parser.panic_mode) compiler.parser.synchronize();
    }

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

fn declaration(compiler: *Compiler) CompileError!void {
    if (compiler.parser.match(.@"var")) {
        log.debug("Parsing a var declaration\n", .{});
        try compiler.varDeclaration();
    } else {
        try compiler.statement();
    }
}

fn varDeclaration(compiler: *Compiler) CompileError!void {
    const global = try compiler.parseVariable("Expect variable name.");

    if (compiler.parser.match(.equal)) {
        try compiler.expression();
    } else {
        try compiler.emitOp(.nil);
    }
    compiler.parser.consume(.semicolon, "Expect ';' after variable declaration");
    try compiler.defineVariable(global);
}

fn parseVariable(compiler: *Compiler, msg: []const u8) CompileError!u8 {
    compiler.parser.consume(.identifier, msg);
    return compiler.identifierConstant();
}

fn identifierConstant(compiler: *Compiler) CompileError!u8 {
    return compiler.makeConstant(.{ .object = try compiler.copyIdentifier() });
}

fn defineVariable(compiler: *Compiler, index: u8) CompileError!void {
    return compiler.emitOpAndArg(.define_global, index);
}

fn namedVariable(compiler: *Compiler, can_assign: bool) CompileError!void {
    const arg = try compiler.identifierConstant();

    if (can_assign and compiler.parser.match(.equal)) {
        try compiler.expression();
        try compiler.emitOpAndArg(.set_global, arg);
    } else {
        try compiler.emitOpAndArg(.get_global, arg);
    }
}

fn statement(compiler: *Compiler) CompileError!void {
    if (compiler.parser.match(.print)) {
        try compiler.printStatement();
    } else if (compiler.parser.match(.@"return")) {
        try compiler.returnStatement();
    } else {
        try compiler.expressionStatement();
    }
}

fn printStatement(compiler: *Compiler) CompileError!void {
    try compiler.expression();
    compiler.parser.consume(.semicolon, "Expect ';' after value.");
    return compiler.emitOp(.print);
}

fn returnStatement(compiler: *Compiler) CompileError!void {
    try compiler.expression();
    compiler.parser.consume(.semicolon, "Expect ';' after value.");
    return compiler.emitOp(.@"return");
}

fn expressionStatement(compiler: *Compiler) CompileError!void {
    try compiler.expression();
    compiler.parser.consume(.semicolon, "Expect ';' after expression.");
    return compiler.emitOp(.pop);
}

fn expression(compiler: *Compiler) CompileError!void {
    try compiler.parsePrecedence(.assignment);
}

fn number(compiler: *Compiler, _: bool) CompileError!void {
    const val = try std.fmt.parseFloat(f64, compiler.parser.previous.?.raw);
    try compiler.emitConstant(.{ .number = val });
}

fn string(compiler: *Compiler, _: bool) CompileError!void {
    try compiler.emitConstant(.{ .object = try compiler.copyString() });
}

fn variable(compiler: *Compiler, can_assign: bool) CompileError!void {
    log.debug("Parsing variable identifier: can assign? {}\n", .{can_assign});
    try compiler.namedVariable(can_assign);
}

fn copyString(compiler: *Compiler) !*Object {
    const original = compiler.parser.previous.?.raw;
    const interned = compiler.string_pool.find(original[1 .. original.len - 1]);
    if (interned) |i| {
        return &i.object;
    }
    const res = try Object.String.copy(original[1 .. original.len - 1], compiler.currentChunk().allocator());
    try compiler.string_pool.set(res.asString(), values.NIL_VAL);
    return res;
}

fn copyIdentifier(compiler: *Compiler) !*Object {
    const original = compiler.parser.previous.?.raw;
    log.debug("Copying identifier: {s}\n", .{original});
    const interned = compiler.string_pool.find(original);
    if (interned) |i| {
        return &i.object;
    }
    const res = try Object.String.copy(original, compiler.currentChunk().allocator());
    try compiler.string_pool.set(res.asString(), values.NIL_VAL);
    return res;
}

fn literal(compiler: *Compiler, _: bool) CompileError!void {
    return switch (compiler.parser.previous.?.tag) {
        .false => compiler.emitOp(.false),
        .true => compiler.emitOp(.true),
        .nil => compiler.emitOp(.nil),
        else => unreachable,
    };
}

fn grouping(compiler: *Compiler, _: bool) CompileError!void {
    try compiler.expression();
    compiler.parser.consume(.right_paren, "Expect ')' after expression.");
}

fn unary(compiler: *Compiler, _: bool) CompileError!void {
    const tag = compiler.parser.previous.?.tag;
    try compiler.parsePrecedence(.unary);
    switch (tag) {
        .minus => try compiler.emitOp(.negate),
        .bang => try compiler.emitOp(.not),
        else => unreachable,
    }
}

fn binary(compiler: *Compiler, _: bool) CompileError!void {
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
        \\1 + 2 * 3 / 4;
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
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, chunk.code[0..chunk.count]);
    try std.testing.expectEqualSlices(values.Value, &[_]values.Value{.{ .number = 1}, .{ .number = 2}, .{ .number = 3}, .{ .number = 4}}, chunk.constants.items);
}
test "Chapter 17 Challenge 1" {
    const source =
        \\(-1 + 2) * 3 - -4;
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
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, chunk.code[0..chunk.count]);
    try std.testing.expectEqualSlices(values.Value, &[_]values.Value{.{ .number = 1}, .{ .number = 2}, .{ .number = 3}, .{ .number = 4}}, chunk.constants.items);
}

test "Chapter 21: Globals" {
    const source =
        \\ var breakfast = "beignets";
        \\ var beverage = "cafe au lait";
        \\ breakfast = "beignets with " + beverage;
        \\
        \\ return breakfast;
        \\
    ;

    var chunk = try Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var compiler: Compiler = undefined;
    var pool = StringPool.init(std.testing.allocator);
    defer pool.deinit();
    compiler.string_pool = &pool;

    const success = try compiler.compile(source, &chunk);
    try std.testing.expectEqual(true, success);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.constant), 3,
        @intFromEnum(Chunk.Opcode.define_global), 2,
        @intFromEnum(Chunk.Opcode.constant), 5,
        @intFromEnum(Chunk.Opcode.get_global), 6,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.set_global), 4,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.get_global), 7,
        @intFromEnum(Chunk.Opcode.@"return"),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, chunk.code[0..chunk.count]);
    try std.testing.expectEqualStrings("breakfast", chunk.constants.items[0].object.asString().data);
    try std.testing.expectEqualStrings("beignets", chunk.constants.items[1].object.asString().data);
    try std.testing.expectEqualStrings("beverage", chunk.constants.items[2].object.asString().data);
    try std.testing.expectEqualStrings("cafe au lait", chunk.constants.items[3].object.asString().data);
}
