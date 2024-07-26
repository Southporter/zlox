const std = @import("std");
const Compiler = @This();
const Chunk = @import("Chunk.zig");
const Parser = @import("Parser.zig");
const Scanner = @import("Scanner.zig");
const values = @import("values.zig");
const Object = @import("Object.zig");
const StringPool = @import("StringPool.zig");
const Manager = @import("memory.zig").Manager;
const debug = @import("debug.zig");

const log = std.log.scoped(.compiler);

const MAX_LOCALS = std.math.maxInt(u8) + 1;
const MAX_UPVALUES = std.math.maxInt(u8) + 1;

const Local = struct {
    name: Scanner.Token,
    depth: isize,
    is_captured: bool,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const FunctionType = enum {
    function,
    initializer,
    method,
    script,
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler = null,
    has_superclass: bool = false,
};

fn syntheticToken(name: []const u8) Scanner.Token {
    return .{
        .line = 0,
        .raw = name,
        .tag = .super,
    };
}

manager: *Manager,
parser: Parser = undefined,

function: *Object.Function,
function_type: FunctionType = .script,

locals: [MAX_LOCALS]Local = [_]Local{Local{
    .name = Scanner.Token{ .line = 0, .raw = "", .tag = .@"error" },
    .depth = -1,
    .is_captured = false,
}} ** MAX_LOCALS,
local_count: usize = 0,
upvalues: [MAX_UPVALUES]Upvalue = undefined,
scope_depth: isize = 0,
enclosing: ?*Compiler = null,
inner: ?*Compiler = null,
current_class: ?*ClassCompiler = null,

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
        .infix = call,
        .precedence = .call,
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
    map.set(.dot, ParseRule{
        .infix = dot,
        .precedence = .call,
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
    map.set(.@"and", ParseRule{
        .infix = and_,
        .precedence = .@"and",
    });
    map.set(.@"or", ParseRule{
        .infix = or_,
        .precedence = .@"or",
    });
    map.set(.this, ParseRule{
        .prefix = this,
    });
    map.set(.super, ParseRule{
        .prefix = super,
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
        log.debug("(compiler) Parsing infix: {s} = {?}\n", .{ @tagName(compiler.parser.previous.?.tag), infix_fn });
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
    return &compiler.function.chunk;
}

pub fn init(manager: *Manager, function_type: FunctionType) !Compiler {
    const fun = try manager.allocObject(Object.Function);
    return .{
        .function = fun.as(Object.Function),
        .function_type = function_type,
        .locals = [1]Local{.{
            .name = Scanner.Token{
                .tag = .identifier,
                .raw = if (function_type != .function) "this" else "",
                .line = 0,
            },
            .depth = 0,
            .is_captured = false,
        }} ++ [1]Local{.{ .name = Scanner.Token{ .tag = .@"error", .raw = "", .line = 0 }, .depth = -1, .is_captured = false }} ** (MAX_LOCALS - 1),
        .local_count = 1,
        .manager = manager,
    };
}

pub fn compile(compiler: *Compiler, source: []const u8) !*Object.Function {
    compiler.parser = .{
        .scanner = Scanner{ .source = source },
    };
    compiler.parser.advance();

    while (!compiler.parser.match(.eof)) {
        try compiler.declaration();

        if (compiler.parser.panic_mode) compiler.parser.synchronize();
    }

    const function = try compiler.endCompilation();
    if (compiler.parser.had_error) {
        return error.ParseError;
    } else {
        return function;
    }
}

fn endCompilation(compiler: *Compiler) !*Object.Function {
    try compiler.emitReturn();
    const function = compiler.function;

    if (!compiler.parser.had_error) {
        _ = debug.disassembleChunk(compiler.currentChunk(), if (function.name) |name| name.data else "<script>", debug.Writer) catch {};
    }
    return function;
}

fn emitOp(compiler: *Compiler, op: Chunk.Opcode) !void {
    try compiler.currentChunk().writeOp(op, compiler.parser.previous.?.line);
}
fn emitByte(compiler: *Compiler, byte: u8) !void {
    try compiler.currentChunk().write(byte, compiler.parser.previous.?.line);
}
fn emitOpAndArg(compiler: *Compiler, op: Chunk.Opcode, op_arg: u8) !void {
    try compiler.emitOp(op);
    try compiler.emitByte(op_arg);
}
fn emitOpOp(compiler: *Compiler, op: Chunk.Opcode, op2: Chunk.Opcode) !void {
    try compiler.emitOp(op);
    try compiler.emitOp(op2);
}
fn emitReturn(compiler: *Compiler) !void {
    if (compiler.function_type == .initializer) {
        try compiler.emitOpAndArg(.get_local, 0);
    } else {
        try compiler.emitOp(.nil);
    }
    try compiler.emitOp(.@"return");
}

fn emitJump(compiler: *Compiler, op: Chunk.Opcode) !usize {
    try compiler.emitOp(op);
    try compiler.emitByte(0xff);
    try compiler.emitByte(0xff);
    return compiler.currentChunk().count - 2;
}

fn emitLoop(compiler: *Compiler, start: usize) !void {
    try compiler.emitOp(.loop);

    const offset = compiler.currentChunk().count - start + 2;
    if (offset > std.math.maxInt(u16)) {
        compiler.parser.err("Loop body too large.");
    }

    try compiler.emitByte(@truncate(offset >> 8));
    try compiler.emitByte(@truncate(offset));
}

fn patchJump(compiler: *Compiler, offset: usize) void {
    var chunk = compiler.currentChunk();
    const jump = chunk.count - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        compiler.parser.err("Too much code to jump over");
    }

    chunk.code[offset] = @truncate(jump >> 8);
    chunk.code[offset + 1] = @truncate(jump);
}

fn emitConstant(compiler: *Compiler, constant: values.Value) !void {
    try compiler.emitOpAndArg(.constant, try compiler.makeConstant(constant));
}

fn emitClosure(compiler: *Compiler, function: *Object.Function) !void {
    try compiler.emitOpAndArg(.closure, try compiler.makeConstant(.{ .object = &function.object }));
}

fn makeConstant(compiler: *Compiler, constant: values.Value) !u8 {
    for (compiler.currentChunk().constants.items, 0..) |c, i| {
        if (c.equal(constant)) {
            return @intCast(i);
        }
    }
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
    if (compiler.parser.match(.class)) {
        try compiler.classDeclaration();
    } else if (compiler.parser.match(.@"var")) {
        try compiler.varDeclaration();
    } else if (compiler.parser.match(.fun)) {
        try compiler.funDeclaration();
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

fn funDeclaration(compiler: *Compiler) CompileError!void {
    const global = try compiler.parseVariable("Expect function name.");
    compiler.markInitialized();
    try compiler.compileFunction(.function);
    try compiler.defineVariable(global);
}

fn classDeclaration(compiler: *Compiler) CompileError!void {
    compiler.parser.consume(.identifier, "Expect class name.");
    const class_name: Scanner.Token = compiler.parser.previous.?;
    const name = try compiler.identifierConstant(class_name);
    compiler.declareVariable();

    try compiler.emitOpAndArg(.class, name);
    try compiler.defineVariable(name);

    var class_compiler = ClassCompiler{
        .enclosing = compiler.current_class,
    };
    compiler.current_class = &class_compiler;
    defer compiler.current_class = class_compiler.enclosing;

    if (compiler.parser.match(.less)) {
        compiler.parser.consume(.identifier, "Expect superclass name.");
        try compiler.variable(false);

        if (std.mem.eql(u8, class_name.raw, compiler.parser.previous.?.raw)) {
            compiler.parser.err("A class cannot inherit from itself.");
        }

        compiler.beginScope();
        compiler.addLocal(syntheticToken("super"));
        try compiler.defineVariable(0);
        try compiler.namedVariable(class_name, false);
        try compiler.emitOp(.inherit);
        class_compiler.has_superclass = true;
    }

    try compiler.namedVariable(class_name, false);
    compiler.parser.consume(.left_brace, "Expect '{' before class body.");

    while (!compiler.parser.check(.right_brace) and !compiler.parser.check(.eof)) {
        try compiler.method();
    }
    compiler.parser.consume(.right_brace, "Expect '}' after class body.");
    try compiler.emitOp(.pop);

    if (class_compiler.has_superclass) {
        try compiler.endScope();
    }
}

fn parseVariable(compiler: *Compiler, msg: []const u8) CompileError!u8 {
    compiler.parser.consume(.identifier, msg);
    const name = compiler.parser.previous.?;

    compiler.declareVariable();
    if (compiler.scope_depth > 0) return 0;
    return compiler.identifierConstant(name);
}

fn identifierConstant(compiler: *Compiler, name: Scanner.Token) CompileError!u8 {
    return compiler.makeConstant(.{ .object = try compiler.copyIdentifier(name) });
}

fn defineVariable(compiler: *Compiler, index: u8) CompileError!void {
    if (compiler.scope_depth > 0) {
        compiler.markInitialized();
        return;
    }
    return compiler.emitOpAndArg(.define_global, index);
}

fn markInitialized(compiler: *Compiler) void {
    if (compiler.scope_depth == 0) return;
    compiler.locals[compiler.local_count - 1].depth = compiler.scope_depth;
}

fn declareVariable(compiler: *Compiler) void {
    if (compiler.scope_depth == 0) return;

    const name = compiler.parser.previous.?;
    for (compiler.locals[0..compiler.local_count]) |local| {
        if (local.depth < compiler.scope_depth) {
            break;
        }

        if (name.equal(local.name)) {
            compiler.parser.err("Already a variable with this name in this scope.");
        }
    }
    compiler.addLocal(name);
}

fn addLocal(compiler: *Compiler, name: Scanner.Token) void {
    if (compiler.local_count == MAX_LOCALS) {
        compiler.parser.err("TOO many local variables in function.");
        return;
    }
    defer compiler.local_count += 1;
    compiler.locals[compiler.local_count] = .{
        .name = name,
        .depth = -1,
        .is_captured = false,
    };
}

fn printLocals(compiler: *Compiler) void {
    log.debug("Locals: \n       ", .{});
    for (compiler.locals) |local| {
        if (local.depth == compiler.scope_depth) {
            log.debug("[ {s} ]", .{local.name.raw});
        }
    }
    log.debug("\n", .{});
}

fn resolveLocal(compiler: *Compiler, name: Scanner.Token) isize {
    compiler.printLocals();
    if (compiler.local_count == 0) {
        return -1;
    }
    var i = compiler.local_count;
    while (i > 0) : (i -= 1) {
        const local = compiler.locals[i - 1];
        if (local.name.equal(name)) {
            if (local.depth == -1) {
                compiler.parser.err("Can't read local variable in its own initializer.");
            }
            return @intCast(i - 1);
        }
    }
    return -1;
}

fn resolveUpvalue(compiler: *Compiler, name: Scanner.Token) isize {
    if (compiler.enclosing) |enclosing| {
        const local = enclosing.resolveLocal(name);
        if (local != -1) {
            enclosing.locals[@intCast(local)].is_captured = true;
            return compiler.addUpvalue(@intCast(local), true);
        }

        const upvalue = enclosing.resolveUpvalue(name);
        if (upvalue != -1) {
            return compiler.addUpvalue(@intCast(upvalue), false);
        }
    }
    return -1;
}

fn addUpvalue(compiler: *Compiler, index: u8, is_local: bool) u8 {
    const upvalue_count = compiler.function.upvalue_count;

    for (compiler.upvalues[0..upvalue_count], 0..) |upvalue, i| {
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return @intCast(i);
        }
    }

    if (upvalue_count == MAX_UPVALUES) {
        compiler.parser.err("Too many closure variables in function.");
        return 0;
    }

    compiler.upvalues[upvalue_count].is_local = is_local;
    compiler.upvalues[upvalue_count].index = index;

    compiler.function.upvalue_count += 1;
    return @intCast(upvalue_count);
}

fn namedVariable(compiler: *Compiler, name: Scanner.Token, can_assign: bool) CompileError!void {
    var getOp = Chunk.Opcode.get_global;
    var setOp = Chunk.Opcode.set_global;
    var op_arg = compiler.resolveLocal(name);
    if (op_arg != -1) {
        setOp = .set_local;
        getOp = .get_local;
    } else {
        op_arg = compiler.resolveUpvalue(name);
        if (op_arg != -1) {
            setOp = .set_upvalue;
            getOp = .get_upvalue;
        } else {
            op_arg = try compiler.identifierConstant(name);
        }
    }

    if (can_assign and compiler.parser.match(.equal)) {
        try compiler.expression();
        try compiler.emitOpAndArg(setOp, @intCast(op_arg));
    } else {
        try compiler.emitOpAndArg(getOp, @intCast(op_arg));
    }
}

inline fn beginScope(compiler: *Compiler) void {
    compiler.scope_depth += 1;
}
fn endScope(compiler: *Compiler) CompileError!void {
    compiler.scope_depth -= 1;

    while (compiler.local_count > 0 and compiler.locals[compiler.local_count - 1].depth > compiler.scope_depth) : (compiler.local_count -= 1) {
        if (compiler.locals[compiler.local_count - 1].is_captured) {
            try compiler.emitOp(.close_upvalue);
        } else {
            try compiler.emitOp(.pop);
        }
    }
}

fn statement(compiler: *Compiler) CompileError!void {
    if (compiler.parser.match(.print)) {
        try compiler.printStatement();
    } else if (compiler.parser.match(.@"if")) {
        try compiler.ifStatement();
    } else if (compiler.parser.match(.@"while")) {
        try compiler.whileStatement();
    } else if (compiler.parser.match(.@"for")) {
        try compiler.forStatement();
    } else if (compiler.parser.match(.left_brace)) {
        compiler.beginScope();
        try compiler.block();
        try compiler.endScope();
    } else if (compiler.parser.match(.@"return")) {
        try compiler.returnStatement();
    } else {
        try compiler.expressionStatement();
    }
}

fn printStatement(compiler: *Compiler) CompileError!void {
    try compiler.expression();
    compiler.parser.consume(.semicolon, "Expect ';' after value.");
    try compiler.emitOp(.print);
}

fn ifStatement(compiler: *Compiler) CompileError!void {
    compiler.parser.consume(.left_paren, "Expected '(' after 'if'.");
    try compiler.expression();
    compiler.parser.consume(.right_paren, "Expected ')' after 'if'.");

    const then_jump = try compiler.emitJump(.jump_if_false);
    try compiler.emitOp(.pop);
    try compiler.statement();

    const else_jump = try compiler.emitJump(.jump);
    compiler.patchJump(then_jump);
    try compiler.emitOp(.pop);

    if (compiler.parser.match(.@"else")) try compiler.statement();
    compiler.patchJump(else_jump);
}

fn whileStatement(compiler: *Compiler) CompileError!void {
    const loop_start = compiler.currentChunk().count;
    compiler.parser.consume(.left_paren, "Expected '(' after 'while'.");
    try compiler.expression();
    compiler.parser.consume(.right_paren, "Expected ')' after while clause.");

    const exit_jump = try compiler.emitJump(.jump_if_false);
    try compiler.emitOp(.pop);
    try compiler.statement();
    try compiler.emitLoop(loop_start);
    compiler.patchJump(exit_jump);
    try compiler.emitOp(.pop);
}
fn forStatement(compiler: *Compiler) CompileError!void {
    compiler.beginScope();
    compiler.parser.consume(.left_paren, "Expected '(' after 'for'.");
    if (compiler.parser.match(.semicolon)) {
        // No initializer
    } else if (compiler.parser.match(.@"var")) {
        try compiler.varDeclaration();
    } else {
        try compiler.expressionStatement();
    }
    var loop_start = compiler.currentChunk().count;
    var exit_jump: usize = std.math.maxInt(usize);

    if (!compiler.parser.match(.semicolon)) {
        // Condition clause
        try compiler.expression();
        compiler.parser.consume(.semicolon, "Expected ';'.");

        exit_jump = try compiler.emitJump(.jump_if_false);
        try compiler.emitOp(.pop);
    }

    if (!compiler.parser.match(.semicolon)) {
        // Increment clause
        const body_jump = try compiler.emitJump(.jump);
        const increment_start = compiler.currentChunk().count;
        try compiler.expression();
        try compiler.emitOp(.pop);
        compiler.parser.consume(.right_paren, "Expected ')' after for clause.");

        try compiler.emitLoop(loop_start);
        loop_start = increment_start;
        compiler.patchJump(body_jump);
    }

    try compiler.statement();
    try compiler.emitLoop(loop_start);

    if (exit_jump != std.math.maxInt(usize)) {
        compiler.patchJump(exit_jump);
        try compiler.emitOp(.pop);
    }
    try compiler.endScope();
}

fn block(compiler: *Compiler) CompileError!void {
    while (!(compiler.parser.check(.right_brace) or compiler.parser.check(.eof))) {
        try compiler.declaration();
    }

    compiler.parser.consume(.right_brace, "Expected '}' after block.");
}

fn returnStatement(compiler: *Compiler) CompileError!void {
    if (compiler.parser.match(.semicolon)) {
        try compiler.emitReturn();
    } else {
        if (compiler.function_type == .initializer) {
            compiler.parser.err("Cannot return a value from an initializer.");
        }
        try compiler.expression();
        compiler.parser.consume(.semicolon, "Expect ';' after value.");
        return compiler.emitOp(.@"return");
    }
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
    const name = compiler.parser.previous.?;
    try compiler.namedVariable(name, can_assign);
}

fn this(compiler: *Compiler, _: bool) CompileError!void {
    if (compiler.current_class == null) {
        compiler.parser.err("Can't use 'this' outside of a class");
        return;
    }
    try compiler.variable(false);
}

fn super(compiler: *Compiler, _: bool) CompileError!void {
    if (compiler.current_class == null) {
        compiler.parser.err("Can't use 'super' outside of a class.");
    } else if (!compiler.current_class.?.has_superclass) {
        compiler.parser.err("Can't use 'super' in a class with no superclass.");
    }
    compiler.parser.consume(.dot, "Expected '.' after 'super'.");
    compiler.parser.consume(.identifier, "Expected superclass method name.");
    const name = try compiler.identifierConstant(compiler.parser.previous.?);

    try compiler.namedVariable(syntheticToken("this"), false);
    if (compiler.parser.match(.left_paren)) {
        const arguments = try compiler.argList();
        try compiler.namedVariable(syntheticToken("super"), false);
        try compiler.emitOpAndArg(.super_invoke, name);
        try compiler.emitByte(arguments);
    } else {
        try compiler.namedVariable(syntheticToken("super"), false);
        try compiler.emitOpAndArg(.get_super, name);
    }
}

fn call(compiler: *Compiler, _: bool) CompileError!void {
    const arg_count = try compiler.argList();
    try compiler.emitOpAndArg(.call, arg_count);
}

fn dot(compiler: *Compiler, can_assign: bool) CompileError!void {
    compiler.parser.consume(.identifier, "Expected property name after '.'.");
    const name = try compiler.identifierConstant(compiler.parser.previous.?);

    if (can_assign and compiler.parser.match(.equal)) {
        try compiler.expression();
        try compiler.emitOpAndArg(.set_property, name);
    } else if (compiler.parser.match(.left_paren)) {
        const arg_count = try compiler.argList();
        try compiler.emitOpAndArg(.invoke, name);
        try compiler.emitByte(arg_count);
    } else {
        try compiler.emitOpAndArg(.get_property, name);
    }
}

fn argList(compiler: *Compiler) CompileError!u8 {
    var arg_count: u8 = 0;
    if (!compiler.parser.check(.right_paren)) {
        try compiler.expression();
        arg_count += 1;
        while (compiler.parser.match(.comma)) : (arg_count += 1) {
            try compiler.expression();
            if (arg_count == 255) {
                compiler.parser.err("Can't have more than 255 arguments");
            }
        }
    }
    compiler.parser.consume(.right_paren, "Expected ')' after arguments.");
    return arg_count;
}

fn arg(compiler: *Compiler) CompileError!void {
    compiler.function.arity += 1;
    if (compiler.function.arity > 255) {
        compiler.parser.errorAtCurrent("Can't have more than 255 function parameters.");
    }
    const constant = try compiler.parseVariable("Expect parameter name.");
    try compiler.defineVariable(constant);
}

fn args(compiler: *Compiler) CompileError!void {
    if (compiler.parser.check(.right_paren)) {
        return;
    }
    try compiler.arg();
    while (compiler.parser.match(.comma)) {
        try compiler.arg();
    }
}

fn compileFunction(compiler: *Compiler, function_type: FunctionType) !void {
    var fun_compiler = try Compiler.init(compiler.manager, function_type);
    fun_compiler.enclosing = compiler;
    compiler.inner = &fun_compiler;
    fun_compiler.function.name = (try compiler.copyIdentifier(compiler.parser.previous.?)).as(Object.String);
    fun_compiler.current_class = compiler.current_class;
    fun_compiler.beginScope();
    fun_compiler.parser = compiler.parser;
    fun_compiler.parser.consume(.left_paren, "Expect '(' after function name.");

    try fun_compiler.args();

    fun_compiler.parser.consume(.right_paren, "Expect ')' after function parameters.");
    fun_compiler.parser.consume(.left_brace, "Expect '{' before function body.");
    try fun_compiler.block();

    const function = try fun_compiler.endCompilation();
    compiler.parser = fun_compiler.parser;
    try compiler.emitClosure(function);

    for (fun_compiler.upvalues[0..function.upvalue_count]) |upvalue| {
        try compiler.emitByte(if (upvalue.is_local) 1 else 0);
        try compiler.emitByte(upvalue.index);
    }
    compiler.inner = null;
}

fn method(compiler: *Compiler) !void {
    compiler.parser.consume(.identifier, "Expected method name.");
    const name_raw = compiler.parser.previous.?;
    const name = try compiler.identifierConstant(name_raw);

    const method_type: FunctionType = if (std.mem.eql(u8, "init", name_raw.raw)) .initializer else .method;

    try compiler.compileFunction(method_type);
    try compiler.emitOpAndArg(.method, name);
}

fn copyString(compiler: *Compiler) !*Object {
    const original = compiler.parser.previous.?.raw;
    return compiler.manager.copy(compiler.parser.previous.?.raw[1 .. original.len - 1]);
}

fn copyIdentifier(compiler: *Compiler, name: Scanner.Token) !*Object {
    return compiler.manager.copy(name.raw);
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

fn and_(compiler: *Compiler, _: bool) CompileError!void {
    const end_jump = try compiler.emitJump(.jump_if_false);
    try compiler.emitOp(.pop);
    try compiler.parsePrecedence(.@"and");

    compiler.patchJump(end_jump);
}

fn or_(compiler: *Compiler, _: bool) CompileError!void {
    const else_jump = try compiler.emitJump(.jump_if_false);
    const end_jump = try compiler.emitJump(.jump);

    compiler.patchJump(else_jump);
    try compiler.emitOp(.pop);

    try compiler.parsePrecedence(.@"or");
    compiler.patchJump(end_jump);
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

    var compiler: Compiler = undefined;
    compiler.function = try Object.Function.init(std.testing.allocator);
    defer compiler.function.object.deinit(std.testing.allocator);
    const func = try compiler.compile(source);

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
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
    try std.testing.expectEqualSlices(values.Value, &[_]values.Value{.{ .number = 1}, .{ .number = 2}, .{ .number = 3}, .{ .number = 4}}, func.chunk.constants.items);
}
test "Chapter 17 Challenge 1" {
    const source =
        \\(-1 + 2) * 3 - -4;
        \\
    ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);
    const func = try compiler.compile(source, );

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
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
    try std.testing.expectEqualSlices(values.Value, &[_]values.Value{.{ .number = 1}, .{ .number = 2}, .{ .number = 3}, .{ .number = 4}}, func.chunk.constants.items);
}

test "Chapter 21: Globals" {
    const source =
        \\ var empty;
        \\ var breakfast = "beignets";
        \\ var beverage = "cafe au lait";
        \\ breakfast = "beignets with " + beverage;
        \\
        \\ return breakfast;
        \\
    ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const func = try compiler.compile(source);

    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.constant), 2,
        @intFromEnum(Chunk.Opcode.define_global), 1,
        @intFromEnum(Chunk.Opcode.constant), 4,
        @intFromEnum(Chunk.Opcode.define_global), 3,
        @intFromEnum(Chunk.Opcode.constant), 5,
        @intFromEnum(Chunk.Opcode.get_global), 3,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.set_global), 1,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.get_global), 1,
        @intFromEnum(Chunk.Opcode.@"return"),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
    try std.testing.expectEqualStrings("empty", func.chunk.constants.items[0].object.as(Object.String).data);
    try std.testing.expectEqualStrings("breakfast", func.chunk.constants.items[1].object.as(Object.String).data);
    try std.testing.expectEqualStrings("beignets",func. chunk.constants.items[2].object.as(Object.String).data);
    try std.testing.expectEqualStrings("beverage", func.chunk.constants.items[3].object.as(Object.String).data);
    try std.testing.expectEqualStrings("cafe au lait", func.chunk.constants.items[4].object.as(Object.String).data);
}

// test "Chapter 22: Conflicting locals" {
//     const source =
//         \\ {
//         \\   var a = "test";
//         \\   var a = "another";
//         \\ }
//         ;

//     var manager: Manager = undefined;
//     manager.init(std.testing.allocator);
//     defer manager.deinit();

//     var compiler = try Compiler.init(&manager, .script);

//     const res = try compiler.compile(source);
//     try std.testing.expectError(error.ParseError, res);
// }

// test "Chapter 22: Shadow access" {
//     const source =
//         \\ {
//         \\   var a = "test";
//         \\   {
//         \\      var a = a;
//         \\   }
//         \\ }
//         ;

//     var manager: Manager = undefined;
//     manager.init(std.testing.allocator);
//     defer manager.deinit();

//     var compiler = try Compiler.init(&manager, .script);

//     const result = compiler.compile(source);
//     try std.testing.expectError(error.ParseError, result);
// }

test "Chapter 22: variables" {
    const source =
        \\ {
        \\ var a = "test";
        \\ a = a + " is successful";
        \\ }
        ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const func = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.get_local), 1,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.set_local), 1,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
    try std.testing.expectEqualStrings("test", func.chunk.constants.items[0].object.as(Object.String).data);
    try std.testing.expectEqualStrings(" is successful", func.chunk.constants.items[1].object.as(Object.String).data);
}

test "Chapter 22: block global access" {
    const source =
        \\ var global;
        \\ {
        \\     global = "test";
        \\ }
        \\ print global;
        \\
        ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const func = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.set_global), 0,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.print),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
    try std.testing.expectEqualStrings("global", func.chunk.constants.items[0].object.as(Object.String).data);
    try std.testing.expectEqualStrings("test", func.chunk.constants.items[1].object.as(Object.String).data);
}


test "Chapter 23: If statements" {
    const source =
        \\ if (true) { 1 + 2; }
        \\ if (false) { 1 + 2; } else { 1 * 2; }
        \\
        ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const func = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.true),
        @intFromEnum(Chunk.Opcode.jump_if_false), 0, 10,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.jump), 0, 1,
        @intFromEnum(Chunk.Opcode.pop),
        // END FIRST IF STATEMENT
        @intFromEnum(Chunk.Opcode.false),
        @intFromEnum(Chunk.Opcode.jump_if_false), 0, 10,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.jump), 0, 7,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.multiply),
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
    try std.testing.expectEqual(1, func.chunk.constants.items[0].number);
    try std.testing.expectEqual(2, func.chunk.constants.items[1].number);
}

test "Chapter 23: while statements" {
    const source =
        \\ var i = 0;
        \\ while (i < 10) {
        \\    print i;
        \\    i = i + 1;
        \\ }
        \\
        ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const func = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.define_global), 0,
        // START WHILE
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.constant), 2,
        @intFromEnum(Chunk.Opcode.less),
        @intFromEnum(Chunk.Opcode.jump_if_false), 0, 15,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.print),
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.constant), 3,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.set_global), 0,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.loop), 0, 0x17,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
}

test "Chapter 23: for statements" {
    const source =
        \\ for (var i = 0; i < 10; i = i + 1) {
        \\     print i;
        \\ }
        \\
        ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const func = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.get_local), 1,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.less),
        @intFromEnum(Chunk.Opcode.jump_if_false), 0, 0x15,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.jump), 0, 11,
        @intFromEnum(Chunk.Opcode.get_local), 1,
        @intFromEnum(Chunk.Opcode.constant), 2,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.set_local), 1,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.loop), 0, 0x17,
        @intFromEnum(Chunk.Opcode.get_local), 1,
        @intFromEnum(Chunk.Opcode.print),
        @intFromEnum(Chunk.Opcode.loop), 0, 0x11,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
}

test "Chapter 24: function declaration" {
    const source =
        \\ fun main(args) {
        \\     print args;
        \\ }
        \\
        \\ print main;
        \\
        ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const func = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.closure), 1,
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.print),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func.chunk.code[0..func.chunk.count]);
    const func_decl = func.chunk.constants.items[1].object.as(Object.Function);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.get_local), 1,
        @intFromEnum(Chunk.Opcode.print),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, func_decl.chunk.code[0..func_decl.chunk.count]);

}

test "Chapter 25: closure upvalues" {
    const source =
        \\ fun outer() {
        \\   var a = 1;
        \\   var b = 2;
        \\   fun middle() {
        \\     var c = 3;
        \\     var d = 4;
        \\     fun inner() {
        \\       print a + c + b + d;
        \\     }
        \\   }
        \\ }
        \\
    ;

    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const script = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.closure), 1,
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, script.chunk.code[0..script.chunk.count]);
    const outer = script.chunk.constants.items[1].object.as(Object.Function);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.closure), 2,
            // Upvalues
            1, 1,
            1, 2,
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, outer.chunk.code[0..outer.chunk.count]);
    const middle = outer.chunk.constants.items[2].object.as(Object.Function);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.constant), 1,
        @intFromEnum(Chunk.Opcode.closure), 2,
            // Upvalues
            0, 0,
            1, 1,
            0, 1,
            1, 2,
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, middle.chunk.code[0..middle.chunk.count]);
    const inner = middle.chunk.constants.items[2].object.as(Object.Function);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.get_upvalue), 0,
        @intFromEnum(Chunk.Opcode.get_upvalue), 1,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.get_upvalue), 2,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.get_upvalue), 3,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.print),

        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, inner.chunk.code[0..inner.chunk.count]);

}

test "Chapter 27: classes and instances" {
    const source =
        \\ class Test {}
        \\
        \\ var test = Test();
        \\
        \\ test.success = true;
        \\
    ;
    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const script = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.class), 0,
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.call), 0,
        @intFromEnum(Chunk.Opcode.define_global), 1,
        @intFromEnum(Chunk.Opcode.get_global), 1,
        @intFromEnum(Chunk.Opcode.true),
        @intFromEnum(Chunk.Opcode.set_property), 2,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, script.chunk.code[0..script.chunk.count]);

}
test "Chapter 28: methods" {
    const source =
        \\ class Test {
        \\    run() {}
        \\ }
        \\
        \\ {
        \\    class Local{
        \\       scope() {
        \\          return 1;
        \\       }
        \\    }
        \\ }
        \\
    ;
    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const script = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.class), 0,
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.closure), 2,
        @intFromEnum(Chunk.Opcode.method), 1,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.class), 3,
        @intFromEnum(Chunk.Opcode.get_local), 1,
        @intFromEnum(Chunk.Opcode.closure), 5,
        @intFromEnum(Chunk.Opcode.method), 4,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, script.chunk.code[0..script.chunk.count]);

}

test "Chapter 28: initializer" {
    const source =
        \\ class CoffeeMaker {
        \\   init(coffee) {
        \\     this.coffee = coffee;
        \\   }
        \\
        \\   brew() {
        \\     print "Enjoy your cup of " + this.coffee;
\\
        \\     // No reusing the grounds!
        \\     this.coffee = nil;
        \\   }
        \\ }
\\
        \\ var maker = CoffeeMaker("coffee and chicory");
        \\ maker.brew();
        \\
    ;
    var manager: Manager = undefined;
    manager.init(std.testing.allocator);
    defer manager.deinit();

    var compiler = try Compiler.init(&manager, .script);

    const script = try compiler.compile(source);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.class), 0,
        @intFromEnum(Chunk.Opcode.define_global), 0,
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.closure), 2,
        @intFromEnum(Chunk.Opcode.method), 1,
        @intFromEnum(Chunk.Opcode.closure), 4,
        @intFromEnum(Chunk.Opcode.method), 3,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.get_global), 0,
        @intFromEnum(Chunk.Opcode.constant), 6,
        @intFromEnum(Chunk.Opcode.call), 1,
        @intFromEnum(Chunk.Opcode.define_global), 5,
        @intFromEnum(Chunk.Opcode.get_global), 5,
        @intFromEnum(Chunk.Opcode.invoke), 3,
        @intFromEnum(Chunk.Opcode.@"return"),
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, script.chunk.code[0..script.chunk.count]);
    const init_fun = script.chunk.constants.items[2].object.as(Object.Function);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.get_local), 0,
        @intFromEnum(Chunk.Opcode.get_local), 1,
        @intFromEnum(Chunk.Opcode.set_property), 0,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.get_local), 0,
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, init_fun.chunk.code[0..init_fun.chunk.count]);
    const brew_fun = script.chunk.constants.items[4].object.as(Object.Function);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        // zig fmt: off
        @intFromEnum(Chunk.Opcode.constant), 0,
        @intFromEnum(Chunk.Opcode.get_local), 0,
        @intFromEnum(Chunk.Opcode.get_property), 1,
        @intFromEnum(Chunk.Opcode.add),
        @intFromEnum(Chunk.Opcode.print),
        @intFromEnum(Chunk.Opcode.get_local), 0,
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.set_property), 1,
        @intFromEnum(Chunk.Opcode.pop),
        @intFromEnum(Chunk.Opcode.nil),
        @intFromEnum(Chunk.Opcode.@"return"),
        // zig fmt: on
    }, brew_fun.chunk.code[0..brew_fun.chunk.count]);
}
