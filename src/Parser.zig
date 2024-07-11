const std = @import("std");
const Parser = @This();
const Scanner = @import("Scanner.zig");

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

pub fn match(parser: *Parser, tag: Scanner.TokenType) bool {
    if (!parser.check(tag)) return false;
    parser.advance();
    return true;
}

pub fn check(parser: *Parser, tag: Scanner.TokenType) bool {
    return parser.current.?.tag == tag;
}
pub fn synchronize(parser: *Parser) void {
    parser.panic_mode = false;

    while (parser.current.?.tag != .eof) {
        if (parser.previous.?.tag == .semicolon) return;
        switch (parser.previous.?.tag) {
            .class, .fun, .@"var", .@"for", .@"if", .@"while", .print, .@"return" => return,
            else => {},
        }
        parser.advance();
    }
}

pub fn errorAtCurrent(parser: *Parser, message: []const u8) void {
    parser.errorAt(parser.current.?, message);
}

fn errorAt(parser: *Parser, token: Scanner.Token, message: []const u8) void {
    if (parser.panic_mode) return;
    parser.panic_mode = true;

    const logger = if (@import("builtin").is_test) log.warn else log.err;

    logger("[line {d}] Error", .{token.line});
    if (token.tag == .eof) {
        logger(" at end", .{});
    } else if (token.tag == .@"error") {
        // Nothing.
    } else {
        logger(" at '{s}'", .{token.raw});
    }

    logger(": {s}\n", .{message});
    parser.had_error = true;
}

pub fn err(parser: *Parser, message: []const u8) void {
    parser.errorAt(parser.previous.?, message);
}
