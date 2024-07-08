const std = @import("std");
const Scanner = @This();
const isDigit = std.ascii.isDigit;
fn isAlpha(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

source: []const u8,
start: usize = 0,
current: usize = 0,
line: usize = 1,

pub const TokenType = enum {
    // Single-character tokens.
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    // one or two character tokens.
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    // literals.
    identifier,
    string,
    number,
    // keywords.
    @"and",
    class,
    @"else",
    false,
    @"for",
    fun,
    @"if",
    nil,
    @"or",
    print,
    @"return",
    super,
    this,
    true,
    @"var",
    @"while",

    @"error",
    eof,
};

pub const Token = struct {
    line: usize,
    tag: TokenType,
    raw: []const u8,

    pub fn equal(a: *const Token, b: Token) bool {
        if (a.raw.len != b.raw.len) return false;
        return std.mem.eql(u8, a.raw, b.raw);
    }
};
fn makeToken(scanner: *Scanner, tag: TokenType) Token {
    return .{
        .tag = tag,
        .line = scanner.line,
        .raw = scanner.source[scanner.start..scanner.current],
    };
}
fn errorToken(scanner: *Scanner, message: []const u8) Token {
    return .{
        .tag = .@"error",
        .line = scanner.line,
        .raw = message,
    };
}

pub fn next(scanner: *Scanner) ?Token {
    scanner.skipWhitespace();

    scanner.start = scanner.current;
    if (scanner.isAtEnd()) return scanner.makeToken(.eof);

    const c = scanner.advance();
    if (isDigit(c)) return scanner.number();
    if (isAlpha(c)) return scanner.identifier();

    return switch (c) {
        '(' => scanner.makeToken(.left_paren),
        ')' => scanner.makeToken(.right_paren),
        '{' => scanner.makeToken(.left_brace),
        '}' => scanner.makeToken(.right_brace),
        ';' => scanner.makeToken(.semicolon),
        ',' => scanner.makeToken(.comma),
        '.' => scanner.makeToken(.dot),
        '-' => scanner.makeToken(.minus),
        '+' => scanner.makeToken(.plus),
        '*' => scanner.makeToken(.star),
        '/' => scanner.makeToken(.slash),
        '!' => scanner.makeToken(if (scanner.match('=')) .bang_equal else .bang),
        '=' => scanner.makeToken(if (scanner.match('=')) .equal_equal else .equal),
        '<' => scanner.makeToken(if (scanner.match('=')) .less_equal else .less),
        '>' => scanner.makeToken(if (scanner.match('=')) .greater_equal else .greater),
        '"' => scanner.string(),
        else => scanner.errorToken("Unexpected character"),
    };
}

fn advance(scanner: *Scanner) u8 {
    defer scanner.current += 1;
    return scanner.source[scanner.current];
}

fn match(scanner: *Scanner, char: u8) bool {
    if (scanner.isAtEnd()) return false;
    if (scanner.source[scanner.current] != char) return false;
    scanner.current += 1;
    return true;
}

fn peek(scanner: *Scanner) u8 {
    return scanner.source[scanner.current];
}
fn peekNext(scanner: *Scanner) u8 {
    if (scanner.isAtEnd()) {
        return 0;
    }
    return scanner.source[scanner.current + 1];
}

fn isAtEnd(scanner: *Scanner) bool {
    return scanner.current >= scanner.source.len;
}

fn skipWhitespace(scanner: *Scanner) void {
    while (true) {
        if (scanner.isAtEnd()) return;
        const c = scanner.peek();
        switch (c) {
            ' ', '\t', '\r' => _ = scanner.advance(),
            '\n' => {
                scanner.line += 1;
                _ = scanner.advance();
            },
            '/' => {
                if (scanner.peekNext() == '/') {
                    while (scanner.peek() != '\n' and !scanner.isAtEnd()) : (_ = scanner.advance()) {}
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

fn string(scanner: *Scanner) Token {
    while (scanner.peek() != '"' and !scanner.isAtEnd()) : (_ = scanner.advance()) {
        if (scanner.peek() == '\n') {
            scanner.line += 1;
        }
    }
    if (scanner.isAtEnd()) {
        return scanner.errorToken("Unterminated string");
    }
    _ = scanner.advance();
    return scanner.makeToken(.string);
}

fn number(scanner: *Scanner) Token {
    while (isDigit(scanner.peek())) : (_ = scanner.advance()) {}

    if (scanner.peek() == '.' and isDigit(scanner.peekNext())) {
        _ = scanner.advance();

        while (isDigit(scanner.peek())) : (_ = scanner.advance()) {}
    }
    return scanner.makeToken(.number);
}

fn identifier(scanner: *Scanner) Token {
    while (isAlpha(scanner.peek()) or isDigit(scanner.peek())) : (_ = scanner.advance()) {}
    return scanner.makeToken(scanner.identifierType());
}
fn identifierType(scanner: *Scanner) TokenType {
    return switch (scanner.source[scanner.start]) {
        'a' => scanner.checkKeyword(1, "nd", .@"and"),
        'c' => scanner.checkKeyword(1, "lass", .class),
        'e' => scanner.checkKeyword(1, "lse", .@"else"),
        'f' => {
            if (scanner.current - scanner.start > 1) {
                return switch (scanner.source[scanner.start + 1]) {
                    'a' => scanner.checkKeyword(2, "lse", .false),
                    'o' => scanner.checkKeyword(2, "r", .@"for"),
                    'u' => scanner.checkKeyword(2, "n", .fun),
                    else => .identifier,
                };
            }
            return .identifier;
        },
        'i' => scanner.checkKeyword(1, "f", .@"if"),
        'n' => scanner.checkKeyword(1, "il", .nil),
        'o' => scanner.checkKeyword(1, "r", .@"or"),
        'p' => scanner.checkKeyword(1, "rint", .print),
        'r' => scanner.checkKeyword(1, "eturn", .@"return"),
        's' => scanner.checkKeyword(1, "uper", .super),
        't' => {
            if (scanner.current - scanner.start > 1) {
                return switch (scanner.source[scanner.start + 1]) {
                    'h' => scanner.checkKeyword(2, "is", .this),
                    'r' => scanner.checkKeyword(2, "ue", .true),
                    else => .identifier,
                };
            }
            return .identifier;
        },
        'v' => scanner.checkKeyword(1, "ar", .@"var"),
        'w' => scanner.checkKeyword(1, "hile", .@"while"),
        else => .identifier,
    };
}

fn checkKeyword(scanner: *Scanner, start: usize, rest: []const u8, tag: TokenType) TokenType {
    const total_len = start + rest.len;
    if (scanner.current - scanner.start == total_len and
        std.mem.eql(u8, scanner.source[scanner.start + start .. scanner.start + total_len], rest))
    {
        return tag;
    }
    return .identifier;
}

test "checkKeyword" {
    var scanner = Scanner{ .source = 
    \\print
    \\and
    \\super
    \\false
    \\
    };
    scanner.current = 5;
    try std.testing.expectEqual(.print, scanner.checkKeyword(1, "rint", .print));

    scanner.start = 6;
    scanner.current = 9;
    try std.testing.expectEqual(.@"and", scanner.checkKeyword(1, "nd", .@"and"));

    scanner.start = 10;
    scanner.current = 15;
    try std.testing.expectEqual(.super, scanner.checkKeyword(1, "uper", .super));

    scanner.start = 16;
    scanner.current = 21;
    try std.testing.expectEqual(.false, scanner.checkKeyword(2, "lse", .false));
}

test "Tokens" {
    const source =
        \\(){};,.-+*/! != = == < <= > >=
        \\"test"
        \\"multi
        \\line
        \\test"
        \\10 3.14
        \\pi
        \\return print
        \\
    ;

    var scanner = Scanner{ .source = source };
    const expected = [_]TokenType{ .left_paren, .right_paren, .left_brace, .right_brace, .semicolon, .comma, .dot, .minus, .plus, .star, .slash, .bang, .bang_equal, .equal, .equal_equal, .less, .less_equal, .greater, .greater_equal };

    for (expected) |tag| {
        const tok = scanner.next();
        try std.testing.expectEqual(tag, tok.?.tag);
    }
    const str = scanner.next();
    try std.testing.expectEqualDeep(Token{
        .line = 2,
        .raw =
        \\"test"
        ,
        .tag = .string,
    }, str.?);
    const multiline_string = scanner.next();
    try std.testing.expectEqualDeep(Token{
        .line = 5,
        .raw =
        \\"multi
        \\line
        \\test"
        ,
        .tag = .string,
    }, multiline_string.?);

    const ten = scanner.next();
    try std.testing.expectEqual(.number, ten.?.tag);
    try std.testing.expectEqual(10, try std.fmt.parseInt(usize, ten.?.raw, 10));

    const pi = scanner.next();
    try std.testing.expectEqual(.number, pi.?.tag);
    try std.testing.expectEqual(3.14, try std.fmt.parseFloat(f32, pi.?.raw));

    const pi_var = scanner.next();
    try std.testing.expectEqual(.identifier, pi_var.?.tag);
    try std.testing.expectEqualSlices(u8, "pi", pi_var.?.raw);
    const ret = scanner.next();
    try std.testing.expectEqual(.@"return", ret.?.tag);
    const print = scanner.next();
    try std.testing.expectEqual(.print, print.?.tag);
}
