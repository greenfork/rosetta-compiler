const std = @import("std");
const fmt = std.fmt;
const testing = std.testing;
const p = std.debug.print;

pub const TokenType = enum {
    unknown,
    multiply,
    divide,
    mod,
    add,
    subtract,
    negate,
    less,
    less_equal,
    greater,
    greater_equal,
    equal,
    not_equal,
    not,
    assign,
    bool_and,
    bool_or,
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    semicolon,
    comma,
    kw_if,
    kw_else,
    kw_while,
    kw_print,
    kw_putc,
    identifier,
    integer,
    string,
    eof,

    pub fn toString(self: @This()) []const u8 {
        return switch (self) {
            .unknown => "UNKNOWN",
            .multiply => "Op_multiply",
            .divide => "Op_divide",
            .mod => "Op_mod",
            .add => "Op_add",
            .subtract => "Op_subtract",
            .negate => "Op_negate",
            .less => "Op_less",
            .less_equal => "Op_lessequal",
            .greater => "Op_greater",
            .greater_equal => "Op_greaterequal",
            .equal => "Op_equal",
            .not_equal => "Op_notequal",
            .not => "Op_not",
            .assign => "Op_assign",
            .bool_and => "Op_and",
            .bool_or => "Op_or",
            .left_paren => "LeftParen",
            .right_paren => "RightParen",
            .left_brace => "LeftBrace",
            .right_brace => "RightBrace",
            .semicolon => "Semicolon",
            .comma => "Comma",
            .kw_if => "Keyword_if",
            .kw_else => "Keyword_else",
            .kw_while => "Keyword_while",
            .kw_print => "Keyword_print",
            .kw_putc => "Keyword_putc",
            .identifier => "Identifier",
            .integer => "Integer",
            .string => "String",
            .eof => "End_of_input",
        };
    }
};

pub const TokenValue = union(enum) {
    ident: []const u8,
    intlit: i64,
    intchar: u8,
    string: []const u8,
};

pub const Token = struct {
    line: usize,
    col: usize,
    typ: TokenType = .unknown,
    value: ?TokenValue = null,

    fn build(lexer: Lexer, typ: TokenType, value: ?TokenValue) Token {
        return Token{
            .line = lexer.line,
            .col = lexer.col,
            .typ = typ,
            .value = value,
        };
    }
};

pub const LexerError = error{
    EmptyCharacterConstant,
    UnknownEscapeSequence,
    MulticharacterConstant,
    EndOfFileInComment,
    EndOfFileInString,
    EnfOfLineInString,
    UnrecognizedCharacter,
    InvalidNumber,
};

pub const Lexer = struct {
    content: []const u8,
    line: usize,
    col: usize,
    offset: usize,
    start: bool = true,

    const Self = @This();

    pub fn init(content: []const u8) Lexer {
        return Lexer{
            .content = content,
            .line = 1,
            .col = 1,
            .offset = 0,
        };
    }

    fn print(self: Self) void {
        std.debug.print(
            "Lexer: line({d}), column({d}), offset({d})\n",
            .{ self.line, self.col, self.offset },
        );
    }

    pub fn curr(self: Self) u8 {
        return self.content[self.offset];
    }

    pub fn next(self: *Self) ?u8 {
        if (self.start) {
            self.start = false;
        } else {
            self.offset += 1;
            if (self.offset >= self.content.len) return null;

            if (self.curr() == '\n') {
                self.col = 1;
                self.line += 1;
            } else {
                self.col += 1;
            }
        }
        return self.curr();
    }

    pub fn peek(self: Self) ?u8 {
        if (self.offset + 1 >= self.content.len) {
            return null;
        } else {
            return self.content[self.offset + 1];
        }
    }

    fn divOrComment(self: *Self) LexerError!?Token {
        if (self.peek()) |peek_ch| {
            if (peek_ch == '*') {
                _ = self.next(); // peeked character
                while (self.next()) |ch| {
                    if (ch == '*') {
                        if (self.peek()) |next_ch| {
                            if (next_ch == '/') {
                                _ = self.next(); // peeked character
                                return null;
                            }
                        }
                    }
                }
                return LexerError.EndOfFileInComment;
            }
        }
        return Token.build(self.*, .divide, null);
    }

    fn identifierOrKeyword(self: *Self, allocator: *std.mem.Allocator) !Token {
        var result = Token.build(self.*, .identifier, null);
        var init_offset = self.offset;
        var final_offset = self.offset + 1;
        while (self.peek()) |ch| : (_ = self.next()) {
            switch (ch) {
                '_', 'a'...'z', 'A'...'Z', '0'...'9' => final_offset += 1,
                else => break,
            }
        }

        if (std.mem.eql(u8, self.content[init_offset..final_offset], "if")) {
            result.typ = .kw_if;
        } else if (std.mem.eql(u8, self.content[init_offset..final_offset], "else")) {
            result.typ = .kw_else;
        } else if (std.mem.eql(u8, self.content[init_offset..final_offset], "while")) {
            result.typ = .kw_while;
        } else if (std.mem.eql(u8, self.content[init_offset..final_offset], "print")) {
            result.typ = .kw_print;
        } else if (std.mem.eql(u8, self.content[init_offset..final_offset], "putc")) {
            result.typ = .kw_putc;
        } else {
            result.value = TokenValue{ .ident = self.content[init_offset..final_offset] };
        }

        return result;
    }

    fn string(self: *Self, allocator: *std.mem.Allocator) !Token {
        var result = Token.build(self.*, .identifier, null);
        return result;
    }
};

pub fn lex(allocator: *std.mem.Allocator, content: []u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    var lexer = Lexer.init(content);
    while (lexer.next()) |ch| {
        switch (ch) {
            '/' => {
                if (try lexer.divOrComment()) |token| try tokens.append(token);
            },
            '_', 'a'...'z', 'A'...'Z' => {
                try tokens.append(try lexer.identifierOrKeyword(allocator));
            },
            '"' => {
                try tokens.append(try lexer.string(allocator));
            },
            else => {},
        }
    }
    lexer.print();
    return tokens;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    const example_path = "examples/input0.txt";
    var file = try std.fs.cwd().openFile(example_path, std.fs.File.OpenFlags{});
    defer std.fs.File.close(file);

    const content = try std.fs.File.readToEndAlloc(file, allocator, std.math.maxInt(usize));
    printContent(content);

    const tokens = try lex(allocator, content);
    const pretty_output = try tokenListToString(allocator, tokens);
    printContent(pretty_output);
}

fn printContent(content: []u8) void {
    p("==================== File start =====================\n\n", .{});
    p("{s}", .{content});
    p("\n==================== File end =======================\n", .{});
}

fn tokenListToString(allocator: *std.mem.Allocator, token_list: std.ArrayList(Token)) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    var w = result.writer();
    for (token_list.items) |token| {
        const common_args = .{ token.line, token.col, token.typ.toString() };
        if (token.value) |value| {
            const init_fmt = "{d:>5}{d:>5} {s:<15}";
            switch (value) {
                .string => |str| _ = try w.write(try fmt.allocPrint(
                    allocator,
                    init_fmt ++ "\"{s}\"\n",
                    common_args ++ .{str},
                )),
                .ident => |ident| _ = try w.write(try fmt.allocPrint(
                    allocator,
                    init_fmt ++ "{s}\n",
                    common_args ++ .{ident},
                )),
                .intlit => |i| _ = try w.write(try fmt.allocPrint(
                    allocator,
                    init_fmt ++ "{d}\n",
                    common_args ++ .{i},
                )),
                .intchar => |ch| _ = try w.write(try fmt.allocPrint(
                    allocator,
                    init_fmt ++ "{c}\n",
                    common_args ++ .{ch},
                )),
            }
        } else {
            _ = try w.write(try fmt.allocPrint(allocator, "{d:>5}{d:>5} {s}\n", common_args));
        }
    }
    if (result.items.len > 0) _ = result.pop(); // final newline
    return result.items;
}

test "tokenListToString" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;
    var tokens = std.ArrayList(Token).init(allocator);
    const str: []const u8 = "Hello, World!\\n";
    const tok_array = [6]Token{
        Token{ .line = 4, .col = 1, .typ = .kw_print },
        Token{ .line = 4, .col = 6, .typ = .left_paren },
        Token{ .line = 4, .col = 7, .typ = .string, .value = TokenValue{ .string = str } },
        Token{ .line = 4, .col = 24, .typ = .right_paren },
        Token{ .line = 4, .col = 25, .typ = .semicolon },
        Token{ .line = 5, .col = 1, .typ = .eof },
    };
    var token_list = std.ArrayList(Token).init(allocator);
    (try token_list.addManyAsArray(6)).* = tok_array;
    const expected =
        \\    4    1 Keyword_print
        \\    4    6 LeftParen
        \\    4    7 String         "Hello, World!\n"
        \\    4   24 RightParen
        \\    4   25 Semicolon
        \\    5    1 End_of_input
    ;
    const result = try tokenListToString(allocator, token_list);

    try testing.expectFmt(expected, "{s}", .{result});
}

test "lexer" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;
    const test_str =
        \\abc
        \\de
    ;
    var lexer = Lexer.init(test_str);
    try testing.expectEqual(@as(u8, 'a'), lexer.next().?);
    try testing.expectEqual(@as(u8, 'b'), lexer.next().?);
    try testing.expectEqual(@as(u8, 'b'), lexer.curr());
    try testing.expectEqual(@as(u8, 'c'), lexer.peek().?);
    try testing.expectEqual(@as(u8, 'c'), lexer.next().?);
    try testing.expectEqual(@as(u8, '\n'), lexer.peek().?);
    try testing.expectEqual(@as(u8, '\n'), lexer.next().?);
    try testing.expectEqual(@as(u8, 'd'), lexer.next().?);
    try testing.expectEqual(@as(u8, 'e'), lexer.next().?);
    try testing.expect(null == lexer.next());
}
