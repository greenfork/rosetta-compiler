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
    intchar: i64,
    string: []const u8,
};

pub const Token = struct {
    line: usize,
    col: usize,
    typ: TokenType = .unknown,
    value: ?TokenValue = null,
};

pub const LexerError = error{
    EmptyCharacterConstant,
    UnknownEscapeSequence,
    MulticharacterConstant,
    EndOfFileInComment,
    EndOfFileInString,
    EndOfLineInString,
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

    pub fn buildToken(self: Self) Token {
        return Token{ .line = self.line, .col = self.col };
    }

    pub fn buildTokenT(self: Self, typ: TokenType) Token {
        return Token{ .line = self.line, .col = self.col, .typ = typ };
    }

    pub fn curr(self: Self) u8 {
        return self.content[self.offset];
    }

    pub fn next(self: *Self) ?u8 {
        if (self.start) {
            self.start = false;
        } else {
            const newline = self.curr() == '\n';
            self.offset += 1;
            if (newline) {
                self.col = 1;
                self.line += 1;
            } else {
                self.col += 1;
            }
        }
        if (self.offset >= self.content.len) {
            return null;
        } else {
            return self.curr();
        }
    }

    pub fn peek(self: Self) ?u8 {
        if (self.offset + 1 >= self.content.len) {
            return null;
        } else {
            return self.content[self.offset + 1];
        }
    }

    fn divOrComment(self: *Self) LexerError!?Token {
        var result = self.buildToken();
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
        result.typ = .divide;
        return result;
    }

    fn identifierOrKeyword(self: *Self) !Token {
        var result = self.buildToken();
        const init_offset = self.offset;
        while (self.peek()) |ch| : (_ = self.next()) {
            switch (ch) {
                '_', 'a'...'z', 'A'...'Z', '0'...'9' => {},
                else => break,
            }
        }
        const final_offset = self.offset + 1;

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
            result.typ = .identifier;
            result.value = TokenValue{ .ident = self.content[init_offset..final_offset] };
        }

        return result;
    }

    fn string(self: *Self) !Token {
        var result = self.buildToken();
        result.typ = .string;
        const init_offset = self.offset;
        while (self.next()) |ch| {
            switch (ch) {
                '"' => break,
                '\n' => return LexerError.EndOfLineInString,
                '\\' => {
                    if (self.peek()) |escaped_ch| {
                        switch (escaped_ch) {
                            'n', '\\' => _ = self.next(), // peeked character
                            else => return LexerError.UnknownEscapeSequence,
                        }
                    } else {
                        return LexerError.EndOfFileInString;
                    }
                },
                else => {},
            }
        } else {
            return LexerError.EndOfFileInString;
        }
        const final_offset = self.offset + 1;
        result.value = TokenValue{ .string = self.content[init_offset..final_offset] };
        return result;
    }

    fn followed(self: *Self, by: u8, ifyes: TokenType, ifno: TokenType) Token {
        var result = self.buildToken();
        if (self.peek()) |ch| {
            if (ch == by) {
                _ = self.next(); // peeked character
                result.typ = ifyes;
            } else {
                result.typ = ifno;
            }
        } else {
            result.typ = ifno;
        }
        return result;
    }

    fn consecutive(self: *Self, by: u8, typ: TokenType) LexerError!Token {
        const result = self.buildTokenT(typ);
        if (self.peek()) |ch| {
            if (ch == by) {
                _ = self.next(); // peeked character
                return result;
            } else {
                return LexerError.UnrecognizedCharacter;
            }
        } else {
            return LexerError.UnrecognizedCharacter;
        }
    }

    fn integerLiteral(self: *Self) LexerError!Token {
        var result = self.buildTokenT(.integer);
        const init_offset = self.offset;
        while (self.peek()) |ch| {
            switch (ch) {
                '0'...'9' => _ = self.next(), // peeked character
                '_', 'a'...'z', 'A'...'Z' => return LexerError.InvalidNumber,
                else => break,
            }
        }
        const final_offset = self.offset + 1;
        result.value = TokenValue{
            .intlit = std.fmt.parseInt(i64, self.content[init_offset..final_offset], 10) catch {
                return LexerError.InvalidNumber;
            },
        };
        return result;
    }

    fn nextOrEmpty(self: *Self) LexerError!u8 {
        return self.next() orelse LexerError.EmptyCharacterConstant;
    }

    fn integerChar(self: *Self) LexerError!Token {
        var result = self.buildTokenT(.integer);
        const init_offset = self.offset;
        switch (try self.nextOrEmpty()) {
            '\'', '\n' => return LexerError.EmptyCharacterConstant,
            '\\' => {
                switch (try self.nextOrEmpty()) {
                    'n' => result.value = TokenValue{ .intchar = '\n' },
                    '\\' => result.value = TokenValue{ .intchar = '\\' },
                    else => return LexerError.EmptyCharacterConstant,
                }
                switch (try self.nextOrEmpty()) {
                    '\'' => {},
                    else => return LexerError.MulticharacterConstant,
                }
            },
            else => {
                result.value = TokenValue{ .intchar = self.curr() };
                switch (try self.nextOrEmpty()) {
                    '\'' => {},
                    else => return LexerError.MulticharacterConstant,
                }
            },
        }
        return result;
    }
};

pub fn lex(allocator: *std.mem.Allocator, content: []u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    var lexer = Lexer.init(content);
    while (lexer.next()) |ch| {
        switch (ch) {
            ' ' => {},
            '*' => try tokens.append(lexer.buildTokenT(.multiply)),
            '%' => try tokens.append(lexer.buildTokenT(.mod)),
            '+' => try tokens.append(lexer.buildTokenT(.add)),
            '-' => try tokens.append(lexer.buildTokenT(.subtract)),
            '<' => try tokens.append(lexer.followed('=', .less_equal, .less)),
            '>' => try tokens.append(lexer.followed('=', .greater_equal, .greater)),
            '=' => try tokens.append(lexer.followed('=', .equal, .assign)),
            '!' => try tokens.append(lexer.followed('=', .not_equal, .not)),
            '(' => try tokens.append(lexer.buildTokenT(.left_paren)),
            ')' => try tokens.append(lexer.buildTokenT(.right_paren)),
            '{' => try tokens.append(lexer.buildTokenT(.left_brace)),
            '}' => try tokens.append(lexer.buildTokenT(.right_brace)),
            ';' => try tokens.append(lexer.buildTokenT(.semicolon)),
            ',' => try tokens.append(lexer.buildTokenT(.comma)),
            '&' => try tokens.append(try lexer.consecutive('&', .bool_and)),
            '|' => try tokens.append(try lexer.consecutive('|', .bool_or)),
            '/' => {
                if (try lexer.divOrComment()) |token| try tokens.append(token);
            },
            '_', 'a'...'z', 'A'...'Z' => try tokens.append(try lexer.identifierOrKeyword()),
            '"' => try tokens.append(try lexer.string()),
            '0'...'9' => try tokens.append(try lexer.integerLiteral()),
            '\'' => try tokens.append(try lexer.integerChar()),
            else => {},
        }
    }
    try tokens.append(lexer.buildTokenT(.eof));

    return tokens;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    const example_input_path = "examples/input2.txt";
    var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
    defer std.fs.File.close(file_input);
    const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));
    printContent(content_input, "Input");

    const example_output_path = "examples/lexed2.txt";
    var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
    defer std.fs.File.close(file_output);
    const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));
    printContent(content_output, "Lexed");

    const tokens = try lex(allocator, content_input);
    const pretty_output = try tokenListToString(allocator, tokens);
    printContent(pretty_output, "Result");
}

fn printContent(content: []u8, title: []const u8) void {
    p("==================== {s:<10} =====================\n\n", .{title});
    p("{s}", .{content});
    p("\n==================== File end =======================\n", .{});
}

fn tokenListToString(allocator: *std.mem.Allocator, token_list: std.ArrayList(Token)) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    var w = result.writer();
    for (token_list.items) |token| {
        const common_args = .{ token.line, token.col, token.typ.toString() };
        if (token.value) |value| {
            const init_fmt = "{d:>5}{d:>7} {s:<15}";
            switch (value) {
                .string => |str| _ = try w.write(try fmt.allocPrint(
                    allocator,
                    init_fmt ++ "{s}\n",
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
                    init_fmt ++ "{d}\n",
                    common_args ++ .{ch},
                )),
            }
        } else {
            _ = try w.write(try fmt.allocPrint(allocator, "{d:>5}{d:>7} {s}\n", common_args));
        }
    }
    return result.items;
}

test "tokenListToString" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;
    var tokens = std.ArrayList(Token).init(allocator);
    const str: []const u8 = "\"Hello, World!\\n\"";
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
        \\    4      1 Keyword_print
        \\    4      6 LeftParen
        \\    4      7 String         "Hello, World!\n"
        \\    4     24 RightParen
        \\    4     25 Semicolon
        \\    5      1 End_of_input
        \\
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

fn squishSpaces(allocator: *std.mem.Allocator, str: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    var was_space = false;
    for (str) |ch| {
        switch (ch) {
            ' ' => {
                if (!was_space) {
                    was_space = true;
                    try result.append(ch);
                }
            },
            else => {
                was_space = false;
                try result.append(ch);
            },
        }
    }
    return result.items;
}

test "examples" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    {
        const example_input_path = "examples/input0.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed0.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input1.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed1.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input2.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed2.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input3.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed3.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input4.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed4.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input5.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed5.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input6.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed6.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input7.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed7.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input8.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed8.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input9.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed9.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input10.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed10.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input11.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed11.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input12.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed12.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input13.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed13.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/input14.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/lexed14.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const tokens = try lex(allocator, content_input);
        const pretty_output = try tokenListToString(allocator, tokens);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }
}
