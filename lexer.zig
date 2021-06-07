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

    pub fn curr(self: Self) u8 {
        return self.content[self.offset];
    }

    pub fn next(self: *Self) LexerError!?u8 {
        if (self.start) {
            self.start = false;
        } else {
            self.offset += 1;
            if (self.offset >= self.content.len) return null;
            if (self.curr() == '\n') {
                self.col = 1;
                while (self.curr() == '\n') {
                    self.line += 1;
                    self.offset += 1;
                    if (self.offset >= self.content.len) return null;
                }
            } else {
                self.col += 1;
            }
        }
        return self.curr();
    }
};

pub fn lex(allocator: *std.mem.Allocator, content: []u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    var lexer = Lexer.init(content);
    while (try lexer.next()) |ch| {
        switch (ch) {
            '/' => try tokens.append(Token.build(
                lexer,
                TokenType.string,
                TokenValue{ .string = content[0..2] },
            )),
            else => {},
        }
    }
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
        _ = try w.write(try fmt.allocPrint(allocator, "{d:>5}", .{token.line}));
        _ = try w.write(try fmt.allocPrint(allocator, "{d:>7}", .{token.col}));
        _ = try w.write(try fmt.allocPrint(allocator, " {s:<15}", .{token.typ.toString()}));
        if (token.value) |value| {
            switch (value) {
                .string => |str| _ = try w.write(try fmt.allocPrint(allocator, "\"{s}\"", .{str})),
                .ident => |ident| _ = try w.write(try fmt.allocPrint(allocator, "{s}", .{ident})),
                .intlit => |i| _ = try w.write(try fmt.allocPrint(allocator, "{d}", .{i})),
                .intchar => |ch| _ = try w.write(try fmt.allocPrint(allocator, "{c}", .{ch})),
            }
        }
        _ = try w.write("\n");
    }
    _ = result.pop(); // final newline
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
        \\    4      1 Keyword_print  
        \\    4      6 LeftParen      
        \\    4      7 String         "Hello, World!\n"
        \\    4     24 RightParen     
        \\    4     25 Semicolon      
        \\    5      1 End_of_input   
    ;
    const result = try tokenListToString(allocator, token_list);

    try testing.expectFmt(expected, "{s}", .{result});
}
