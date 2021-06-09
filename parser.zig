const std = @import("std");
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

    const from_string_map = std.ComptimeStringMap(TokenType, .{
        .{ "Op_multiply", .multiply },
        .{ "Op_divide", .divide },
        .{ "Op_mod", .mod },
        .{ "Op_add", .add },
        .{ "Op_subtract", .subtract },
        .{ "Op_negate", .negate },
        .{ "Op_less", .less },
        .{ "Op_lessequal", .less_equal },
        .{ "Op_greater", .greater },
        .{ "Op_greaterequal", .greater_equal },
        .{ "Op_equal", .equal },
        .{ "Op_notequal", .not_equal },
        .{ "Op_not", .not },
        .{ "Op_assign", .assign },
        .{ "Op_and", .bool_and },
        .{ "Op_or", .bool_or },
        .{ "LeftParen", .left_paren },
        .{ "RightParen", .right_paren },
        .{ "LeftBrace", .left_brace },
        .{ "RightBrace", .right_brace },
        .{ "Semicolon", .semicolon },
        .{ "Comma", .comma },
        .{ "Keyword_if", .kw_if },
        .{ "Keyword_else", .kw_else },
        .{ "Keyword_while", .kw_while },
        .{ "Keyword_print", .kw_print },
        .{ "Keyword_putc", .kw_putc },
        .{ "Identifier", .identifier },
        .{ "Integer", .integer },
        .{ "String", .string },
        .{ "End_of_input", .eof },
    });

    pub fn fromString(str: []const u8) TokenType {
        return from_string_map.get(str).?;
    }
};

const NodeMetadata = struct {
    token_type: TokenType,
    right_associative: bool,
    binary: bool,
    unary: bool,
    precedence: i8,
    node_type: NodeType,

    const self = [_]NodeMetadata{
        .{ .token_type = .multiply, .right_associative = false, .binary = true, .unary = false, .precedence = 13, .node_type = .multiply },
        .{ .token_type = .divide, .right_associative = false, .binary = true, .unary = false, .precedence = 13, .node_type = .divide },
        .{ .token_type = .mod, .right_associative = false, .binary = true, .unary = false, .precedence = 13, .node_type = .mod },
        .{ .token_type = .add, .right_associative = false, .binary = true, .unary = false, .precedence = 12, .node_type = .add },
        .{ .token_type = .subtract, .right_associative = false, .binary = true, .unary = false, .precedence = 12, .node_type = .subtract },
        .{ .token_type = .negate, .right_associative = false, .binary = false, .unary = true, .precedence = 14, .node_type = .negate },
        .{ .token_type = .less, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .less },
        .{ .token_type = .less_equal, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .less_equal },
        .{ .token_type = .greater, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .greater },
        .{ .token_type = .greater_equal, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .greater_equal },
        .{ .token_type = .equal, .right_associative = false, .binary = true, .unary = false, .precedence = 9, .node_type = .equal },
        .{ .token_type = .not_equal, .right_associative = false, .binary = true, .unary = false, .precedence = 9, .node_type = .not_equal },
        .{ .token_type = .not, .right_associative = false, .binary = false, .unary = true, .precedence = 14, .node_type = .not },
        .{ .token_type = .assign, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .assign },
        .{ .token_type = .bool_and, .right_associative = false, .binary = true, .unary = false, .precedence = 5, .node_type = .bool_and },
        .{ .token_type = .bool_or, .right_associative = false, .binary = true, .unary = false, .precedence = 4, .node_type = .bool_or },
        .{ .token_type = .left_paren, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .right_paren, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .left_brace, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .right_brace, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .semicolon, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .comma, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .kw_if, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .kw_if },
        .{ .token_type = .kw_else, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .kw_while, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .kw_while },
        .{ .token_type = .kw_print, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .kw_putc, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
        .{ .token_type = .identifier, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .identifier },
        .{ .token_type = .integer, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .integer },
        .{ .token_type = .string, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .string },
        .{ .token_type = .eof, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown },
    };

    pub fn find(token: TokenType) NodeMetadata {
        for (self) |metadata| {
            if (metadata.token_type == token.typ) return metadata;
        } else {
            unreachable;
        }
    }
};

pub const TokenValue = union(enum) {
    identifier: []const u8,
    integer: i64,
    string: []const u8,
};

pub const Token = struct {
    line: usize,
    col: usize,
    typ: TokenType = .unknown,
    value: ?TokenValue = null,

    fn init() Token {
        return Token{ .line = 0, .col = 0, .typ = .unknown };
    }
};

pub const NodeType = enum {
    unknown,
    identifier,
    string,
    integer,
    sequence,
    kw_if,
    prtc,
    prts,
    prti,
    kw_while,
    assign,
    negate,
    not,
    multiply,
    divide,
    mod,
    add,
    subtract,
    less,
    less_equal,
    greater,
    greater_equal,
    equal,
    not_equal,
    bool_and,
    bool_or,
};

pub const NodeValue = union(enum) {
    identifier: []const u8,
    integer: i64,
    string: []const u8,

    fn fromToken(token: Token) ?NodeValue {
        if (token.value) |value| {
            switch (value) {
                .identifier => |ident| return NodeValue{ .identifier = ident },
                .integer => |int| return NodeValue{ .integer = int },
                .string => |str| return NodeValue{ .string = str },
            }
        } else {
            return null;
        }
    }
};

pub const Tree = struct {
    left: ?*Tree,
    right: ?*Tree,
    typ: NodeType = .unknown,
    value: ?NodeValue = null,
};

pub const ParserError = error{
    ExpectedNotFound,
};

pub const Parser = struct {
    token_it: LexerOutputTokenizer,
    curr: Token,
    allocator: *std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator, str: []const u8) Self {
        return Self{
            .token_it = LexerOutputTokenizer.init(str),
            .curr = Token.init(),
            .allocator = allocator,
        };
    }

    pub fn makeNode(self: *Self, typ: NodeType, left: ?*Tree, right: ?*Tree) !*Tree {
        const result = try self.allocator.create(Tree);
        result.* = Tree{ .left = left, .right = right, .typ = typ };
        return result;
    }

    pub fn makeLeaf(self: *Self, typ: NodeType, value: ?NodeValue) !*Tree {
        const result = try self.allocator.create(Tree);
        result.* = Tree{ .left = null, .right = null, .typ = typ, .value = value };
        return result;
    }

    pub fn parse(self: *Self) !*Tree {
        try self.next();
        _ = NodeMetadata.self;
        _ = try self.parse_stmt();
        return try self.makeNode(.sequence, null, null);
    }

    fn parse_stmt(self: *Self) !?*Tree {
        var result: ?*Tree = null;
        switch (self.curr.typ) {
            .kw_print => {
                try self.expect(.left_paren);
                while (self.curr.typ != .comma) {
                    var e: ?*Tree = null;
                    if (self.curr.typ == .string) {
                        e = try self.makeNode(
                            .prts,
                            try self.makeLeaf(.string, NodeValue.fromToken(self.curr)),
                            null,
                        );
                        try self.next();
                    } else {
                        e = try self.makeNode(.prti, try self.parse_expr(0), null);
                    }
                    result = try self.makeNode(.sequence, result, e);
                }
                try self.expect(.right_paren);
                try self.expect(.semicolon);
            },
            else => unreachable,
        }
        return result;
    }

    fn parse_expr(self: *Self, precedence: i8) !?*Tree {
        return try self.makeLeaf(.integer, NodeValue{ .integer = 10 });
    }

    fn next(self: *Self) !void {
        const token = try self.token_it.next();
        if (token) |tok| {
            self.curr = tok;
        } else {
            self.curr = Token.init();
        }
    }

    fn expect(self: *Self, token_type: TokenType) !void {
        try self.next();
        if (self.curr.typ != token_type) return ParserError.ExpectedNotFound;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    var arg_it = std.process.args();
    _ = try arg_it.next(allocator) orelse unreachable; // program name
    const file_name = arg_it.next(allocator);
    var file_handle = blk: {
        if (file_name) |file_name_delimited| {
            const fname: []const u8 = try file_name_delimited;
            break :blk try std.fs.cwd().openFile(fname, .{});
        } else {
            break :blk std.io.getStdIn();
        }
    };
    defer file_handle.close();
    const input_content = try file_handle.readToEndAlloc(allocator, std.math.maxInt(usize));

    var parser = Parser.init(allocator, input_content);
    const result: *Tree = try parser.parse();

    printContent(input_content, "Result");
}

fn printContent(content: []u8, title: []const u8) void {
    p("==================== {s:<10} =====================\n\n", .{title});
    p("{s}", .{content});
    p("\n==================== File end =======================\n", .{});
}

pub const LexerOutputTokenizer = struct {
    it: std.mem.SplitIterator,

    const Self = @This();

    pub fn init(str: []const u8) Self {
        return Self{ .it = std.mem.split(str, "\n") };
    }

    pub fn next(self: *Self) !?Token {
        if (self.it.next()) |line| {
            if (line.len == 0) return null;
            var tokens_it = std.mem.tokenize(line, " ");
            const lineNumber = try std.fmt.parseInt(usize, tokens_it.next().?, 10);
            const colNumber = try std.fmt.parseInt(usize, tokens_it.next().?, 10);
            const typ_text = tokens_it.next().?;
            const typ = TokenType.fromString(typ_text);
            const pre_value_index = tokens_it.index;
            const value = tokens_it.next();
            var token = Token{ .line = lineNumber, .col = colNumber, .typ = typ };
            if (value) |val| {
                const token_value = blk: {
                    switch (typ) {
                        .string => {
                            tokens_it.index = pre_value_index;
                            break :blk TokenValue{ .string = tokens_it.rest() };
                        },
                        .integer => break :blk TokenValue{ .integer = try std.fmt.parseInt(i64, val, 10) },
                        .identifier => break :blk TokenValue{ .identifier = val },
                        else => unreachable,
                    }
                };
                token.value = token_value;
            }
            return token;
        } else {
            return null;
        }
    }
};

fn stringToTokenList(allocator: *std.mem.Allocator, str: []const u8) !std.ArrayList(Token) {
    var result = std.ArrayList(Token).init(allocator);
    var lexer_output_it = LexerOutputTokenizer.init(str);
    while (try lexer_output_it.next()) |token| {
        try result.append(token);
    }
    return result;
}

test "stringToTokenList" {
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
    const string =
        \\    4      1 Keyword_print
        \\    4      6 LeftParen
        \\    4      7 String         "Hello, World!\n"
        \\    4     24 RightParen
        \\    4     25 Semicolon
        \\    5      1 End_of_input
        \\
    ;
    const result = try stringToTokenList(allocator, string);

    try testing.expectEqual(token_list.items.len, result.items.len);
    try testing.expectEqual(token_list.items[0], result.items[0]);
    try testing.expectEqual(token_list.items[1], result.items[1]);

    try testing.expectEqual(token_list.items[2].line, result.items[2].line);
    try testing.expectEqual(token_list.items[2].col, result.items[2].col);
    try testing.expectEqual(token_list.items[2].typ, result.items[2].typ);
    try testing.expectEqualSlices(u8, token_list.items[2].value.?.string, result.items[2].value.?.string);

    try testing.expectEqual(token_list.items[3], result.items[3]);
    try testing.expectEqual(token_list.items[4], result.items[4]);
    try testing.expectEqual(token_list.items[5], result.items[5]);
}
