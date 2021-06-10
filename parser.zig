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
    node_str: []const u8,

    const self = [_]NodeMetadata{
        .{ .token_type = .multiply, .right_associative = false, .binary = true, .unary = false, .precedence = 13, .node_type = .multiply, .node_str = "*" },
        .{ .token_type = .divide, .right_associative = false, .binary = true, .unary = false, .precedence = 13, .node_type = .divide, .node_str = "/" },
        .{ .token_type = .mod, .right_associative = false, .binary = true, .unary = false, .precedence = 13, .node_type = .mod, .node_str = "%" },
        .{ .token_type = .add, .right_associative = false, .binary = true, .unary = false, .precedence = 12, .node_type = .add, .node_str = "+" },
        .{ .token_type = .subtract, .right_associative = false, .binary = true, .unary = false, .precedence = 12, .node_type = .subtract, .node_str = "-" },
        .{ .token_type = .negate, .right_associative = false, .binary = false, .unary = true, .precedence = 14, .node_type = .negate, .node_str = "-" },
        .{ .token_type = .less, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .less, .node_str = "<" },
        .{ .token_type = .less_equal, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .less_equal, .node_str = "<=" },
        .{ .token_type = .greater, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .greater, .node_str = ">" },
        .{ .token_type = .greater_equal, .right_associative = false, .binary = true, .unary = false, .precedence = 10, .node_type = .greater_equal, .node_str = ">=" },
        .{ .token_type = .equal, .right_associative = false, .binary = true, .unary = false, .precedence = 9, .node_type = .equal, .node_str = "=" },
        .{ .token_type = .not_equal, .right_associative = false, .binary = true, .unary = false, .precedence = 9, .node_type = .not_equal, .node_str = "!=" },
        .{ .token_type = .not, .right_associative = false, .binary = false, .unary = true, .precedence = 14, .node_type = .not, .node_str = "!" },
        .{ .token_type = .assign, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .assign, .node_str = "=" },
        .{ .token_type = .bool_and, .right_associative = false, .binary = true, .unary = false, .precedence = 5, .node_type = .bool_and, .node_str = "&&" },
        .{ .token_type = .bool_or, .right_associative = false, .binary = true, .unary = false, .precedence = 4, .node_type = .bool_or, .node_str = "||" },
        .{ .token_type = .left_paren, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "(" },
        .{ .token_type = .right_paren, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = ")" },
        .{ .token_type = .left_brace, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "{" },
        .{ .token_type = .right_brace, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "}" },
        .{ .token_type = .semicolon, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = ";" },
        .{ .token_type = .comma, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "," },
        .{ .token_type = .kw_if, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .kw_if, .node_str = "if" },
        .{ .token_type = .kw_else, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "else" },
        .{ .token_type = .kw_while, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .kw_while, .node_str = "while" },
        .{ .token_type = .kw_print, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "print" },
        .{ .token_type = .kw_putc, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "putc" },
        .{ .token_type = .identifier, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .identifier, .node_str = "Identifier" },
        .{ .token_type = .integer, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .integer, .node_str = "Integer literal" },
        .{ .token_type = .string, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .string, .node_str = "String literal" },
        .{ .token_type = .eof, .right_associative = false, .binary = false, .unary = false, .precedence = -1, .node_type = .unknown, .node_str = "End of line" },
    };

    pub fn find(token_type: TokenType) NodeMetadata {
        for (self) |metadata| {
            if (metadata.token_type == token_type) return metadata;
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

    pub fn toString(self: NodeType) []const u8 {
        return switch (self) {
            .unknown => "UNKNOWN",
            .identifier => "Identifier",
            .string => "String",
            .integer => "Integer",
            .sequence => "Sequence",
            .kw_if => "If",
            .prtc => "Prtc",
            .prts => "Prts",
            .prti => "Prti",
            .kw_while => "While",
            .assign => "Assign",
            .negate => "Negate",
            .not => "Not",
            .multiply => "Multiply",
            .divide => "Divide",
            .mod => "Mod",
            .add => "And",
            .subtract => "Subtract",
            .less => "Less",
            .less_equal => "LessEqual",
            .greater => "Greater",
            .greater_equal => "GreaterEqual",
            .equal => "Equal",
            .not_equal => "NotEqual",
            .bool_and => "And",
            .bool_or => "Or",
        };
    }
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
    OutOfMemory,
    ExpectedNotFound,
} || std.fmt.ParseIntError;

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

    fn makeNode(self: *Self, typ: NodeType, left: ?*Tree, right: ?*Tree) !*Tree {
        const result = try self.allocator.create(Tree);
        result.* = Tree{ .left = left, .right = right, .typ = typ };
        return result;
    }

    fn makeLeaf(self: *Self, typ: NodeType, value: ?NodeValue) !*Tree {
        const result = try self.allocator.create(Tree);
        result.* = Tree{ .left = null, .right = null, .typ = typ, .value = value };
        return result;
    }

    pub fn parse(self: *Self) ParserError!?*Tree {
        try self.next();
        var result: ?*Tree = null;
        while (true) {
            const stmt = try self.parseStmt();
            result = try self.makeNode(.sequence, result, stmt);
            if (self.curr.typ == .eof) break;
        }
        return result;
    }

    fn print(self: Self) void {
        std.debug.print("\n{}\n\n", .{self.curr});
    }

    fn parseStmt(self: *Self) ParserError!?*Tree {
        var result: ?*Tree = null;
        switch (self.curr.typ) {
            .kw_print => {
                try self.next();
                try self.expect(.left_paren);
                while (true) {
                    var expr: ?*Tree = null;
                    if (self.curr.typ == .string) {
                        expr = try self.makeNode(
                            .prts,
                            try self.makeLeaf(.string, NodeValue.fromToken(self.curr)),
                            null,
                        );
                        try self.next();
                    } else {
                        expr = try self.makeNode(.prti, try self.parseExpr(0), null);
                    }
                    result = try self.makeNode(.sequence, result, expr);
                    if (self.curr.typ != .comma) break;
                    try self.next();
                }
                try self.expect(.right_paren);
                try self.expect(.semicolon);
            },
            .identifier => {
                const identifer = try self.makeLeaf(.identifier, NodeValue.fromToken(self.curr));
                try self.next();
                try self.expect(.assign);
                const expr = try self.parseExpr(0);
                result = try self.makeNode(.assign, identifer, expr);
                try self.expect(.semicolon);
            },
            else => {
                p("\nSTMT: UNKOWN {}\n", .{self.curr});
                self.curr.typ = .eof;
                return result;
            },
        }
        return result;
    }

    fn parseExpr(self: *Self, precedence: i8) ParserError!?*Tree {
        switch (self.curr.typ) {
            .left_paren => {
                try self.next();
                const result = try self.parseExpr(0);
                try self.expect(.right_paren);
                return result;
            },
            .integer, .identifier => |typ| {
                const node_type = NodeMetadata.find(typ).node_type;
                const terminal = try self.makeLeaf(node_type, NodeValue.fromToken(self.curr));
                try self.next();
                return terminal;
            },
            else => {
                p("\nEXPR: UNKNOWN {}\n", .{self.curr});
                self.curr.typ = .eof;
                return try self.makeLeaf(.unknown, NodeValue{ .integer = 10 });
            },
        }
    }

    fn next(self: *Self) ParserError!void {
        const token = try self.token_it.next();
        if (token) |tok| {
            self.curr = tok;
        } else {
            self.curr = Token.init();
        }
    }

    fn expect(self: *Self, token_type: TokenType) ParserError!void {
        if (self.curr.typ != token_type) {
            const expected_str = NodeMetadata.find(token_type).node_str;
            const found_str = NodeMetadata.find(self.curr.typ).node_str;
            std.debug.print(
                "({d}, {d}) error: Expecting '{s}', found '{s}'\n",
                .{ self.curr.line, self.curr.col, expected_str, found_str },
            );
            return ParserError.ExpectedNotFound;
        }
        try self.next();
    }
};

pub fn parse(allocator: *std.mem.Allocator, str: []const u8) !?*Tree {
    var parser = Parser.init(allocator, str);
    return try parser.parse();
}

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
    printContent(input_content, "Input");

    const result: ?*Tree = try parse(allocator, input_content);
    const result_str = try astToFlattenedString(allocator, result);
    printContent(result_str, "AST");
}

fn printContent(content: []const u8, title: []const u8) void {
    p("==================== {s:<10} =====================\n\n", .{title});
    p("{s}", .{content});
    p("\n==================== File end =======================\n", .{});
}

fn astToFlattenedString(allocator: *std.mem.Allocator, tree: ?*Tree) ![]const u8 {
    var result = std.ArrayList(u8).init(allocator);
    var writer = result.writer();
    try treeToString(allocator, writer, tree);
    return result.items;
}

const TreeToStringError = error{OutOfMemory};

fn treeToString(
    allocator: *std.mem.Allocator,
    writer: std.ArrayList(u8).Writer,
    tree: ?*Tree,
) TreeToStringError!void {
    if (tree) |t| {
        _ = try writer.write(try std.fmt.allocPrint(
            allocator,
            "{s}",
            .{t.typ.toString()},
        ));
        switch (t.typ) {
            .identifier => _ = try writer.write(try std.fmt.allocPrint(
                allocator,
                "   {s}\n",
                .{t.value.?.identifier},
            )),
            .string => _ = try writer.write(try std.fmt.allocPrint(
                allocator,
                "   {s}\n",
                .{t.value.?.string},
            )),
            .integer => _ = try writer.write(try std.fmt.allocPrint(
                allocator,
                "   {d}\n",
                .{t.value.?.integer},
            )),
            else => {
                _ = try writer.write(try std.fmt.allocPrint(
                    allocator,
                    "\n",
                    .{},
                ));
                try treeToString(allocator, writer, t.left);
                try treeToString(allocator, writer, t.right);
            },
        }
    } else {
        _ = try writer.write(try std.fmt.allocPrint(
            allocator,
            ";\n",
            .{},
        ));
    }
}

pub const LexerOutputTokenizer = struct {
    it: std.mem.SplitIterator,

    const Self = @This();

    pub fn init(str: []const u8) Self {
        return Self{ .it = std.mem.split(str, "\n") };
    }

    pub fn next(self: *Self) std.fmt.ParseIntError!?Token {
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
        const example_input_path = "examples/lexed0.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/parsed0.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const ast = try parse(allocator, content_input);
        const pretty_output = try astToFlattenedString(allocator, ast);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/lexed1.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/parsed1.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        const ast = try parse(allocator, content_input);
        const pretty_output = try astToFlattenedString(allocator, ast);

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }
}
