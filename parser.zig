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

    const eql = std.mem.eql;
    const tk = TokenType;

    pub fn fromString(str: []const u8) TokenType {
        if (eql(u8, str, "Op_multiply")) {
            return tk.multiply;
        } else if (eql(u8, str, "Op_divide")) {
            return tk.divide;
        } else if (eql(u8, str, "Op_mod")) {
            return tk.mod;
        } else if (eql(u8, str, "Op_add")) {
            return tk.add;
        } else if (eql(u8, str, "Op_subtract")) {
            return tk.subtract;
        } else if (eql(u8, str, "Op_negate")) {
            return tk.negate;
        } else if (eql(u8, str, "Op_less")) {
            return tk.less;
        } else if (eql(u8, str, "Op_lessequal")) {
            return tk.less_equal;
        } else if (eql(u8, str, "Op_greater")) {
            return tk.greater;
        } else if (eql(u8, str, "Op_greaterequal")) {
            return tk.greater_equal;
        } else if (eql(u8, str, "Op_equal")) {
            return tk.equal;
        } else if (eql(u8, str, "Op_notequal")) {
            return tk.not_equal;
        } else if (eql(u8, str, "Op_not")) {
            return tk.not;
        } else if (eql(u8, str, "Op_assign")) {
            return tk.assign;
        } else if (eql(u8, str, "Op_and")) {
            return tk.bool_and;
        } else if (eql(u8, str, "Op_or")) {
            return tk.bool_or;
        } else if (eql(u8, str, "LeftParen")) {
            return tk.left_paren;
        } else if (eql(u8, str, "RightParen")) {
            return tk.right_paren;
        } else if (eql(u8, str, "LeftBrace")) {
            return tk.left_brace;
        } else if (eql(u8, str, "RightBrace")) {
            return tk.right_brace;
        } else if (eql(u8, str, "Semicolon")) {
            return tk.semicolon;
        } else if (eql(u8, str, "Comma")) {
            return tk.comma;
        } else if (eql(u8, str, "Keyword_if")) {
            return tk.kw_if;
        } else if (eql(u8, str, "Keyword_else")) {
            return tk.kw_else;
        } else if (eql(u8, str, "Keyword_while")) {
            return tk.kw_while;
        } else if (eql(u8, str, "Keyword_print")) {
            return tk.kw_print;
        } else if (eql(u8, str, "Keyword_putc")) {
            return tk.kw_putc;
        } else if (eql(u8, str, "Identifier")) {
            return tk.identifier;
        } else if (eql(u8, str, "Integer")) {
            return tk.integer;
        } else if (eql(u8, str, "String")) {
            return tk.string;
        } else if (eql(u8, str, "End_of_input")) {
            return tk.eof;
        } else {
            return tk.unknown;
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
};

pub const Node = struct {
    left: ?Node,
    right: ?Node,
    typ: NodeType,
    value: ?NodeValue,
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

    const tokens = try stringToTokenList(allocator, input_content);

    printContent(input_content, "Result");
}

fn printContent(content: []u8, title: []const u8) void {
    p("==================== {s:<10} =====================\n\n", .{title});
    p("{s}", .{content});
    p("\n==================== File end =======================\n", .{});
}

fn stringToTokenList(allocator: *std.mem.Allocator, str: []const u8) !std.ArrayList(Token) {
    var result = std.ArrayList(Token).init(allocator);
    var lines = std.mem.split(str, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) break;
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
