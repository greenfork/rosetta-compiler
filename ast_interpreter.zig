const std = @import("std");
const testing = std.testing;
const p = std.debug.print;

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

    const from_string_map = std.ComptimeStringMap(NodeType, .{
        .{ "UNKNOWN", .unknown },
        .{ "Identifier", .identifier },
        .{ "String", .string },
        .{ "Integer", .integer },
        .{ "Sequence", .sequence },
        .{ "If", .kw_if },
        .{ "Prtc", .prtc },
        .{ "Prts", .prts },
        .{ "Prti", .prti },
        .{ "While", .kw_while },
        .{ "Assign", .assign },
        .{ "Negate", .negate },
        .{ "Not", .not },
        .{ "Multiply", .multiply },
        .{ "Divide", .divide },
        .{ "Mod", .mod },
        .{ "Add", .add },
        .{ "Subtract", .subtract },
        .{ "Less", .less },
        .{ "LessEqual", .less_equal },
        .{ "Greater", .greater },
        .{ "GreaterEqual", .greater_equal },
        .{ "Equal", .equal },
        .{ "NotEqual", .not_equal },
        .{ "And", .bool_and },
        .{ "Or", .bool_or },
    });

    pub fn fromString(str: []const u8) NodeType {
        return from_string_map.get(str).?;
    }
};

pub const NodeValue = union(enum) {
    integer: i64,
    string: []const u8,
};

pub const Tree = struct {
    left: ?*Tree,
    right: ?*Tree,
    typ: NodeType = .unknown,
    value: ?NodeValue = null,

    fn makeNode(allocator: *std.mem.Allocator, typ: NodeType, left: ?*Tree, right: ?*Tree) !*Tree {
        const result = try allocator.create(Tree);
        result.* = Tree{ .left = left, .right = right, .typ = typ };
        return result;
    }

    fn makeLeaf(allocator: *std.mem.Allocator, typ: NodeType, value: ?NodeValue) !*Tree {
        const result = try allocator.create(Tree);
        result.* = Tree{ .left = null, .right = null, .typ = typ, .value = value };
        return result;
    }
};

pub const ASTInterpreterError = error{OutOfMemory};

pub const ASTInterpreter = struct {
    output: std.ArrayList(u8),
    globals: std.StringHashMap(NodeValue),

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator) Self {
        return ASTInterpreter{
            .output = std.ArrayList(u8).init(allocator),
            .globals = std.StringHashMap(NodeValue).init(allocator),
        };
    }

    pub fn interp(self: *Self, tree: ?*Tree) ASTInterpreterError!?NodeValue {
        if (tree) |t| {
            switch (t.typ) {
                .sequence => {
                    _ = try self.interp(t.left);
                    _ = try self.interp(t.right);
                },
                .assign => try self.globals.put(
                    t.left.?.value.?.string,
                    (try self.interp(t.right)).?,
                ),
                .identifier => return self.globals.get(t.value.?.string).?,
                .prts => _ = try self.out("{s}", .{(try self.interp(t.left)).?.string}),
                .prti => _ = try self.out("{d}", .{(try self.interp(t.left)).?.integer}),
                .string => return t.value,
                .integer => return t.value,
                else => {
                    p("\nINTERP: UNKNOWN {}\n", .{t.typ});
                    std.os.exit(1);
                },
            }
        }

        return null;
    }

    pub fn out(self: *Self, comptime format: []const u8, args: anytype) ASTInterpreterError!void {
        try self.output.writer().print(format, args);
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
    p("=========\n\n{s}\n=========\n", .{input_content});
    p("\n\n", .{});

    var string_pool = std.ArrayList([]const u8).init(allocator);
    const ast = try loadAST(allocator, input_content, &string_pool);
    var ast_interpreter = ASTInterpreter.init(allocator);
    _ = try ast_interpreter.interp(ast);
    const result: []const u8 = ast_interpreter.output.items;
    p("=========\n\n", .{});
    _ = try std.io.getStdOut().write(result);
    p("\n=========\n", .{});
}

const LoadASTError = error{OutOfMemory} || std.fmt.ParseIntError;

fn loadAST(
    allocator: *std.mem.Allocator,
    str: []const u8,
    string_pool: *std.ArrayList([]const u8),
) LoadASTError!?*Tree {
    var line_it = std.mem.split(str, "\n");
    return try loadASTHelper(allocator, &line_it, string_pool);
}

fn loadASTHelper(
    allocator: *std.mem.Allocator,
    line_it: *std.mem.SplitIterator,
    string_pool: *std.ArrayList([]const u8),
) LoadASTError!?*Tree {
    if (line_it.next()) |line| {
        var tok_it = std.mem.tokenize(line, " ");
        const tok_str = tok_it.next().?;
        if (tok_str[0] == ';') return null;

        const node_type = NodeType.fromString(tok_str);
        const pre_iteration_index = tok_it.index;

        if (tok_it.next()) |leaf_value| {
            const node_value = blk: {
                switch (node_type) {
                    .integer => break :blk NodeValue{ .integer = try std.fmt.parseInt(i64, leaf_value, 10) },
                    .identifier => break :blk NodeValue{ .string = leaf_value },
                    .string => {
                        tok_it.index = pre_iteration_index;
                        const str = tok_it.rest();
                        var string_literal = try std.ArrayList(u8).initCapacity(allocator, str.len);
                        var escaped = false;
                        for (str[1 .. str.len - 1]) |ch| {
                            if (escaped) {
                                escaped = false;
                                switch (ch) {
                                    'n' => try string_literal.append('\n'),
                                    '\\' => try string_literal.append('\\'),
                                    else => unreachable,
                                }
                            } else {
                                switch (ch) {
                                    '\\' => escaped = true,
                                    else => try string_literal.append(ch),
                                }
                            }
                        }
                        try string_pool.append(string_literal.items);
                        break :blk NodeValue{ .string = string_literal.items };
                    },
                    else => unreachable,
                }
            };
            return try Tree.makeLeaf(allocator, node_type, node_value);
        }

        const left = try loadASTHelper(allocator, line_it, string_pool);
        const right = try loadASTHelper(allocator, line_it, string_pool);
        return try Tree.makeNode(allocator, node_type, left, right);
    } else {
        return null;
    }
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
        const example_input_path = "examples/parsed0.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output0.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        const ast = try loadAST(allocator, content_input, &string_pool);
        var ast_interpreter = ASTInterpreter.init(allocator);
        _ = try ast_interpreter.interp(ast);
        const pretty_output: []const u8 = ast_interpreter.output.items;

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/parsed1.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output1.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        const ast = try loadAST(allocator, content_input, &string_pool);
        var ast_interpreter = ASTInterpreter.init(allocator);
        _ = try ast_interpreter.interp(ast);
        const pretty_output: []const u8 = ast_interpreter.output.items;

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }
}
