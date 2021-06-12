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

pub const Op = enum {
    fetch,
    store,
    push,
    add,
    sub,
    mul,
    div,
    mod,
    lt,
    gt,
    le,
    ge,
    eq,
    ne,
    @"and",
    @"or",
    neg,
    not,
    jmp,
    jq,
    prtc,
    prts,
    prti,
    halt,

    const from_node = std.enums.directEnumArray(NodeType, ?Op, 0, .{
        .unknown = null,
        .identifier = null,
        .string = null,
        .integer = null,
        .sequence = null,
        .kw_if = null,
        .prtc = null,
        .prts = null,
        .prti = null,
        .kw_while = null,
        .assign = null,
        .negate = .neg,
        .not = .not,
        .multiply = .mul,
        .divide = .div,
        .mod = .mod,
        .add = .add,
        .subtract = .sub,
        .less = .lt,
        .less_equal = .le,
        .greater = .gt,
        .greater_equal = .ge,
        .equal = .eq,
        .not_equal = .ne,
        .bool_and = .@"and",
        .bool_or = .@"or",
    });
};

pub const CodeGenerator = struct {
    allocator: *std.mem.Allocator,
    data: std.ArrayList(u8),
    string_pool: std.ArrayList([]const u8),
    sp: usize,
    pc: usize,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator) Self {
        return CodeGenerator{
            .allocator = allocator,
            .data = std.ArrayList(u8).init(allocator),
            .string_pool = std.ArrayList([]const u8).init(allocator),
            .sp = 0,
            .pc = 0,
        };
    }

    pub fn gen(self: *Self, ast: ?*Tree) ![]u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        var writer = result.writer();
        try writer.print(
            "Datasize: {d} Strings: {d}\n",
            .{ self.data.items.len, self.string_pool.items.len },
        );
        for (self.string_pool.items) |string| {
            try writer.print("{s}\n", .{string});
        }
        try writer.writeAll("\n");
        return result.items;
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

    const ast = try loadAST(allocator, input_content);
    var code_generator = CodeGenerator.init(allocator);
    const result: []const u8 = try code_generator.gen(ast);
    p("=========\n\n", .{});
    _ = try std.io.getStdOut().write(result);
    p("\n=========\n", .{});
}

const LoadASTError = error{OutOfMemory} || std.fmt.ParseIntError;

fn loadAST(allocator: *std.mem.Allocator, str: []const u8) LoadASTError!?*Tree {
    var line_it = std.mem.split(str, "\n");
    return try loadASTHelper(allocator, &line_it);
}

fn loadASTHelper(allocator: *std.mem.Allocator, line_it: *std.mem.SplitIterator) LoadASTError!?*Tree {
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
                        break :blk NodeValue{ .string = tok_it.rest() };
                    },
                    else => unreachable,
                }
            };
            return try Tree.makeLeaf(allocator, node_type, node_value);
        }

        const left = try loadASTHelper(allocator, line_it);
        const right = try loadASTHelper(allocator, line_it);
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
