const std = @import("std");
const p = std.debug.print;

pub const Op = enum(u8) {
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

    pub fn fromNodeType(node_type: NodeType) ?Op {
        return from_node[node_type];
    }
};

pub const CodeGeneratorError = error{OutOfMemory};

pub const CodeGenerator = struct {
    allocator: *std.mem.Allocator,
    string_pool: std.ArrayList([]const u8),
    globals: std.ArrayList([]const u8),
    bytecode: std.ArrayList(u8),

    const Self = @This();
    const word_size = @sizeOf(i32);

    pub fn init(
        allocator: *std.mem.Allocator,
        string_pool: std.ArrayList([]const u8),
        globals: std.ArrayList([]const u8),
    ) Self {
        return CodeGenerator{
            .allocator = allocator,
            .string_pool = string_pool,
            .globals = globals,
            .bytecode = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn gen(self: *Self, ast: ?*Tree) CodeGeneratorError!void {
        try self.genH(ast);
        try self.emitHalt();
    }

    pub fn genH(self: *Self, ast: ?*Tree) CodeGeneratorError!void {
        if (ast) |t| {
            switch (t.typ) {
                .sequence => {
                    try self.genH(t.left);
                    try self.genH(t.right);
                },
                .assign => {
                    try self.genH(t.right);
                    try self.emitByte(.store);
                    try self.emitInt(self.fetchGlobalOffset(t.left.?.value.?.string));
                },
                .prts => {
                    try self.genH(t.left);
                    try self.emitByte(.prts);
                },
                .prti => {
                    try self.genH(t.left);
                    try self.emitByte(.prti);
                },
                .prtc => {
                    try self.genH(t.left);
                    try self.emitByte(.prtc);
                },
                .string => {
                    try self.emitByte(.push);
                    try self.emitInt(self.fetchStringOffset(t.value.?.string));
                },
                .integer => {
                    try self.emitByte(.push);
                    try self.emitInt(t.value.?.integer);
                },
                .identifier => {
                    try self.emitByte(.fetch);
                    try self.emitInt(self.fetchGlobalOffset(t.value.?.string));
                },
                else => {
                    std.debug.print("\nINTERP: UNKNOWN {}\n", .{t.typ});
                    std.os.exit(1);
                },
            }
        }
    }

    fn emitByte(self: *Self, op: Op) CodeGeneratorError!void {
        try self.bytecode.append(@enumToInt(op));
    }

    fn emitInt(self: *Self, n: i32) CodeGeneratorError!void {
        // p("emitInt: {d}\n", .{n});
        var n_var = n;
        var n_bytes = @ptrCast(*[4]u8, &n_var);
        for (n_bytes) |byte| {
            try self.bytecode.append(byte);
        }
    }

    fn emitHalt(self: *Self) CodeGeneratorError!void {
        try self.bytecode.append(@enumToInt(Op.halt));
    }

    fn fetchStringOffset(self: Self, str: []const u8) i32 {
        for (self.string_pool.items) |string, idx| {
            if (std.mem.eql(u8, string, str)) {
                return @intCast(i32, idx);
            }
        }
        unreachable;
    }

    fn fetchGlobalOffset(self: Self, str: []const u8) i32 {
        for (self.globals.items) |global, idx| {
            if (std.mem.eql(u8, global, str)) {
                return @intCast(i32, idx);
            }
        }
        unreachable;
    }

    pub fn print(self: Self) ![]u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        var writer = result.writer();
        try writer.print(
            "Datasize: {d} Strings: {d}\n",
            .{ self.globals.items.len, self.string_pool.items.len },
        );
        for (self.string_pool.items) |string| {
            try writer.print("{s}\n", .{string});
        }

        var pc: usize = 0;
        while (pc < self.bytecode.items.len) : (pc += 1) {
            try writer.print("{d:>5} ", .{pc});
            switch (@intToEnum(Op, self.bytecode.items[pc])) {
                .push => {
                    try writer.print("push  {d}\n", .{self.unpackInt(pc + 1)});
                    pc += word_size;
                },
                .store => {
                    try writer.print("store [{d}]\n", .{self.unpackInt(pc + 1)});
                    pc += word_size;
                },
                .fetch => {
                    try writer.print("fetch [{d}]\n", .{self.unpackInt(pc + 1)});
                    pc += word_size;
                },
                .prts => try writer.writeAll("prts\n"),
                .prti => try writer.writeAll("prti\n"),
                .prtc => try writer.writeAll("prtc\n"),
                .halt => try writer.writeAll("halt\n"),
                else => |en| {
                    std.debug.print("\nPRINT: UNKNOWN {} at pc {d}\n", .{ en, pc });
                    std.os.exit(1);
                },
            }
        }

        return result.items;
    }

    fn unpackInt(self: Self, pc: usize) i32 {
        const arg_ptr = @ptrCast(*[4]u8, self.bytecode.items[pc .. pc + word_size]);
        var arg_array = arg_ptr.*;
        const arg = @ptrCast(*i32, @alignCast(@alignOf(i32), &arg_array));
        return arg.*;
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
    var globals = std.ArrayList([]const u8).init(allocator);
    const ast = try loadAST(allocator, input_content, &string_pool, &globals);
    var code_generator = CodeGenerator.init(allocator, string_pool, globals);
    try code_generator.gen(ast);
    const result: []const u8 = try code_generator.print();
    p("=========\n\n", .{});
    _ = try std.io.getStdOut().write(result);
    p("\n=========\n", .{});
}

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
    integer: i32,
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

const LoadASTError = error{OutOfMemory} || std.fmt.ParseIntError;

fn loadAST(
    allocator: *std.mem.Allocator,
    str: []const u8,
    string_pool: *std.ArrayList([]const u8),
    globals: *std.ArrayList([]const u8),
) LoadASTError!?*Tree {
    var line_it = std.mem.split(str, "\n");
    return try loadASTHelper(allocator, &line_it, string_pool, globals);
}

fn loadASTHelper(
    allocator: *std.mem.Allocator,
    line_it: *std.mem.SplitIterator,
    string_pool: *std.ArrayList([]const u8),
    globals: *std.ArrayList([]const u8),
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
                    .integer => break :blk NodeValue{ .integer = try std.fmt.parseInt(i32, leaf_value, 10) },
                    .identifier => {
                        var already_exists = false;
                        for (globals.items) |global| {
                            if (std.mem.eql(u8, global, leaf_value)) {
                                already_exists = true;
                                break;
                            }
                        }
                        if (!already_exists) try globals.append(leaf_value);
                        break :blk NodeValue{ .string = leaf_value };
                    },
                    .string => {
                        tok_it.index = pre_iteration_index;
                        const str = tok_it.rest();
                        var already_exists = false;
                        for (string_pool.items) |string| {
                            if (std.mem.eql(u8, string, str)) {
                                already_exists = true;
                                break;
                            }
                        }
                        if (!already_exists) try string_pool.append(str);
                        break :blk NodeValue{ .string = str };
                    },
                    else => unreachable,
                }
            };
            return try Tree.makeLeaf(allocator, node_type, node_value);
        }

        const left = try loadASTHelper(allocator, line_it, string_pool, globals);
        const right = try loadASTHelper(allocator, line_it, string_pool, globals);
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

const testing = std.testing;

test "examples" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    {
        const example_input_path = "examples/parsed0.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/codegened0.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList([]const u8).init(allocator);
        const ast = try loadAST(allocator, content_input, &string_pool, &globals);
        var code_generator = CodeGenerator.init(allocator, string_pool, globals);
        try code_generator.gen(ast);
        const pretty_output: []const u8 = try code_generator.print();

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }

    {
        const example_input_path = "examples/parsed1.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/codegened1.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList([]const u8).init(allocator);
        const ast = try loadAST(allocator, content_input, &string_pool, &globals);
        var code_generator = CodeGenerator.init(allocator, string_pool, globals);
        try code_generator.gen(ast);
        const pretty_output: []const u8 = try code_generator.print();

        const stripped_expected = try squishSpaces(allocator, content_output);
        const stripped_result = try squishSpaces(allocator, pretty_output);
        try testing.expectFmt(stripped_expected, "{s}", .{stripped_result});
    }
}
