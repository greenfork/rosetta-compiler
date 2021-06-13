const std = @import("std");
const p = std.debug.print;

pub const VirtualMachineError = error{OutOfMemory};

pub const VirtualMachine = struct {
    allocator: *std.mem.Allocator,
    stack: [stack_size]i32,
    program: std.ArrayList(u8),
    sp: usize,
    pc: usize,
    string_pool: std.ArrayList([]const u8),
    globals: std.ArrayList(i32),
    output: std.ArrayList(u8),

    const Self = @This();
    const stack_size = 32;

    pub fn init(
        allocator: *std.mem.Allocator,
        program: std.ArrayList(u8),
        string_pool: std.ArrayList([]const u8),
        globals: std.ArrayList(i32),
    ) Self {
        return VirtualMachine{
            .allocator = allocator,
            .stack = [_]i32{std.math.maxInt(i32)} ** stack_size,
            .program = program,
            .sp = 0,
            .pc = 0,
            .string_pool = string_pool,
            .globals = globals,
            .output = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn interp(self: *Self) VirtualMachineError!void {
        try self.out("Hello\n", .{});
    }

    pub fn out(self: *Self, comptime format: []const u8, args: anytype) VirtualMachineError!void {
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

    var string_pool = std.ArrayList([]const u8).init(allocator);
    var globals = std.ArrayList(i32).init(allocator);
    const bytecode = try loadBytecode(allocator, input_content, &string_pool, &globals);
    var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
    try vm.interp();
    const result: []const u8 = vm.output.items;
    _ = try std.io.getStdOut().write(result);
}

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
    jz,
    prtc,
    prts,
    prti,
    halt,

    const from_string = std.ComptimeStringMap(Op, .{
        .{ "fetch", .fetch },
        .{ "store", .store },
        .{ "push", .push },
        .{ "add", .add },
        .{ "sub", .sub },
        .{ "mul", .mul },
        .{ "div", .div },
        .{ "mod", .mod },
        .{ "lt", .lt },
        .{ "gt", .gt },
        .{ "le", .le },
        .{ "ge", .ge },
        .{ "eq", .eq },
        .{ "ne", .ne },
        .{ "and", .@"and" },
        .{ "or", .@"or" },
        .{ "neg", .neg },
        .{ "not", .not },
        .{ "jmp", .jmp },
        .{ "jz", .jz },
        .{ "prtc", .prtc },
        .{ "prts", .prts },
        .{ "prti", .prti },
        .{ "halt", .halt },
    });

    pub fn fromString(str: []const u8) Op {
        return from_string.get(str).?;
    }
};

fn loadBytecode(
    allocator: *std.mem.Allocator,
    str: []const u8,
    string_pool: *std.ArrayList([]const u8),
    globals: *std.ArrayList(i32),
) !std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(allocator);
    var line_it = std.mem.split(str, "\n");
    while (line_it.next()) |line| {
        if (std.mem.indexOf(u8, line, "halt")) |_| {
            var tok_it = std.mem.tokenize(line, " ");
            const size = try std.fmt.parseInt(usize, tok_it.next().?, 10);
            try result.resize(size + 1);
            break;
        }
    }

    line_it.index = 0;
    const first_line = line_it.next().?;
    const strings_index = std.mem.indexOf(u8, first_line, " Strings: ").?;
    const globals_size = try std.fmt.parseInt(usize, first_line["Datasize: ".len..strings_index], 10);
    const string_pool_size = try std.fmt.parseInt(usize, first_line[strings_index + " Strings: ".len ..], 10);
    try globals.ensureTotalCapacity(globals_size);
    try string_pool.ensureTotalCapacity(string_pool_size);
    var string_cnt: usize = 0;
    while (string_cnt < string_pool_size) : (string_cnt += 1) {
        try string_pool.append(line_it.next().?);
    }
    // p("bytecode length: {d}, globals_size: {d}, string_pool_size: {d}\n", .{
    //     result.items.len,
    //     globals_size,
    //     string_pool_size,
    // });

    while (line_it.next()) |line| {
        if (line.len == 0) break;

        // p("{s}\n", .{line});
        var tok_it = std.mem.tokenize(line, " ");
        const address = try std.fmt.parseInt(usize, tok_it.next().?, 10);
        const op = Op.fromString(tok_it.next().?);
        // p("address: {d}, op: {}, opint: {d}\n", .{ address, op, @enumToInt(op) });
        result.items[address] = @enumToInt(op);
        switch (op) {
            .fetch, .store => {
                const index_bracketed = tok_it.rest();
                const index = try std.fmt.parseInt(i32, index_bracketed[1 .. index_bracketed.len - 1], 10);
                insertInt(&result, address + 1, index);
            },
            else => {},
        }
    }
    return result;
}

fn insertInt(array: *std.ArrayList(u8), address: usize, n: i32) void {
    const word_size = @sizeOf(i32);
    var i: usize = 0;
    var n_var = n;
    var n_bytes = @ptrCast(*[4]u8, &n_var);
    while (i < word_size) : (i += 1) {
        array.items[@intCast(usize, address + i)] = n_bytes[@intCast(usize, i)];
    }
}
