const std = @import("std");

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

    const to_string = std.enums.directEnumArray(Op, []const u8, 0, .{
        .fetch = "fetch",
        .store = "store",
        .push = "push",
        .add = "add",
        .sub = "sub",
        .mul = "mul",
        .div = "div",
        .mod = "mod",
        .lt = "lt",
        .gt = "gt",
        .le = "le",
        .ge = "ge",
        .eq = "eq",
        .ne = "ne",
        .@"and" = "and",
        .@"or" = "or",
        .neg = "neg",
        .not = "not",
        .jmp = "jmp",
        .jz = "jz",
        .prtc = "prtc",
        .prts = "prts",
        .prti = "prti",
        .halt = "halt",
    });

    pub fn toString(self: Op) []const u8 {
        return to_string[@enumToInt(self)];
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

fn loadBytecode(
    allocator: *std.mem.Allocator,
    str: []const u8,
    string_pool: *std.ArrayList([]const u8),
    globals: *std.ArrayList(i32),
) !std.ArrayList(u8) {
    var result = std.ArrayList(u8).init(allocator);
    return result;
}
