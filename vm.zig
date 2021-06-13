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
    const word_size = @sizeOf(i32);

    pub fn init(
        allocator: *std.mem.Allocator,
        program: std.ArrayList(u8),
        string_pool: std.ArrayList([]const u8),
        globals: std.ArrayList(i32),
    ) Self {
        return VirtualMachine{
            .allocator = allocator,
            .stack = [_]i32{0xabab} ** stack_size,
            .program = program,
            .sp = 0,
            .pc = 0,
            .string_pool = string_pool,
            .globals = globals,
            .output = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn interp(self: *Self) VirtualMachineError!void {
        while (true) : (self.pc += 1) {
            // p(
            //     "pc: {d}, sp: {d}, val: {d}, op: {}\n",
            //     .{ self.pc, self.sp, self.stack[self.sp], @intToEnum(Op, self.program.items[self.pc]) },
            // );
            switch (@intToEnum(Op, self.program.items[self.pc])) {
                .push => self.push(self.unpackInt()),
                .store => self.globals.items[@intCast(usize, self.unpackInt())] = self.pop(),
                .fetch => self.push(self.globals.items[@intCast(usize, self.unpackInt())]),
                .jmp => self.pc = @intCast(usize, self.unpackInt() - 1),
                .jz => {
                    if (self.pop() == 0) {
                        // -1 because `while` increases it with every iteration.
                        // This doesn't allow to jump to location 0 because we use `usize` for `pc`,
                        // just arbitrary implementation limitation.
                        self.pc = @intCast(usize, self.unpackInt() - 1);
                    } else {
                        self.pc += word_size;
                    }
                },
                .prts => try self.out("{s}", .{self.string_pool.items[@intCast(usize, self.pop())]}),
                .prti => try self.out("{d}", .{self.pop()}),
                .prtc => try self.out("{c}", .{@intCast(u8, self.pop())}),
                .lt => self.binOp(lt),
                .le => self.binOp(le),
                .gt => self.binOp(gt),
                .ge => self.binOp(ge),
                .eq => self.binOp(eq),
                .ne => self.binOp(ne),
                .add => self.binOp(add),
                .mul => self.binOp(mul),
                .sub => self.binOp(sub),
                .div => self.binOp(div),
                .mod => self.binOp(mod),
                .@"and" => self.binOp(@"and"),
                .@"or" => self.binOp(@"or"),
                .not => self.push(@boolToInt(self.pop() == 0)),
                .neg => self.push(-self.pop()),
                .halt => break,
            }
        }
    }

    fn push(self: *Self, n: i32) void {
        // p("pushed: {d}\n", .{n});
        self.sp += 1;
        self.stack[self.sp] = n;
    }

    fn pop(self: *Self) i32 {
        std.debug.assert(self.sp != 0);
        // p("popped: {d}\n", .{self.stack[self.sp]});
        self.sp -= 1;
        return self.stack[self.sp + 1];
    }

    fn unpackInt(self: *Self) i32 {
        const arg_ptr = @ptrCast(*[4]u8, self.program.items[self.pc + 1 .. self.pc + 1 + word_size]);
        self.pc += word_size;
        var arg_array = arg_ptr.*;
        for (arg_array) |byte, idx| {
            // p("arg_array byte {d}: {x}\n", .{ idx, byte });
        }
        const arg = @ptrCast(*i32, @alignCast(@alignOf(i32), &arg_array));
        // p("unpacked: {d}\n", .{arg.*});
        return arg.*;
    }

    pub fn out(self: *Self, comptime format: []const u8, args: anytype) VirtualMachineError!void {
        try self.output.writer().print(format, args);
    }

    fn binOp(self: *Self, func: fn (a: i32, b: i32) i32) void {
        const a = self.pop();
        const b = self.pop();
        const result = func(b, a);
        // p("binOp :: {d} {d} :: res {d}\n", .{ a, b, result });
        self.push(result);
    }

    fn lt(a: i32, b: i32) i32 {
        return @boolToInt(a < b);
    }
    fn le(a: i32, b: i32) i32 {
        return @boolToInt(a <= b);
    }
    fn gt(a: i32, b: i32) i32 {
        return @boolToInt(a > b);
    }
    fn ge(a: i32, b: i32) i32 {
        return @boolToInt(a >= b);
    }
    fn eq(a: i32, b: i32) i32 {
        return @boolToInt(a == b);
    }
    fn ne(a: i32, b: i32) i32 {
        return @boolToInt(a != b);
    }
    fn add(a: i32, b: i32) i32 {
        return a + b;
    }
    fn sub(a: i32, b: i32) i32 {
        return a - b;
    }
    fn mul(a: i32, b: i32) i32 {
        return a * b;
    }
    fn div(a: i32, b: i32) i32 {
        return @divTrunc(a, b);
    }
    fn mod(a: i32, b: i32) i32 {
        return @mod(a, b);
    }
    fn @"or"(a: i32, b: i32) i32 {
        return @boolToInt((a != 0) or (b != 0));
    }
    fn @"and"(a: i32, b: i32) i32 {
        return @boolToInt((a != 0) and (b != 0));
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
    p("\n==================\n\n{s}\n==================\n\n", .{input_content});

    var string_pool = std.ArrayList([]const u8).init(allocator);
    var globals = std.ArrayList(i32).init(allocator);
    const bytecode = try loadBytecode(allocator, input_content, &string_pool, &globals);
    var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
    try vm.interp();
    const result: []const u8 = vm.output.items;
    p("\n==================\n\n{s}\n==================\n", .{result});
    // _ = try std.io.getStdOut().write(result);
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
    try globals.resize(globals_size);
    try string_pool.ensureCapacity(string_pool_size);
    var string_cnt: usize = 0;
    while (string_cnt < string_pool_size) : (string_cnt += 1) {
        const line = line_it.next().?;
        var program_string = try std.ArrayList(u8).initCapacity(allocator, line.len);
        var escaped = false;
        // Skip double quotes
        for (line[1 .. line.len - 1]) |ch| {
            if (escaped) {
                escaped = false;
                switch (ch) {
                    '\\' => try program_string.append('\\'),
                    'n' => try program_string.append('\n'),
                    else => {
                        p("unknown escape sequence: {c}\n", .{ch});
                        std.os.exit(1);
                    },
                }
            } else {
                switch (ch) {
                    '\\' => escaped = true,
                    else => try program_string.append(ch),
                }
            }
        }
        try string_pool.append(program_string.items);
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
            .push => {
                insertInt(&result, address + 1, try std.fmt.parseInt(i32, tok_it.rest(), 10));
            },
            .jmp, .jz => {
                _ = tok_it.next();
                insertInt(&result, address + 1, try std.fmt.parseInt(i32, tok_it.rest(), 10));
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

const testing = std.testing;

test "examples" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    {
        const example_input_path = "examples/codegened0.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output0.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened1.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output1.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened3.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output3.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened4.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output4.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened5.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output5.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened6.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output6.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened7.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output7.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened8.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output8.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened9.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output9.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened10.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output10.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened11.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output11.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened12.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output12.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened13.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output13.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }

    {
        const example_input_path = "examples/codegened14.txt";
        var file_input = try std.fs.cwd().openFile(example_input_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_input);
        const content_input = try std.fs.File.readToEndAlloc(file_input, allocator, std.math.maxInt(usize));

        const example_output_path = "examples/output14.txt";
        var file_output = try std.fs.cwd().openFile(example_output_path, std.fs.File.OpenFlags{});
        defer std.fs.File.close(file_output);
        const content_output = try std.fs.File.readToEndAlloc(file_output, allocator, std.math.maxInt(usize));

        var string_pool = std.ArrayList([]const u8).init(allocator);
        var globals = std.ArrayList(i32).init(allocator);
        const bytecode = try loadBytecode(allocator, content_input, &string_pool, &globals);
        var vm = VirtualMachine.init(allocator, bytecode, string_pool, globals);
        try vm.interp();
        const pretty_output: []const u8 = vm.output.items;

        try testing.expectFmt(content_output, "{s}", .{pretty_output});
    }
}
