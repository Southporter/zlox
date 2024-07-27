const std = @import("std");
const Object = @import("Object.zig");
const Compiler = @import("Compiler.zig");
const Table = @import("Table.zig");
const StringPool = @import("StringPool.zig");
const VM = @import("VM.zig");
const values = @import("values.zig");
const Value = values.Value;

const GC_GROW_FACTOR = 2;

pub const Manager = struct {
    pub const Error = std.mem.Allocator.Error;
    const log = std.log.scoped(.memory_manager);

    bytes_allocated: usize = 0,
    next_gc: usize = 1024 * 1024,
    times_collected: usize = 0,
    arena: std.heap.ArenaAllocator,
    is_collecting: bool = false,
    strings: StringPool,
    objects: ?*Object = null,
    gray_stack: std.ArrayList(*Object),

    pub fn init(manager: *Manager, gpa: std.mem.Allocator) void {
        manager.arena = std.heap.ArenaAllocator.init(gpa);
        manager.gray_stack = std.ArrayList(*Object).init(gpa);
        manager.strings = StringPool.init(manager.allocator());
        manager.bytes_allocated = 0;
        manager.times_collected = 0;
        manager.next_gc = 1024 * 1024;
        manager.objects = null;
        manager.is_collecting = false;
    }

    pub fn deinit(manager: *Manager) void {
        var object = manager.objects;
        var object_count: usize = 0;
        while (object) |obj| {
            object_count += 1;
            object = obj.next;
        }
        std.debug.print("\nRan GC {d} times!!! There were {d} objects. Final GC threshold: {d}\n", .{ manager.times_collected, object_count, manager.next_gc });
        manager.gray_stack.deinit();
        manager.arena.deinit();
    }

    pub fn bookkeeping(manager: *Manager, size_t: usize, amount: usize) Error!void {
        log.debug("allocating {d} bytes\n", .{size_t * amount});
        manager.bytes_allocated += size_t * amount;
        if (!manager.is_collecting and manager.bytes_allocated > manager.next_gc) {
            try manager.collect();
        }
    }

    pub fn alloc(ctx: *anyopaque, n: usize, log2_ptr_align: u8, ra: usize) ?[*]u8 {
        var manager: *Manager = @ptrCast(@alignCast(ctx));
        const ptr_align = @as(usize, 1) << @as(std.mem.Allocator.Log2Align, @intCast(log2_ptr_align));

        manager.bookkeeping(ptr_align, n) catch |e| {
            log.err("Error running bookkeeping in alloc: {any}\n", .{e});
        };
        return manager.arena.allocator().vtable.alloc(&manager.arena, n, log2_ptr_align, ra);
    }

    pub fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
        var manager: *Manager = @ptrCast(@alignCast(ctx));

        log.debug("{*} reallocating {d} bytes\n", .{ buf, new_len });

        manager.bytes_allocated += new_len - buf.len;
        if (manager.bytes_allocated > manager.next_gc) {
            manager.collect() catch |e| {
                log.err("Error while running collect in resize: {any}\n", .{e});
            };
        }

        return manager.arena.allocator().vtable.resize(&manager.arena, buf, log2_buf_align, new_len, ret_addr);
    }

    pub fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
        var manager: *Manager = @ptrCast(@alignCast(ctx));
        log.debug("{*} freeing {d} bytes\n", .{ buf, buf.len });
        manager.bytes_allocated -= buf.len;
        manager.arena.allocator().vtable.free(&manager.arena, buf, log2_buf_align, ret_addr);
    }

    pub fn allocator(manager: *Manager) std.mem.Allocator {
        return .{
            .ptr = manager,
            .vtable = &.{
                .alloc = alloc,
                .free = free,
                .resize = resize,
            },
        };
    }

    pub fn copy(manager: *Manager, str: []const u8) !*Object {
        const interned = manager.strings.find(str);
        log.debug("Copying identifier: {?} -> {}?\n", .{ interned, interned == null });

        if (interned) |i| {
            return &i.object;
        }
        try manager.bookkeeping(@sizeOf(Object.String), 1);
        const res = try Object.String.copy(str, manager.allocator());
        try manager.strings.set(res.as(Object.String), values.NIL_VAL);
        res.next = manager.objects;
        manager.objects = res;
        return res;
    }

    pub fn concat(manager: *Manager, a: []const u8, b: []const u8) !*Object {
        var new_raw = try manager.allocator().alloc(u8, a.len + b.len);
        @memcpy(new_raw[0..a.len], a);
        @memcpy(new_raw[a.len..], b);

        if (manager.strings.find(new_raw)) |interned| {
            manager.allocator().free(new_raw);
            return &interned.object;
        } else {
            try manager.bookkeeping(@sizeOf(Object.String), 1);
            const str = try Object.String.fromAlloc(new_raw, manager.allocator());
            try manager.strings.set(str, values.NIL_VAL);
            const obj = &str.object;
            str.object.next = manager.objects;
            manager.objects = obj;
            return obj;
        }
    }

    pub fn destroyObject(manager: *Manager, object: *Object) void {
        var previous: ?*Object = null;
        var heap = manager.objects;
        while (heap) |obj| {
            if (obj == object) {
                previous.next = obj.next;
            } else {
                previous = obj;
                heap = obj.next;
            }
        }
        object.deinit(manager.allocator());
    }

    pub fn allocObject(manager: *Manager, comptime T: type) !*Object {
        const res = try T.init(manager.allocator());
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return &res.object;
    }

    pub fn allocClosure(manager: *Manager, function: *Object.Function) !*Object.Closure {
        const res = try Object.Closure.init(manager.allocator(), function);
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return res;
    }

    pub fn allocUpvalue(manager: *Manager, slot: *Value) !*Object.Upvalue {
        const res = try Object.Upvalue.init(manager.allocator(), slot);
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return res;
    }

    pub fn allocClass(manager: *Manager, name: *Object.String) !*Object.Class {
        const res = try Object.Class.init(manager.allocator(), name);
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return res;
    }
    pub fn allocInstance(manager: *Manager, class: *Object.Class) !*Object.Instance {
        const res = try Object.Instance.init(manager.allocator(), class);
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return res;
    }

    pub fn allocBoundMethod(manager: *Manager, receiver: Value, method: *Object.Closure) !*Object.BoundMethod {
        const res = try Object.BoundMethod.init(manager.allocator(), receiver, method);
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return res;
    }

    pub fn collect(manager: *Manager) !void {
        manager.is_collecting = true;
        defer manager.is_collecting = false;

        manager.times_collected += 1;
        log.info("-- gc begin\n", .{});
        const before = manager.bytes_allocated;

        manager.gray_stack.clearRetainingCapacity();

        try manager.markRoots();
        try manager.traceReferences();
        manager.sweep();

        manager.removeWhiteStrings();

        log.info("-- gc end\n", .{});
        log.info("   before {d}; after {d}\n", .{ before, manager.bytes_allocated });
        log.info("   collected {d} bytes; next at {d}\n", .{ before - manager.bytes_allocated, manager.next_gc });
    }

    fn markRoots(manager: *Manager) !void {
        const vm: *VM = @alignCast(@fieldParentPtr("manager", manager));
        var ptr = vm.stack[0..].ptr;
        while (ptr != vm.stack_top) : (ptr += 1) {
            try manager.markValue(ptr[0]);
        }

        try manager.markObject(&vm.init_string.object);
        try manager.markTable(&vm.globals);
        try manager.markCompilerRoots(&vm.root_compiler);

        for (vm.frames[0..vm.frame_count]) |frame| {
            try manager.markObject(&frame.closure.object);
        }

        var upvalue = vm.openUpvalues;
        while (upvalue) |up| : (upvalue = upvalue.?.next) {
            try manager.markObject(&up.object);
        }
    }

    fn traceReferences(manager: *Manager) !void {
        while (manager.gray_stack.popOrNull()) |obj| {
            log.info("Blackening object: {any}\n", .{obj});
            try manager.blackenObject(obj);
        }
    }

    fn sweep(manager: *Manager) void {
        var previous: ?*Object = null;
        var object = manager.objects;

        while (object) |obj| {
            if (obj.is_marked) {
                obj.is_marked = false;
                previous = obj;
                object = obj.next;
            } else {
                const unreached = obj;
                object = obj.next;
                if (previous) |prev| {
                    prev.next = object;
                } else {
                    manager.objects = object;
                }

                unreached.deinit(manager.allocator());
            }
        }

        manager.next_gc = manager.bytes_allocated * GC_GROW_FACTOR;
    }

    fn removeWhiteStrings(manager: *Manager) void {
        for (manager.strings.table.entries) |entry| {
            if (entry.key) |key| {
                if (!key.object.is_marked) {
                    _ = manager.strings.table.delete(key);
                }
            }
        }
    }

    fn markValue(manager: *Manager, value: Value) !void {
        if (values.isObject(value)) {
            try manager.markObject(values.asObject(value));
        }
    }

    fn markArray(manager: *Manager, array: values.ValueArray) !void {
        for (array.items) |value| {
            try manager.markValue(value);
        }
    }

    fn blackenObject(manager: *Manager, object: *Object) !void {
        log.debug("{x} blacken ", .{@intFromPtr(object)});
        values.printObject(object, log.debug);
        log.debug("\n", .{});

        switch (object.tag) {
            .class => {
                const class = object.as(Object.Class);
                try manager.markObject(&class.name.object);
                try manager.markTable(&class.methods);
            },
            .closure => {
                const closure = object.as(Object.Closure);
                try manager.markObject(&closure.function.object);
                for (closure.upvalues) |upvalue| {
                    if (upvalue) |up| {
                        try manager.markObject(&up.object);
                    }
                }
            },
            .function => {
                const fun = object.as(Object.Function);
                if (fun.name) |name| {
                    try manager.markObject(&name.object);
                }
                try manager.markArray(fun.chunk.constants);
            },
            .instance => {
                const inst = object.as(Object.Instance);
                try manager.markObject(&inst.class.object);
                try manager.markTable(&inst.fields);
            },
            .bound_method => {
                const bound = object.as(Object.BoundMethod);
                try manager.markValue(bound.receiver);
                try manager.markObject(&bound.method.object);
            },
            .upvalue => try manager.markValue(object.as(Object.Upvalue).closed),
            .native, .string => {},
        }
    }

    fn markObject(manager: *Manager, object: *Object) !void {
        if (object.is_marked) return;
        object.is_marked = true;

        log.debug("{x} mark ", .{object});
        values.printObject(object, log.debug);
        log.debug("\n", .{});
        try manager.gray_stack.append(object);
    }

    fn markTable(manager: *Manager, table: *Table) !void {
        for (table.entries[0..table.count]) |entry| {
            if (entry.key) |key| {
                try manager.markObject(&key.object);
                try manager.markValue(entry.value);
            }
        }
    }

    fn markCompilerRoots(manager: *Manager, root: *Compiler) !void {
        var compiler: ?*Compiler = root;
        while (compiler) |c| {
            try manager.markObject(&c.function.object);
            compiler = c.inner;
        }
    }
};
