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

    pub fn init(manager: *Manager, allocator: std.mem.Allocator) void {
        manager.arena = std.heap.ArenaAllocator.init(allocator);
        manager.gray_stack = std.ArrayList(*Object).init(allocator);
        manager.strings = StringPool.init(manager);
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
        manager.bytes_allocated += size_t * amount;
        if (!manager.is_collecting and manager.bytes_allocated > manager.next_gc) {
            try manager.collect();
        }
    }

    pub fn alloc(manager: *Manager, comptime T: type, size: usize) Error![]T {
        try manager.bookkeeping(@sizeOf(T), size);
        return manager.arena.allocator().alloc(T, size);
    }

    pub fn dupe(manager: *Manager, comptime T: type, original: []const T) Error![]T {
        try manager.bookkeeping(@sizeOf(T), original.len);
        return try manager.arena.allocator().dupe(T, original);
    }

    pub fn realloc(manager: *Manager, memory: anytype, new_size: usize) !@TypeOf(memory) {
        const item_size = @sizeOf(@typeInfo(@TypeOf(memory)).Slice.child);
        const new_memory = try manager.arena.allocator().realloc(memory, new_size);

        log.debug("{*} reallocating {d} for {s}\n", .{ memory, new_size, @typeName(@TypeOf(memory)) });

        manager.bytes_allocated += (new_size * item_size) - (memory.len + item_size);
        if (manager.bytes_allocated > manager.next_gc) {
            manager.collect();
        }

        return new_memory;
    }

    pub fn free(manager: *Manager, memory: anytype) void {
        log.debug("{*} freeing {d} for {s}\n", .{ memory, memory.len, @typeName(@TypeOf(memory)) });
        manager.arena.allocator().free(memory);

        const T: type = @typeInfo(@TypeOf(memory)).Pointer.child;
        const item_size = @sizeOf(T);
        manager.bookkeeping(item_size, memory.len) catch |e| {
            log.err("Error in bookkeeping for free: {any}\n", .{e});
        };
    }

    pub fn create(manager: *Manager, comptime T: type) Error!*T {
        try manager.bookkeeping(@sizeOf(T), 1);
        const ptr = try manager.arena.allocator().create(T);
        log.debug("{*} allocating {d} for {s}\n", .{ ptr, @sizeOf(T), @typeName(T) });
        return ptr;
    }

    pub fn destroy(manager: *Manager, ptr: anytype) void {
        manager.arena.allocator().destroy(ptr);
        manager.bytes_allocated -= @sizeOf(@typeInfo(@TypeOf(ptr)).Pointer.child);
    }

    pub fn inner(manager: *Manager) std.mem.Allocator {
        return manager.arena.allocator();
    }

    pub fn copy(manager: *Manager, str: []const u8) !*Object {
        const interned = manager.strings.find(str);
        log.debug("Copying identifier: {?} -> {}?\n", .{ interned, interned != null });

        if (interned) |i| {
            return &i.object;
        }
        try manager.bookkeeping(@sizeOf(Object.String), 1);
        const res = try Object.String.copy(str, manager);
        try manager.strings.set(res.as(Object.String), values.NIL_VAL);
        res.next = manager.objects;
        manager.objects = res;
        return res;
    }

    pub fn concat(manager: *Manager, a: []const u8, b: []const u8) !*Object {
        var new_raw = try manager.alloc(u8, a.len + b.len);
        @memcpy(new_raw[0..a.len], a);
        @memcpy(new_raw[a.len..], b);

        if (manager.strings.find(new_raw)) |interned| {
            manager.free(new_raw);
            return &interned.object;
        } else {
            try manager.bookkeeping(@sizeOf(Object.String), 1);
            const str = try Object.String.fromAlloc(new_raw, manager);
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
        const T = switch (object.tag) {
            .string => Object.String,
            .function => Object.Function,
            .native => Object.Native,
            .closure => Object.Closure,
            .upvalue => Object.Upvalue,
        };
        manager.bytes_allocated -= @sizeOf(T);
        object.deinit(manager);
    }

    pub fn allocObject(manager: *Manager, comptime T: type) !*Object {
        try manager.bookkeeping(@sizeOf(T), 1);
        manager.bytes_allocated += @sizeOf(T);
        const res = try T.init(manager);
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return &res.object;
    }

    pub fn allocClosure(manager: *Manager, function: *Object.Function) !*Object.Closure {
        try manager.bookkeeping(@sizeOf(Object.Closure), 1);
        const res = try Object.Closure.init(manager, function);
        res.object.next = manager.objects;
        manager.objects = &res.object;
        return res;
    }

    pub fn allocUpvalue(manager: *Manager, slot: *Value) !*Object.Upvalue {
        try manager.bookkeeping(@sizeOf(Object.Upvalue), 1);
        const res = try Object.Upvalue.init(manager, slot);
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
        log.info("   collected {d} bytes (from {d} to {d}) next at {d}\n", .{ before - manager.bytes_allocated, before, manager.bytes_allocated, manager.next_gc });
    }

    fn markRoots(manager: *Manager) !void {
        const vm: *VM = @alignCast(@fieldParentPtr("manager", manager));
        var ptr = vm.stack[0..].ptr;
        while (ptr != vm.stack_top) : (ptr += 1) {
            try manager.markValue(ptr[0]);
        }

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
        for (manager.gray_stack.items) |obj| {
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

                unreached.deinit(manager);
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
        switch (value) {
            .object => |obj| try manager.markObject(obj),
            else => {},
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
