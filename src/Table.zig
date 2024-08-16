const std = @import("std");
const tracer = @import("tracer");
const Table = @This();
const Object = @import("Object.zig");
const values = @import("values.zig");
const Value = values.Value;

count: usize = 0,
entries: []Entry,
allocator: std.mem.Allocator,

pub const Entry = struct {
    key: ?*Object.String,
    value: Value,
};

const MAX_LOAD: f32 = 0.75;

pub fn init(allocator: std.mem.Allocator) !Table {
    return .{
        .entries = try allocator.alloc(Entry, 0),
        .allocator = allocator,
    };
}

pub fn deinit(table: *Table) void {
    table.allocator.free(table.entries);
}

fn maxLoad(table: *Table) usize {
    const cap: f32 = @floatFromInt(table.entries.len);
    const max = MAX_LOAD * cap;
    return @intFromFloat(max);
}

pub fn set(table: *Table, key: *Object.String, value: Value) !bool {
    if (table.count + 1 > table.maxLoad()) {
        try table.grow();
    }

    var entry = table.findEntry(key);
    const isNewKey = entry.key == null;
    if (isNewKey and std.meta.activeTag(entry.value) == .nil) table.count += 1;

    entry.key = key;
    entry.value = value;
    return isNewKey;
}

pub fn addAll(to: *Table, from: *Table) !void {
    for (from.entries) |entry| {
        if (entry.key) |k| {
            _ = try to.set(k, entry.value);
        }
    }
}

pub fn get(table: *Table, key: *Object.String) ?Value {
    const t = tracer.trace(@src(), "Table.get({s})", .{key.data});
    defer t.end();
    if (table.count == 0) return null;
    const entry = table.findEntry(key);
    if (entry.key) |_| {
        return entry.value;
    } else {
        return null;
    }
}

pub fn delete(table: *Table, key: *Object.String) bool {
    if (table.count == 0) return false;

    const entry = table.findEntry(key);
    if (entry.key == null) return false;

    entry.key = null;
    entry.value = values.TRUE_VAL;
    return true;
}

fn findEntry(table: *Table, key: *Object.String) *Entry {
    const t = tracer.trace(@src(), "Table.findEntry({s})", .{key.data});
    defer t.end();

    const cap_t = tracer.trace(@src(), "Table.findEntry CAP", .{});
    const cap = table.entries.len;
    const mask = cap - 1;
    cap_t.end();

    const index_t = tracer.trace(@src(), "Table.findEntry INDEX MOD", .{});
    var index = key.hash & mask;
    index_t.end();
    var tombstone: ?*Entry = null;
    while (true) {
        const i_t = tracer.trace(@src(), "Table.findEntry({s}) at {d}", .{ key.data, index });
        defer i_t.end();

        const entry = &table.entries[index];

        if (entry.key) |k| {
            const key_t = tracer.trace(@src(), "Table key comparison", .{});
            defer key_t.end();
            if (@intFromPtr(key) == @intFromPtr(k)) {
                return entry;
            }
        } else {
            const key_t = tracer.trace(@src(), "Table key null", .{});
            defer key_t.end();
            switch (entry.value) {
                .nil => return if (tombstone) |stone| stone else entry,
                .boolean => |b| {
                    std.debug.assert(b);
                    tombstone = entry;
                },
                else => unreachable,
            }
        }

        const i_mod_t = tracer.trace(@src(), "Table index wrap", .{});
        defer i_mod_t.end();
        index = (index + 1) & mask;
    }
}

fn grow(table: *Table) !void {
    const t = tracer.trace(@src(), "Table.grow", .{});
    defer t.end();

    const new_cap = if (table.entries.len < 8) 8 else table.entries.len * 2;
    const old_entries = table.entries;
    defer table.allocator.free(old_entries);
    table.entries = try table.allocator.alloc(Entry, new_cap);
    for (table.entries) |*entry| {
        entry.key = null;
        entry.value = values.NIL_VAL;
    }

    table.count = 0;
    for (old_entries) |entry| {
        if (entry.key) |k| {
            var dest = table.findEntry(k);
            dest.key = k;
            dest.value = entry.value;
            table.count += 1;
        }
    }
}

test "Basic set/get" {
    const key1_raw = "hello";
    const key2_raw = "world";
    var key1 = Object.String.from(key1_raw);
    var key2 = Object.String.from(key2_raw);

    var table = try Table.init(std.testing.allocator);
    defer table.deinit();

    try std.testing.expect(try table.set(&key1, .{ .number = 5 }));
    try std.testing.expect(try table.set(&key2, .{ .boolean = true }));

    const num = table.get(&key1);
    try std.testing.expectEqual(Value{ .number = 5 }, num.?);
    const b = table.get(&key2);
    try std.testing.expectEqual(Value{ .boolean = true }, b.?);
}
