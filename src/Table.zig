const std = @import("std");
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
            try to.set(k, entry.value);
        }
    }
}

pub fn get(table: *Table, key: *Object.String) ?Value {
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
    const cap = table.entries.len;
    var index = key.hash % cap;
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &table.entries[index];

        if (entry.key) |k| {
            if (key == k) {
                return entry;
            }
        } else {
            switch (entry.value) {
                .nil => return if (tombstone) |t| t else entry,
                .boolean => |b| {
                    std.debug.assert(b);
                    tombstone = entry;
                },
                else => unreachable,
            }
        }

        index = (index + 1) % cap;
    }
}

fn grow(table: *Table) !void {
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
    const allocator = std.testing.allocator;
    const key1_raw = "hello";
    const key2_raw = "world";
    var key1 = try Object.String.from(key1_raw);
    var key2 = try Object.String.from(key2_raw);

    var table = try Table.init(allocator);
    defer table.deinit();

    try std.testing.expect(try table.set(&key1, .{ .number = 5 }));
    try std.testing.expect(try table.set(&key2, .{ .boolean = true }));

    const num = table.get(&key1);
    try std.testing.expectEqual(Value{ .number = 5 }, num.?);
    const b = table.get(&key2);
    try std.testing.expectEqual(Value{ .boolean = true }, b.?);
}
