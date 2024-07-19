const std = @import("std");
const StringPool = @This();
const Object = @import("Object.zig");
const Table = @import("Table.zig");
const Value = @import("values.zig").Value;
const Manager = @import("memory.zig").Manager;

table: Table = undefined,

pub fn init(allocator: *Manager) StringPool {
    return .{
        .table = Table.init(allocator) catch unreachable,
    };
}

pub fn deinit(pool: *StringPool) void {
    pool.table.deinit();
}

pub fn set(pool: *StringPool, key: *Object.String, value: Value) !void {
    _ = try pool.table.set(key, value);
}

pub fn find(pool: *StringPool, raw: []const u8) ?*Object.String {
    if (pool.table.count == 0) return null;

    const hash = Object.hashString(raw);

    const cap = pool.table.entries.len;
    var index = hash % cap;
    while (true) {
        const entry = pool.table.entries[index];

        if (entry.key) |key| {
            if (key.data.len == raw.len and
                key.hash == hash and
                std.mem.eql(u8, raw, key.data))
            {
                return key;
            }
        } else {
            switch (entry.value) {
                .nil => return null,
                else => {},
            }
        }

        index = (index + 1) % cap;
    }
}
