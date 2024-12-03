const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const util = @import("util.zig");
const gpa = util.gpa;

const data = @embedFile("data/day01.txt");

pub fn part1(l1: []u32, l2: []u32) !u32 {
    // side effect of in place sort is fine here
    sort(u32, l1, {}, comptime asc(u32));
    sort(u32, l2, {}, comptime asc(u32));

    const n = l1.len;

    var delta: u32 = 0;

    for (0..n) |i| {
        delta += @max(l1[i], l2[i]) - @min(l1[i], l2[i]);
    }

    return delta;
}

pub fn part2(l1: []u32, l2: []u32) !u32 {
    var count_set = Map(u32, u32).init(gpa);

    for (l2) |n| {
        try count_set.put(n, (count_set.get(n) orelse 0) + 1);
    }

    var result: u32 = 0;

    for (l1) |n| {
        result += (count_set.get(n) orelse 0) * n;
    }

    return result;
}

pub fn parse(inp: []const u8) ![2][]u32 {
    var l1 = std.ArrayList(u32).init(gpa);
    var l2 = std.ArrayList(u32).init(gpa);

    var lines = splitSeq(u8, inp, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) break; // Ignore empty lines
        var parts = splitSeq(u8, line, "   ");
        const n1 = try parseInt(u32, parts.next().?, 10);
        const n2 = try parseInt(u32, parts.next().?, 10);
        try l1.append(n1);
        try l2.append(n2);
    }

    return .{ try l1.toOwnedSlice(), try l2.toOwnedSlice() };
}

pub fn main() !void {
    print("Day 01\n", .{});
    const parsed = try parse(data);
    const l1 = parsed[0];
    const l2 = parsed[1];
    const delta = try part1(l1, l2);
    print("Part 1: {}\n", .{delta});

    const result = try part2(l1, l2);
    print("Part 2: {}\n", .{result});
}

// Useful stdlib functions
const tokenizeAny = std.mem.tokenizeAny;
const tokenizeSeq = std.mem.tokenizeSequence;
const tokenizeSca = std.mem.tokenizeScalar;
const splitAny = std.mem.splitAny;
const splitSeq = std.mem.splitSequence;
const splitSca = std.mem.splitScalar;
const indexOf = std.mem.indexOfScalar;
const indexOfAny = std.mem.indexOfAny;
const indexOfStr = std.mem.indexOfPosLinear;
const lastIndexOf = std.mem.lastIndexOfScalar;
const lastIndexOfAny = std.mem.lastIndexOfAny;
const lastIndexOfStr = std.mem.lastIndexOfLinear;
const trim = std.mem.trim;
const sliceMin = std.mem.min;
const sliceMax = std.mem.max;

const parseInt = std.fmt.parseInt;
const parseFloat = std.fmt.parseFloat;

const print = std.debug.print;
const assert = std.debug.assert;

const sort = std.sort.block;
const asc = std.sort.asc;
const desc = std.sort.desc;

// Generated from template/template.zig.
// Run `zig build generate` to update.
// Only unmodified days will be updated.
