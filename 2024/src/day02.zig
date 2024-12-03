const std = @import("std");
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Map = std.AutoHashMap;
const StrMap = std.StringHashMap;
const BitSet = std.DynamicBitSet;

const util = @import("util.zig");
const gpa = util.gpa;

const data = @embedFile("data/day02.txt");

const Input = [][]i32;

fn gt(a: i32, b: i32) bool {
    return a > b;
}

fn lt(a: i32, b: i32) bool {
    return a < b;
}

pub fn is_safe(reports: []i32) bool {
    const motion: bool = reports[1] > reports[0];

    for (1..reports.len) |i| {
        const diff = reports[i] - reports[i - 1];
        if (!(motion == (diff > 0) and diff != 0 and
            @abs(diff) <= 3))
        {
            return false;
        }
    }

    return true;
}

pub fn parse(inp: []const u8) !Input {
    var l = std.ArrayList([]i32).init(gpa);

    var lines = splitSeq(u8, inp, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) break; // Ignore empty lines
        var reports_raw = splitSca(u8, line, ' ');
        var reports = std.ArrayList(i32).init(gpa);
        while (reports_raw.next()) |report| {
            const n = parseInt(i32, report, 10) catch |err| {
                print("Failed to parse: {s}\n", .{report});
                return err;
            };
            try reports.append(n);
        }
        try l.append(try reports.toOwnedSlice());
    }

    return try l.toOwnedSlice();
}

pub fn main() !void {
    print("Day 02\n", .{});
    const parsed = try parse(data);

    var part1: usize = 0;
    var part2: usize = 0;

    for (parsed) |line| {
        if (is_safe(line)) {
            part1 += 1;
            continue;
        }
        for (0..line.len) |i| {
            var filtered = std.ArrayList(i32).init(gpa);
            defer filtered.deinit();

            for (0..line.len) |j| {
                if (j != i) {
                    try filtered.append(line[j]);
                }
            }

            if (is_safe(try filtered.toOwnedSlice())) {
                part2 += 1;
                break;
            }
        }
    }

    print("Part 1: {}\n", .{part1});

    print("Part 2: {}\n", .{part1 + part2});
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
