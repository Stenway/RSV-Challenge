// (C) Stefan John / Stenway / Stenway.com / 2023

const std = @import("std");

pub const RsvError = error{ InvalidStringValue, IncompleteRsvDocument, IncompleteRsvRow };

fn encodeRsv(rows: std.ArrayList(std.ArrayList(?[]const u8)), allocator: std.mem.Allocator) ![]const u8 {
    var result = std.ArrayList([]const u8).init(allocator);
    defer result.deinit();
    var result_length: usize = 0;
    for (rows.items) |row| {
        for (row.items) |value| {
            if (value) |strValue| {
                if (strValue.len > 0) {
                    if (!std.unicode.utf8ValidateSlice(strValue)) {
                        return error.InvalidStringValue;
                    }
                    try result.append(strValue);
                    result_length += strValue.len;
                }
            } else {
                try result.append("\xFE");
                result_length += 1;
            }
            try result.append("\xFF");
            result_length += 1;
        }
        try result.append("\xFD");
        result_length += 1;
    }
    var result_str = try allocator.alloc(u8, result_length);
    var index: usize = 0;
    for (result.items) |result_part| {
        std.mem.copy(u8, result_str[index..], result_part);
        index += result_part.len;
    }
    return result_str;
}

fn decodeRsv(bytes: []u8, allocator: std.mem.Allocator) !std.ArrayList(std.ArrayList(?[]const u8)) {
    if (bytes.len > 0 and bytes[bytes.len - 1] != 0xFD) {
        return error.IncompleteRsvDocument;
    }
    var result = std.ArrayList(std.ArrayList(?[]const u8)).init(allocator);
    var current_row = std.ArrayList(?[]const u8).init(allocator);
    var value_start_index: usize = 0;
    var i: usize = 0;
    while (i < bytes.len) : (i += 1) {
        if (bytes[i] == 0xFF) {
            var length = i - value_start_index;
            if (length == 0) {
                try current_row.append("");
            } else if (length == 1 and bytes[value_start_index] == 0xFE) {
                try current_row.append(null);
            } else {
                var value_bytes = bytes[value_start_index .. value_start_index + length];
                if (!std.unicode.utf8ValidateSlice(value_bytes)) {
                    return error.InvalidStringValue;
                }
                try current_row.append(try std.mem.Allocator.dupe(allocator, u8, value_bytes));
            }
            value_start_index = i + 1;
        } else if (bytes[i] == 0xFD) {
            if (i > 0 and value_start_index != i) {
                return error.IncompleteRsvRow;
            }
            try result.append(current_row);
            current_row = std.ArrayList(?[]const u8).init(allocator);
            value_start_index = i + 1;
        }
    }
    return result;
}

// ----------------------------------------------------------------------

const rsvByteClassLookup: [256]u8 = .{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 7, 7, 9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 14 };

const rsvStateTransitionLookup: [180]u8 = .{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11, 0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 0, 0, 11, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11 };

fn isValidRsv(bytes: []const u8) bool {
    var lastState: u8 = 1;
    var i: usize = 0;
    while (i < bytes.len) : (i += 1) {
        const currentByte = bytes[i];
        const currentByteClass = rsvByteClassLookup[currentByte];
        const newStateLookupIndex = lastState * 15 + currentByteClass;
        lastState = rsvStateTransitionLookup[newStateLookupIndex];
        if (lastState == 0) return false;
    }
    return lastState == 1;
}

// ----------------------------------------------------------------------

fn saveRsv(rows: std.ArrayList(std.ArrayList(?[]const u8)), file_path: []const u8, allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().createFile(file_path, .{});
    defer file.close();

    const bytes = try encodeRsv(rows, allocator);
    defer allocator.free(bytes);

    try file.writeAll(bytes);
}

fn loadRsv(file_path: []const u8, allocator: std.mem.Allocator) !std.ArrayList(std.ArrayList(?[]const u8)) {
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();
    const file_size = (try file.stat()).size;
    const bytes = try file.readToEndAlloc(allocator, file_size);
    defer allocator.free(bytes);

    return decodeRsv(bytes, allocator);
}

fn getAppendFile(file_path: []const u8) !std.fs.File {
    return std.fs.cwd().openFile(file_path, .{ .mode = std.fs.File.OpenMode.read_write }) catch {
        return try std.fs.cwd().createFile(file_path, .{ .read = true });
    };
}

fn appendRsv(rows: std.ArrayList(std.ArrayList(?[]const u8)), file_path: []const u8, continue_last_row: bool, allocator: std.mem.Allocator) !void {
    var file = try getAppendFile(file_path);
    defer file.close();
    const file_size = (try file.stat()).size;
    if (continue_last_row and file_size > 0) {
        try file.seekTo(file_size - 1);
        var buffer: [1]u8 = undefined;
        var bytesRead = try file.read(&buffer);
        if (bytesRead == 0 or buffer[0] != 0xFD) {
            return error.IncompleteRsvDocument;
        }
        if (rows.items.len == 0) return;
        try file.seekTo(file_size - 1);
    } else {
        try file.seekTo(file_size);
    }

    const bytes = try encodeRsv(rows, allocator);
    defer allocator.free(bytes);
    try file.writeAll(bytes);
}

// ----------------------------------------------------------------------

fn escapeJsonString(str: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var result_length: usize = 2;
    var i: usize = 0;
    while (i < str.len) : (i += 1) {
        var b = str[i];
        if ((b == 0x08) or (b == 0x09) or (b == 0x0A) or (b == 0x0C) or (b == 0x0D) or (b == 0x22) or (b == 0x5C)) {
            result_length += 2;
        } else if ((b >= 0) and (b <= 0x1F)) {
            result_length += 6;
        } else {
            result_length += 1;
        }
    }
    var result_str = try allocator.alloc(u8, result_length);
    result_str[0] = 0x22;
    i = 0;
    var j: usize = 1;
    while (i < str.len) : (i += 1) {
        var b = str[i];
        if (b == 0x08) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x62;
            j += 2;
        } else if (b == 0x09) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x74;
            j += 2;
        } else if (b == 0x0A) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x6E;
            j += 2;
        } else if (b == 0x0C) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x66;
            j += 2;
        } else if (b == 0x0D) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x72;
            j += 2;
        } else if (b == 0x22) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x22;
            j += 2;
        } else if (b == 0x5C) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x5C;
            j += 2;
        } else if ((b >= 0) and (b <= 0x1F)) {
            result_str[j] = 0x5C;
            result_str[j + 1] = 0x75;
            result_str[j + 2] = 0x30;
            result_str[j + 3] = 0x30;
            var buffer: [2]u8 = undefined;
            var slice = try std.fmt.bufPrint(&buffer, "{x:0>2}", .{b});
            _ = slice;
            result_str[j + 4] = buffer[0];
            result_str[j + 5] = buffer[1];
            j += 6;
        } else {
            result_str[j] = b;
            j += 1;
        }
    }
    result_str[j] = 0x22;
    return result_str;
}

fn rsvToJsonString(rows: std.ArrayList(std.ArrayList(?[]const u8)), allocator: std.mem.Allocator) ![]const u8 {
    var result = std.ArrayList([]const u8).init(allocator);
    defer result.deinit();
    var result_length: usize = 0;
    try result.append("[");
    result_length += 1;
    var is_first_row = true;
    for (rows.items) |row| {
        if (!is_first_row) {
            try result.append(",");
            result_length += 1;
        }
        is_first_row = false;
        try result.append("\n  [");
        result_length += 4;
        var is_first_value = true;
        for (row.items) |value| {
            if (!is_first_value) {
                try result.append(", ");
                result_length += 2;
            }
            is_first_value = false;
            if (value) |strValue| {
                var escapedStr = try escapeJsonString(strValue, allocator);
                try result.append(escapedStr);
                result_length += escapedStr.len;
            } else {
                try result.append("null");
                result_length += 4;
            }
        }
        try result.append("]");
        result_length += 1;
    }
    try result.append("\n]");
    result_length += 2;
    var result_str = try allocator.alloc(u8, result_length);
    var index: usize = 0;
    for (result.items) |result_part| {
        std.mem.copy(u8, result_str[index..], result_part);
        index += result_part.len;
    }
    return result_str;
}

fn printRows(rows: std.ArrayList(std.ArrayList(?[]const u8)), allocator: std.mem.Allocator) !void {
    var str = try rsvToJsonString(rows, allocator);
    std.debug.print("{s}\n", .{str});
}

// ----------------------------------------------------------------------

fn loadFile(file_path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();
    const file_size = (try file.stat()).size;
    const bytes = try file.readToEndAlloc(allocator, file_size);
    return bytes;
}

fn checkTestFiles(allocator: std.mem.Allocator) !void {
    var i: usize = 1;
    while (i <= 79) : (i += 1) {
        var buffer: [256]u8 = undefined;
        var file_path_rsv = try std.fmt.bufPrint(&buffer, "./../TestFiles/Valid_{d:0>3}.rsv", .{i});
        var buffer2: [256]u8 = undefined;
        var file_path_json = try std.fmt.bufPrint(&buffer2, "./../TestFiles/Valid_{d:0>3}.json", .{i});
        std.debug.print("Checking valid test file: {s}\n", .{file_path_rsv});

        var loaded_rows = try loadRsv(file_path_rsv, allocator);
        var json_str = try rsvToJsonString(loaded_rows, allocator);
        var loaded_json = try loadFile(file_path_json, allocator);
        defer allocator.free(loaded_json);

        if (std.mem.eql(u8, json_str, loaded_json) == false) {
            std.debug.panic("JSON mismatch", .{});
        }

        var bytes = try loadFile(file_path_rsv, allocator);
        defer allocator.free(bytes);
        if (!isValidRsv(bytes)) {
            std.debug.panic("Validation mismatch", .{});
        }
    }
    i = 1;
    while (i <= 29) : (i += 1) {
        var buffer: [256]u8 = undefined;
        var file_path_rsv = try std.fmt.bufPrint(&buffer, "./../TestFiles/Invalid_{d:0>3}.rsv", .{i});
        std.debug.print("Checking invalid test file: {s}\n", .{file_path_rsv});

        if (loadRsv(file_path_rsv, allocator)) |loaded_rows| {
            _ = loaded_rows;
            std.debug.panic("RSV document is valid", .{});
        } else |_| {}

        var bytes = try loadFile(file_path_rsv, allocator);
        defer allocator.free(bytes);
        if (isValidRsv(bytes)) {
            std.debug.panic("Validation mismatch", .{});
        }
    }
}

// ----------------------------------------------------------------------

pub fn main() !void {
    var allocator = std.heap.page_allocator;

    var rows = std.ArrayList(std.ArrayList(?[]const u8)).init(allocator);

    var row_1 = std.ArrayList(?[]const u8).init(allocator);
    try row_1.append("Hello");
    try row_1.append("🌎");
    try row_1.append(null);
    try row_1.append("");
    try rows.append(row_1);

    var row_2 = std.ArrayList(?[]const u8).init(allocator);
    try row_2.append("A\x00B\nC");
    try row_2.append("Test 𝄞");
    try rows.append(row_2);

    var row_3 = std.ArrayList(?[]const u8).init(allocator);
    try rows.append(row_3);

    var row_4 = std.ArrayList(?[]const u8).init(allocator);
    try row_4.append("");
    try rows.append(row_4);

    try printRows(rows, allocator);

    try saveRsv(rows, "Test.rsv", allocator);

    const loaded_rows = try loadRsv("Test.rsv", allocator);
    try printRows(loaded_rows, allocator);

    var append_rows = std.ArrayList(std.ArrayList(?[]const u8)).init(allocator);

    var append_row_1 = std.ArrayList(?[]const u8).init(allocator);
    try append_row_1.append("ABC");
    try append_rows.append(append_row_1);

    try appendRsv(append_rows, "Append.rsv", true, allocator);

    try checkTestFiles(allocator);

    std.debug.print("Done", .{});
}
