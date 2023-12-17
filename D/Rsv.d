/* (C) Stefan John / Stenway / Stenway.com / 2023 */

import std.string;
import std.array;
import std.stdio;
import std.file;
import std.utf;

ubyte[] encodeRsv(string[][] rows) {
	Appender!(ubyte[]) result;
	ubyte[] valueTerminatorByte = [0xFF];
	ubyte[] nullValueByte = [0xFE];
	ubyte[] rowTerminatorByte = [0xFD];
	foreach (row; rows) {
		foreach (value; row) {
			if (value.ptr == null) { result.put(nullValueByte); }
			else if (value.length > 0) {
				validate(value);
				ubyte[] valueBytes = cast(ubyte[])value;
				result.put(valueBytes);
			}
			result.put(valueTerminatorByte);
		}
		result.put(rowTerminatorByte);
	}
	return result.data;
}

string[][] decodeRsv(ubyte[] bytes) {
	if (bytes.length > 0 && bytes[bytes.length-1] != 0xFD) {
		throw new Exception("Incomplete RSV document");
	}
	Appender!(string[][]) result;
	Appender!(string[]) currentRow;
	int valueStartIndex = 0;
	for (int i=0; i<bytes.length; i++) {
		if (bytes[i] == 0xFF) {
			int length = i-valueStartIndex;
		   	if (length == 0) {
				currentRow.put("");
			} else if (length == 1 && bytes[valueStartIndex] == 0xFE) {
				currentRow.put(null);
			} else {
				auto strValue = cast(string)bytes[valueStartIndex..valueStartIndex+length];
				validate(strValue);
				currentRow.put(strValue);
			}
			valueStartIndex = i+1;
		} else if (bytes[i] == 0xFD) {
			if (i > 0 && valueStartIndex != i) {
				throw new Exception("Incomplete RSV row");
			}
			result.put(currentRow.data.dup);
			currentRow.clear();
			valueStartIndex = i+1;
		}
	}
	return result.data;
}

// ----------------------------------------------------------------------

bool isValidRsv(ubyte[] bytes) {
	static const ubyte[256] rsvByteClassLookup = [
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
		3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
		4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
		4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
		0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
		5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
		6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 7, 7,
		9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 14
	];

	static const ubyte[180] rsvStateTransitionLookup = [
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11,
		0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 0, 0, 11,
		0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 6, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11,
		0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11
	];

	ubyte lastState = 1;
	for (int i=0; i<bytes.length; i++) {
		ubyte currentByte = bytes[i];
		ubyte currentByteClass = rsvByteClassLookup[currentByte];
		ubyte newStateLookupIndex = cast(ubyte)(lastState * 15 + currentByteClass);
		lastState = rsvStateTransitionLookup[newStateLookupIndex];
		if (lastState == 0) {
			return false;
		}
	}
	return lastState == 1;
}

// ----------------------------------------------------------------------

void saveRsv(string[][] rows, string filePath) {
	std.file.write(filePath, encodeRsv(rows));
}

string[][] loadRsv(string filePath) {
	ubyte[] bytes = cast(ubyte[])read(filePath);
	return decodeRsv(bytes);
}

File getAppendFile(string filePath) {
	try {
		return File(filePath, "rb+");
	} catch(Throwable) {
		return File(filePath, "wb+");
	}
}

void appendRsv(string[][] rows, string filePath, bool continueLastRow) {
	auto file = getAppendFile(filePath);
	auto fileSize = file.size;
	if (continueLastRow && fileSize > 0) {
		file.seek(fileSize - 1);
		ubyte[1] lastByte;
		file.rawRead(lastByte);
		if (lastByte[0] != 0xFD) {
			throw new Exception("Incomplete RSV document");
		}
		if (rows.length == 0) {
			return;
		}
	   	file.seek(fileSize - 1);
	} else {
		file.seek(fileSize);
	}
	auto bytes = encodeRsv(rows);
	file.rawWrite(bytes);
}

// ----------------------------------------------------------------------

void escapeJsonString(string str, Appender!string result) {
	result.put("\"");
	foreach (char c; str) {
		if (c == 0x08) { result.put("\\b"); }
		else if (c == 0x09) { result.put("\\t"); }
		else if (c == 0x0A) { result.put("\\n"); }
		else if (c == 0x0C) { result.put("\\f"); }
		else if (c == 0x0D) { result.put("\\r"); }
		else if (c == 0x22) { result.put("\\\""); }
		else if (c == 0x5C) { result.put("\\\\"); }
		else if (c >= 0x00 && c <= 0x1F) {
			result.put(format("\\u00%02x", c));
		} else { result.put(c); }
	}
	result.put("\"");
}

string rsvToJsonString(string[][] rows) {
	Appender!string result;
	result.put("[");
	bool isFirstRow = true;
	foreach (row; rows) {
		if (!isFirstRow) { result.put(","); }
		isFirstRow = false;
		result.put("\n  [");
		bool isFirstValue = true;
		foreach (value; row) {
			if (!isFirstValue) { result.put(", "); }
			isFirstValue = false;
			if (value.ptr == null) { result.put("null"); }
			else {
			   	escapeJsonString(value, result);
			}
		}
		result.put("]");
	}
	result.put("\n]");
	return result.data;
}

// ----------------------------------------------------------------------

void checkTestFiles() {
	for (int i=1; i<=79; i++) {
		string filePath = ".\\..\\TestFiles\\Valid_" ~ format("%03d", i);
		writeln("Checking valid test file: ", filePath);
		auto loadedRows = loadRsv(filePath ~ ".rsv");
		string jsonStr = rsvToJsonString(loadedRows);

		string loadedJsonStr = cast(string)read(filePath ~ ".json");
		if (jsonStr != loadedJsonStr) {
			throw new Exception("JSON mismatch");
		}
		
		if (!isValidRsv(cast(ubyte[])read(filePath ~ ".rsv"))) {
			throw new Exception("Validation mismatch");
		}
	}

	for (int i=1; i<=29; i++) {
		string filePath = ".\\..\\TestFiles\\Invalid_" ~ format("%03d", i);
		writeln("Checking invalid test file: ", filePath);
		bool wasError = false;
		try {
			auto loadedRows = loadRsv(filePath ~ ".rsv");
		} catch (Exception e) {
			wasError = true;
		}
		if (!wasError) {
			throw new Exception("RSV document is valid");
		}
		if (isValidRsv(cast(ubyte[])read(filePath ~ ".rsv"))) {
			throw new Exception("Validation mismatch");
		}
	}
}

// ----------------------------------------------------------------------

void main()
{
	string[][] rows = [
		["Hello", "🌎", null, ""],
		["A\0B\nC", "Test 𝄞"],
		[],
		[""]
	];
	writeln(rsvToJsonString(rows));
	saveRsv(rows, "Test.rsv");
	
	auto loadedRows = loadRsv("Test.rsv");
	writeln(rsvToJsonString(loadedRows));
	saveRsv(loadedRows, "TestResaved.rsv");
	
	appendRsv([["ABC"]], "Append.rsv", true);
	
	checkTestFiles();
	
	writeln("Done");
}