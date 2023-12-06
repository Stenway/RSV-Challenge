/* (C) Stefan John / Stenway / Stenway.com / 2023 */

import "dart:convert";
import "dart:typed_data";
import "dart:io";

// ----------------------------------------------------------------------

bool isValidUtf16String(String str) {
	for (int i=0; i<str.length; i++) {
		var firstCodeUnit = str.codeUnitAt(i);
		if (firstCodeUnit >= 0xD800 && firstCodeUnit <= 0xDFFF) {
			if (firstCodeUnit >= 0xDC00) { return false; }
			i++;
			if (i >= str.length) { return false; }
			var secondCodeUnit = str.codeUnitAt(i);
			if (!(secondCodeUnit >= 0xDC00 && secondCodeUnit <= 0xDFFF)) { return false; }
		}
	}
	return true;
}

String decodeUtf8(Uint8List bytes) {
	var result = utf8.decode(bytes);
	if (bytes.length >= 3 && bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF) {
		result = "\uFEFF" + result;
	}
	return result;
}

// ----------------------------------------------------------------------

Uint8List encodeRsv(List<List<String?>> rows) {
	var result = BytesBuilder();
	for (var row in rows) {
		for (var value in row) {
			if (value == null) { result.addByte(0xFE); }
			else if (value.length > 0) {
				if (!isValidUtf16String(value)) {
					throw Exception("Invalid string value");
				}
				var valueBytes = utf8.encode(value);
				result.add(valueBytes);
			}
			result.addByte(0xFF);
		}
		result.addByte(0xFD);
	}
	return result.toBytes();
}

List<List<String?>> decodeRsv(Uint8List bytes) {
	if (bytes.length > 0 && bytes[bytes.length - 1] != 0xFD) {
		throw Exception("Incomplete RSV document");
	}
	List<List<String?>> result = [];
	List<String?> currentRow = [];
	int valueStartIndex = 0;
	for (int i = 0; i < bytes.length; i++) {
		if (bytes[i] == 0xFF) {
			int length = i - valueStartIndex;
			if (length == 0) { currentRow.add(""); }
			else if (length == 1 && bytes[valueStartIndex] == 0xFE) { currentRow.add(null); }
			else {
				var valueBytes = bytes.sublist(valueStartIndex, valueStartIndex + length);
				String strValue = decodeUtf8(valueBytes);
				currentRow.add(strValue);
			}
			valueStartIndex = i + 1;
		} else if (bytes[i] == 0xFD) {
			if (i > 0 && valueStartIndex != i) {
				throw Exception("Incomplete RSV row");
			}
			result.add(currentRow);
			currentRow = [];
			valueStartIndex = i + 1;
		}
	}
	return result;
}

// ----------------------------------------------------------------------

Uint8List rsvByteClassLookup = Uint8List.fromList([
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
]);

Uint8List rsvStateTransitionLookup = Uint8List.fromList([
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
]);

bool isValidRsv(Uint8List bytes) {
	int lastState = 1;
	for (int i = 0; i < bytes.length; i++) {
		int currentByte = bytes[i];
		int currentByteClass = rsvByteClassLookup[currentByte];
		int newStateLookupIndex = lastState * 15 + currentByteClass;
		lastState = rsvStateTransitionLookup[newStateLookupIndex];
		if (lastState == 0) {
			return false;
		}
	}
	return lastState == 1;
}

// ----------------------------------------------------------------------

void saveRsvSync(List<List<String?>> rows, String filePath) {
	File(filePath).writeAsBytesSync(encodeRsv(rows));
}

List<List<String?>> loadRsvSync(String filePath) {
	return decodeRsv(File(filePath).readAsBytesSync());
}

void appendRsvSync(List<List<String?>> rows, String filePath, bool continueLastRow) {
	RandomAccessFile file = File(filePath).openSync(mode: FileMode.append);
	try {
		if (continueLastRow && file.lengthSync() > 0) {
			file.setPositionSync(file.lengthSync() - 1);
			if ((file.readByteSync() & 0xFF) != 0xFD) {
				throw Exception("Incomplete RSV document");
			}
			if (rows.isEmpty) return;
			file.setPositionSync(file.lengthSync() - 1);
		} else {
			file.setPositionSync(file.lengthSync());
		}
		file.writeFromSync(encodeRsv(rows));
	} finally {
		file.closeSync();
	}
}

// ----------------------------------------------------------------------

String rsvToJson(List<List<String?>>rows) {
	return "[" + (rows.length > 0 ? "\n" : "") + rows.map(
		(row) {
			return "  [" + row.map((x) => jsonEncode(x)).join(", ") + "]";
		}
	).join(",\n") + "\n]";
}

// ----------------------------------------------------------------------


void checkTestFiles() {
	for (int i = 1; i <= 79; i++) {
		String filePath = "./../TestFiles/Valid_" + i.toString().padLeft(3, '0');
		print("Checking valid test file: $filePath");
		List<List<String?>> loadedRows = loadRsvSync('$filePath.rsv');
		String jsonStr = rsvToJson(loadedRows);
		String loadedJsonStr = File('$filePath.json').readAsStringSync();
		if (jsonStr != loadedJsonStr) {
			print(jsonStr);
			print(loadedJsonStr);
			throw Exception("JSON mismatch");
		}
		if (!isValidRsv(File('$filePath.rsv').readAsBytesSync())) {
			throw Exception("Validation mismatch");
		}
	}
	for (int i = 1; i <= 29; i++) {
		String filePath = "./../TestFiles/Invalid_" + i.toString().padLeft(3, '0');
		print("Checking invalid test file: $filePath");
		bool wasError = false;
		try {
			loadRsvSync('$filePath.rsv');
		} catch (e) {
			wasError = true;
		}
		if (!wasError) {
			throw Exception("RSV document is valid");
		}
		if (isValidRsv(File('$filePath.rsv').readAsBytesSync())) {
			throw Exception("Validation mismatch");
		}
	}
}

// ----------------------------------------------------------------------

void main() {
	List<List<String?>> rows = [
		["Hello", "🌎", null, ""],
		["A\x00B\nC", "Test 𝄞"],
		[],
		[""]
	];
	print(rsvToJson(rows));
	saveRsvSync(rows, "Test.rsv");
	
	var loadedRows = loadRsvSync("Test.rsv");
	print(rsvToJson(loadedRows));
	saveRsvSync(loadedRows, "TestResaved.rsv");
	
	List<List<String?>> appendRows = [["ABC"]];
	appendRsvSync(appendRows, "Append.rsv", true);
	
	checkTestFiles();
	
	print("Done");
}

