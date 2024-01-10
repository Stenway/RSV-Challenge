/* (C) Stefan John / Stenway / Stenway.com / 2023 */
#nullable enable

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

static byte[] EncodeXRsv(string?[][] rows) {
	var parts = new List<byte[]>();
	var valueTerminatorByte = new byte[]{0xFF};
	var nullValueByte = new byte[]{0xFE};
	var rowTerminatorByte = new byte[]{0xFD};
	var encoder = new UTF8Encoding(false, true);
	foreach (var row in rows) {
		foreach (var value in row) {
			if (value == null) { parts.Add(nullValueByte); }
			else {
				 if (value.Length > 0) parts.Add(encoder.GetBytes(value));
				parts.Add(valueTerminatorByte);
			}
		}
		parts.Add(rowTerminatorByte);
	}
	var result = new byte[parts.Sum(part => part.Length)];
	var stream = new MemoryStream(result);
	foreach (var part in parts) { stream.Write(part, 0, part.Length); }
	return result;
}

static string?[][] DecodeXRsv(byte[] bytes) {
	if (bytes.Length > 0 && bytes[bytes.Length-1] != 0xFD) { throw new Exception("Incomplete XRSV document"); }
	var decoder = new UTF8Encoding(false, true);
	var result = new List<string?[]>();
	var currentRow = new List<string?>();
	int valueStartIndex = 0;
	for (int i=0; i<bytes.Length; i++) {
		if (bytes[i] == 0xFF) {
			int length = i-valueStartIndex;
			if (length == 0) { currentRow.Add(""); }
			else {
				var valueBytes = bytes.Skip(valueStartIndex).Take(length).ToArray();
				currentRow.Add(decoder.GetString(valueBytes));
			}
			valueStartIndex = i+1;
		} else if (bytes[i] == 0xFE) {
			currentRow.Add(null);
			valueStartIndex = i+1;
		} else if (bytes[i] == 0xFD) {
			if (i > 0 && valueStartIndex != i) { throw new Exception("Incomplete XRSV row"); }
			result.Add(currentRow.ToArray());
			currentRow.Clear();
			valueStartIndex = i+1;
		}
	}
	return result.ToArray();
}

// ----------------------------------------------------------------------

static void SaveXRsv(string?[][] rows, string filePath) {
	File.WriteAllBytes(filePath, EncodeXRsv(rows));
}

static string?[][] LoadXRsv(string filePath) {
	return DecodeXRsv(File.ReadAllBytes(filePath));
}

static void AppendXRsv(string?[][] rows, string filePath, bool continueLastRow = false) {
	using (FileStream fileStream = File.Open(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite)) {
		if (continueLastRow && fileStream.Length > 0) {
			fileStream.Position = fileStream.Length - 1;
			if (fileStream.ReadByte() != 0xFD) { throw new Exception("Incomplete XRSV document"); }
			if (rows.Length == 0) return;
			fileStream.Position = fileStream.Length - 1;
		} else {
			fileStream.Position = fileStream.Length;
		}
		fileStream.Write(EncodeXRsv(rows));
	}
}

// ----------------------------------------------------------------------

static bool IsValidXRsv(byte[] bytes) {
	byte[] byteClassLookup = {
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
	};
	byte[] stateTransitionLookup = {
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 10,
		0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 0, 0, 10,
		0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 6, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 10
	};
	int lastState = 1;
	for (int i=0; i<bytes.Length; i++) {
		byte currentByte = bytes[i];
		int currentByteClass = byteClassLookup[currentByte];
		int newStateLookupIndex = lastState*15+currentByteClass;
		lastState = stateTransitionLookup[newStateLookupIndex];
		if (lastState == 0) { return false; }
	}
	return (lastState == 1);
}
	
// ----------------------------------------------------------------------

static string ByteArrayToString(byte[] bytes) {
	return "["+string.Join(", ", bytes)+"]";
}

// ----------------------------------------------------------------------

static string EscapeJsonString(string str) {
	StringBuilder result = new StringBuilder();
	result.Append("\"");
	for (int i = 0; i < str.Length; i++) {
		char c = str[i];
		if (c == 0x08) { result.Append("\\b"); }
		else if (c == 0x09) { result.Append("\\t"); }
		else if (c == 0x0A) { result.Append("\\n"); }
		else if (c == 0x0C) { result.Append("\\f"); }
		else if (c == 0x0D) { result.Append("\\r"); }
		else if (c == 0x22) { result.Append("\\\""); }
		else if (c == 0x5C) { result.Append("\\\\"); }
		else if (c >= 0x00 && c <= 0x1F) { result.AppendFormat("\\u{0:x4}", (int)c); }
		else { result.Append(c); }
	}
	result.Append("\"");
	return result.ToString();
}

static string XRsvToJson(string?[][] rows) {
	return "["+(rows.Length > 0 ? "\n" : "")+String.Join(",\n", rows.Select((row) => "  ["+String.Join(", ", row.Select(x => x == null ? "null" : EscapeJsonString(x)))+"]"))+"\n]";
}

static void PrintXRsvToJson(string?[][] rows) {
	Console.WriteLine(XRsvToJson(rows));
}

// ----------------------------------------------------------------------

static void CheckTestFiles() {
	for (var i=1; i<=79; i++) {
		var filePath = ".\\..\\TestFiles\\Valid_" + i.ToString("D3");
		Console.WriteLine("Checking valid test file: " + filePath);
		var loadedRows = LoadXRsv(filePath + ".xrsv");
		var jsonStr = XRsvToJson(loadedRows);
				
		var loadedJsonStr = File.ReadAllText(filePath + ".json");
		if (jsonStr != loadedJsonStr) {
			throw new Exception("JSON mismatch");
		}
		
		if (!IsValidXRsv(File.ReadAllBytes(filePath + ".xrsv"))) {
			throw new Exception("Validation mismatch");
		}
	}
	
	for (var i=1; i<=28; i++) {
		var filePath = ".\\..\\TestFiles\\Invalid_" + i.ToString("D3");
		Console.WriteLine("Checking invalid test file: " + filePath);
		var wasError = false;
		try {
			var loadedRows = LoadXRsv(filePath + ".xrsv");
		} catch(Exception e) {
			wasError = true;
		}
		if (!wasError) {
			throw new Exception("XRSV document is valid");
		}
		
		if (IsValidXRsv(File.ReadAllBytes(filePath + ".xrsv"))) {
			throw new Exception("Validation mismatch");
		}
	}
}

// ----------------------------------------------------------------------

string?[][] rows = new []{
	new []{"Hello", "🌎", null, ""},
	new []{"A\0B\nC", "Test 𝄞"},
	new string?[]{},
	new []{""}
};

Console.OutputEncoding = System.Text.Encoding.UTF8;

PrintXRsvToJson(rows);
var bytes = EncodeXRsv(rows);
//Console.WriteLine(ByteArrayToString(bytes));
var decodedRows = DecodeXRsv(bytes);
SaveXRsv(rows, "Test.xrsv");

var loadedRows = LoadXRsv("Test.xrsv");
PrintXRsvToJson(loadedRows);
SaveXRsv(loadedRows, "TestResaved.xrsv");

AppendXRsv(new []{new []{"ABC"}}, "Append.xrsv", false);

CheckTestFiles();

Console.WriteLine("Done");