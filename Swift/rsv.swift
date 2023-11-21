/* (C) Stefan John / Stenway / Stenway.com / 2023 */

import Foundation

extension String: Error {}

func encodeRsv(_ rows: [[String?]]) -> [UInt8] {
	var result: [UInt8] = []
	for row in rows {
		for value in row {
			if let strValue = value {
				if !strValue.isEmpty {
					let valueBytes = [UInt8](strValue.utf8)
					result.append(contentsOf: valueBytes)
				}
			} else {
				result.append(0xFE)
			}
			result.append(0xFF)
		}
		result.append(0xFD)
	}
	return result
}

func decodeRsv(_ bytes: [UInt8]) throws -> [[String?]] {
	if bytes.count > 0 && bytes[bytes.count-1] != 0xFD {
		throw "Incomplete RSV document"
	}
	var result: [[String?]] = []
	var currentRow: [String?] = []
	var valueStartIndex = 0
	for i in 0..<bytes.count {
		if bytes[i] == 0xFF {
			let length = i - valueStartIndex
			if length == 0 {
				currentRow.append("")
			} else if length == 1 && bytes[valueStartIndex] == 0xFE {
				currentRow.append(nil)
			} else {
				let strValue = String(bytes: bytes[valueStartIndex..<valueStartIndex+length], encoding: .utf8)
				if strValue == nil {
					throw "Invalid string value"
				}
				currentRow.append(strValue)
			}
			valueStartIndex = i + 1
		} else if bytes[i] == 0xFD {
			if i > 0 && valueStartIndex != i {
				throw "Incomplete RSV row"
			}
			result.append(currentRow)
			currentRow = []
			valueStartIndex = i + 1
		}
	}
	return result
}

// ----------------------------------------------------------------------

let rsvByteClassLookup: [UInt8] = [
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
]

let rsvStateTransitionLookup: [UInt8] = [
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
]

func isValidRsv(bytes: [UInt8]) -> Bool {
	var lastState: UInt8 = 1
	for i in 0..<bytes.count {
		let currentByte = bytes[i]
		let currentByteClass = rsvByteClassLookup[Int(currentByte)]
		let newStateLookupIndex = lastState * 15 + currentByteClass
		lastState = rsvStateTransitionLookup[Int(newStateLookupIndex)]
		if lastState == 0 {
			return false
		}
	}
	return lastState == 1
}

// ----------------------------------------------------------------------

func saveRsv(_ rows: [[String?]], _ filePath: String) throws {
	let bytes = encodeRsv(rows)
	let data = Data(bytes)
	try data.write(to: URL(fileURLWithPath: filePath))
}

func loadRsv(_ filePath: String) throws -> [[String?]] {
	let data = try Data(contentsOf: URL(fileURLWithPath: filePath))
	let bytes = [UInt8](data)
	return try decodeRsv(bytes)
}

func appendRsv(_ rows: [[String?]], _ filePath: String, _ continueLastRow: Bool) throws {
	let fileUrl = URL(fileURLWithPath: filePath)
	let file: FileHandle
	do {
		file = try FileHandle(forUpdating: fileUrl)
	} catch let e {
		FileManager.default.createFile(atPath: filePath, contents: nil)
		file = try FileHandle(forUpdating: fileUrl)
	}
	if continueLastRow && file.seekToEndOfFile() > 0 {
		file.seek(toFileOffset: file.seekToEndOfFile() - 1)
		let lastByte = file.readData(ofLength: 1).first
		if lastByte != 0xFD {
			throw "Incomplete RSV document"
		}
		if rows.isEmpty {
			return
		}
		file.seek(toFileOffset: file.seekToEndOfFile() - 1)
	} else {
		file.seekToEndOfFile()
	}
	
	let encodedData = encodeRsv(rows)
	try file.write(contentsOf: encodedData)
	file.closeFile()
}

// ----------------------------------------------------------------------

func escapeJsonString(_ str: String) -> String {
	var result = "\""
	for char in str.unicodeScalars {
		let c = char.value
		if c == 0x08 { result += "\\b" }
		else if c == 0x09 { result += "\\t" }
		else if c == 0x0A { result += "\\n" }
		else if c == 0x0C { result += "\\f" }
		else if c == 0x0D { result += "\\r" }
		else if c == 0x22 { result += "\\\"" }
		else if c == 0x5C { result += "\\\\" }
		else if c >= 0x00 && c <= 0x1F { result += "\\u00" + String(format: "%02x", c) }
		else { result += String(char) }
	}
	result += "\""
	return result
}

func rsvToJsonString(_ rows: [[String?]]) -> String {
	var sb = ""
	sb.append("[")
	var isFirstRow = true
	for row in rows {
		if !isFirstRow { sb.append(",") }
		isFirstRow = false
		sb.append("\n  [")
		var isFirstValue = true
		for value in row {
			if !isFirstValue { sb.append(", ") }
			isFirstValue = false
			if let strValue = value {
				sb.append(escapeJsonString(strValue))
			} else {
				sb.append("null")
			}
		}
		sb.append("]")
	}
	sb.append("\n]")
	return sb
}

// ----------------------------------------------------------------------

func checkTestFiles() throws {
	for i in 1...79 {
		let filePath = ".\\..\\TestFiles\\Valid_" + String(format: "%03d", i)
		print("Checking valid test file: "+filePath)
		let loadedRows = try loadRsv(filePath + ".rsv")
		let jsonStr = rsvToJsonString(loadedRows)
		
		let loadedJsonStr = try String(contentsOf: URL(fileURLWithPath: filePath + ".json"), encoding: .utf8)
		if jsonStr != loadedJsonStr {
			fatalError("JSON mismatch")
		}
		
		let data = try Data(contentsOf: URL(fileURLWithPath: filePath + ".rsv"))
		let bytes = [UInt8](data)
		if !isValidRsv(bytes: bytes) {
			fatalError("Validation mismatch")
		}
	}
	
	for i in 1...29 {
		let filePath = ".\\..\\TestFiles\\Invalid_" + String(format: "%03d", i)
		print("Checking invalid test file: "+filePath)
		var wasError = false
		do {
			let _ = try loadRsv(filePath + ".rsv")
		} catch let e {
			wasError = true
		}
		if !wasError {
			fatalError("RSV document is valid")
		}
		
		let data = try Data(contentsOf: URL(fileURLWithPath: filePath + ".rsv"))
		let bytes = [UInt8](data)
		if isValidRsv(bytes: bytes) {
			fatalError("Validation mismatch")
		}
	}
}

// ----------------------------------------------------------------------

do {
	let rows: [[String?]] = [
		["Hello", "🌎", nil, ""],
		["A\0B\nC", "Test 𝄞"],
		[],
		[""]
	]

	print(rsvToJsonString(rows))

	try saveRsv(rows, "Test.rsv")

	let loadedRows = try loadRsv("Test.rsv")
	print(rsvToJsonString(loadedRows))

	try saveRsv(loadedRows, "TestResaved.rsv")

	let appendRows: [[String?]] = [["ABC"]]
	try appendRsv(appendRows, "Append.rsv", false)

	try checkTestFiles()

	print("Done")
} catch let error {
	print(error)
}