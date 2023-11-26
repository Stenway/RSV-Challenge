/* (C) Stefan John / Stenway / Stenway.com / 2023 */

using System;
using System.Collections;
using System.IO;
using System.Text;

namespace Rsv
{
	class Program
	{
		static uint8[256] utf8ByteClassLookup = .(
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
			9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
		);

		static uint8[108] utf8StateTransitionLookup = .(
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8,
			0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0
		);

		static bool IsValidUtf8(System.Span<uint8> str) {
			uint8 lastState = 1;
			for (let currentByte in str) {
				uint8 currentByteClass = utf8ByteClassLookup[currentByte];
				uint8 newStateLookupIndex = lastState*12+currentByteClass;
				lastState = utf8StateTransitionLookup[newStateLookupIndex];
				if (lastState == 0) { return false; }
			}
			return (lastState == 1);
		}

		// ----------------------------------------------------------------------

		static Result<List<uint8>> EncodeRsv(List<List<String>> rows) {
			var result = new List<uint8>();
			for (let row in rows) {
				for (let value in row) {
					if (value == null) { result.Add(0xFE); }
					else if (value.Length > 0) {
						var bytes = new uint8[value.Length];
						defer delete bytes;
						Internal.MemCpy(bytes.Ptr, value.Ptr, value.Length);
						if (!IsValidUtf8(bytes)) { return .Err; }
						result.AddRange(bytes);
					}
					result.Add(0xFF);
				}
				result.Add(0xFD);
			}
			return .Ok(result);
		}

		static Result<List<List<String>>> DecodeRsv(List<uint8> bytes) {
			if (bytes.Count > 0 && bytes[bytes.Count-1] != 0xFD) { return .Err; }
			var result = new List<List<String>>();
			var currentRow = new List<String>();
			var valueStartIndex = 0;
			var wasError = false;
			for (int i=0; i<bytes.Count; i++) {
				if (bytes[i] == 0xFF) {
					int length = i-valueStartIndex;
					if (length == 0) { currentRow.Add(""); }
					else if (length == 1 && bytes[valueStartIndex] == 0xFE) { currentRow.Add(null); }
					else {
						var valueBytes = bytes.GetRange(valueStartIndex, length);
						if (!IsValidUtf8(valueBytes)) { wasError = true; break; }
						var strValue = new String((char8*)valueBytes.Ptr, valueBytes.Length);
						Internal.MemCpy(strValue.Ptr, valueBytes.Ptr, valueBytes.Length);
						currentRow.Add(strValue);
					}
					valueStartIndex = i+1;
				} else if (bytes[i] == 0xFD) {
					if (i > 0 && valueStartIndex != i) { wasError = true; break; }
					result.Add(currentRow);
					currentRow = new List<String>();
					valueStartIndex = i+1;
				}
			}
			delete currentRow;
			if (wasError) {
				DeleteRows(result);
				return .Err;
			}
			return .Ok(result);
		}

		// ----------------------------------------------------------------------

		static uint8[256] rsvByteClassLookup = .(
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
		);

		static uint8[180] rsvStateTransitionLookup = .(
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
		);

		static bool IsValidRsv(System.Span<uint8> str) {
			uint8 lastState = 1;
			for (let currentByte in str) {
				uint8 currentByteClass = rsvByteClassLookup[currentByte];
				uint8 newStateLookupIndex = lastState*15+currentByteClass;
				lastState = rsvStateTransitionLookup[newStateLookupIndex];
				if (lastState == 0) { return false; }
			}
			return (lastState == 1);
		}

		// ----------------------------------------------------------------------

		static Result<void> SaveRsv(List<List<String>> rows, String filePath) {
			switch (EncodeRsv(rows)) {
			case .Ok(let bytes): {
				defer delete bytes;
				File.WriteAll(filePath, bytes);
			}
			case .Err: return .Err;
			}
			return .Ok;
		}

		static Result<List<List<String>>> LoadRsv(String filePath) {
			var bytes = new List<uint8>();
			defer delete bytes;
			File.ReadAll(filePath, bytes);
			return DecodeRsv(bytes);
		}

		// ----------------------------------------------------------------------

		static void EscapeJsonString(String value, String result) {
			result.Append("\"");
			for (int i=0; i<value.Length; i++) {
				var ch = value.Ptr[i];
				var c = (int)ch;
				if (c == 0x08) { result.Append("\\b"); }
				else if (c == 0x09) { result.Append("\\t"); }
				else if (c == 0x0A) { result.Append("\\n"); }
				else if (c == 0x0C) { result.Append("\\f"); }
				else if (c == 0x0D) { result.Append("\\r"); }
				else if (c == 0x22) { result.Append("\\\""); }
				else if (c == 0x5C) { result.Append("\\\\"); }
				else if (c >= 0x00 && c <= 0x1F) { result.AppendF("\\u00{0:x2}", c); }
				else { result.Append(ch); }
			}
			result.Append("\"");
		}

		static String RsvToJsonString(List<List<String>> rows) {
			var result = new String("[");
			bool isFirstRow = true;
			for (let row in rows) {
				if (!isFirstRow) { result.Append(","); }
				isFirstRow = false;
				result.Append("\n  [");
				bool isFirstValue = true;
				for (let value in row) {
					if (!isFirstValue) { result.Append(", "); }
					isFirstValue = false;
					if (value == null) { result.Append("null"); }
					else { EscapeJsonString(value, result); }
				}
				result.Append("]");
			}
			result.Append("\n]");
			return result;
		}

		static void PrintRows(List<List<String>> rows) {
			var str = RsvToJsonString(rows);
			Console.WriteLine(str);
			delete str;
		}

		static void DeleteRows(List<List<String>> rows) {
			for (let row in rows) {
				for (let value in row) {
					if (value != null && !value.HasExternalPtr) delete value;
				}
				delete row;
			}
			delete rows;
		}

		// ----------------------------------------------------------------------

		static void CheckTestFiles() {
			for (int i=1; i<=79; i++) {
				var filePath = scope String()..AppendF("./../TestFiles/Valid_{0:d3}", i);
				Console.Write("Checking valid test file: ");
				Console.WriteLine(filePath);

				var filePathRsv = scope String(filePath)..Append(".rsv");
				if (LoadRsv(filePathRsv) case .Ok(let loadedRows)) {
					defer DeleteRows(loadedRows);
					String jsonStr = RsvToJsonString(loadedRows);
					defer delete jsonStr;

					var filePathJson = scope String(filePath)..Append(".json");
					String loadedJsonStr = scope String();
					File.ReadAllText(filePathJson, loadedJsonStr);
					if (jsonStr != loadedJsonStr) {
						Runtime.FatalError("JSON mismatch");
					}
				}

				var bytes = new List<uint8>();
				defer delete bytes;
				File.ReadAll(filePathRsv, bytes);

				if (!IsValidRsv(bytes)) {
					Runtime.FatalError("Validation mismatch");
				}
			}

			for (int i=1; i<=29; i++) {
				var filePath = scope String()..AppendF("./../TestFiles/Invalid_{0:d3}", i);
				Console.Write("Checking invalid test file: ");
				Console.WriteLine(filePath);

				var filePathRsv = scope String(filePath)..Append(".rsv");
				if (LoadRsv(filePathRsv) case .Ok(let loadedRows)) {
					defer DeleteRows(loadedRows);
					Runtime.FatalError("RSV document is valid");
				}

				var bytes = new List<uint8>();
				defer delete bytes;
				File.ReadAll(filePathRsv, bytes);

				if (IsValidRsv(bytes)) {
					Runtime.FatalError("Validation mismatch");
				}
			}
		}

		// ----------------------------------------------------------------------

		static void Main()
		{
			var rows = new List<List<String>>();
			defer DeleteRows(rows);
			var row1 = new List<String>(String[] ("Hello", "🌎", null, ""));
			rows.Add(row1);
			var row2 = new List<String>(String[] ("A\0B\nC", "Test 𝄞"));
			rows.Add(row2);
			var row3 = new List<String>(String[] ());
			rows.Add(row3);
			var row4 = new List<String>(String[] (""));
			rows.Add(row4);

			PrintRows(rows);

			SaveRsv(rows, "Test.rsv");

			if (LoadRsv("Test.rsv") case .Ok(let loadedRows)) {
				defer DeleteRows(loadedRows);
				PrintRows(loadedRows);

				SaveRsv(loadedRows, "TestResaved.rsv");
			}

			CheckTestFiles();

			Console.WriteLine("Done");
		}
	}
}