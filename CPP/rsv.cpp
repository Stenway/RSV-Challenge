/* (C) Stefan John / Stenway / Stenway.com / 2023 */

#include <iostream>
#include <vector>
#include <string>
#include <optional>
#include <sstream>
#include <fstream>
using namespace std;
using namespace std::string_literals;

// ----------------------------------------------------------------------

static const uint8_t utf8ByteClassLookup[256] = {
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
};

static const uint8_t utf8StateTransitionLookup[108] = {
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8,
	0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

bool isValidUtf8(string& str) {
	uint8_t lastState = 1;
	for (char c : str) {
		uint8_t currentByte = (uint8_t)c;
		uint8_t currentByteClass = utf8ByteClassLookup[currentByte];
		uint8_t newStateLookupIndex = lastState*12+currentByteClass;
		lastState = utf8StateTransitionLookup[newStateLookupIndex];
		if (lastState == 0) { return false; }
	}
	return (lastState == 1);
}

// ----------------------------------------------------------------------

vector<uint8_t> encodeRsv(vector<vector<optional<string>>>& rows) {
	vector<uint8_t> result;
	for (auto row : rows) {
		for (auto value : row) {
			if (!value.has_value()) { result.push_back(0xFE); }
			else if (value.value().length() > 0) {
				string strValue = value.value();
				if (!isValidUtf8(strValue)) { throw runtime_error("Invalid string value"); }
				result.insert(result.end(), strValue.begin(), strValue.end());
			}
			result.push_back(0xFF);
		}
		result.push_back(0xFD);
	}
	return result;
}

vector<vector<optional<string>>> decodeRsv(vector<uint8_t>& bytes) {
	if (bytes.size() > 0 && bytes[bytes.size()-1] != 0xFD) { throw runtime_error("Incomplete RSV document"); }
	vector<vector<optional<string>>> result{};
	vector<optional<string>> currentRow{};
	int valueStartIndex = 0;
	for (int i=0; i<bytes.size(); i++) {
		if (bytes[i] == 0xFF) {
			int length = i-valueStartIndex;
			if (length == 0) { currentRow.push_back(""); }
			else if (length == 1 && bytes[valueStartIndex] == 0xFE) { currentRow.push_back({}); }
			else {
				string strValue(bytes.begin()+valueStartIndex, bytes.begin()+valueStartIndex+length);
				if (!isValidUtf8(strValue)) { throw runtime_error("Invalid string value"); }
				currentRow.push_back(strValue);
			}
			valueStartIndex = i+1;
		} else if (bytes[i] == 0xFD) {
			if (i > 0 && valueStartIndex != i) { throw runtime_error("Incomplete RSV row"); }
			result.push_back(currentRow);
			currentRow = {};
			valueStartIndex = i+1;
		}
	}
	return result;
}

// ----------------------------------------------------------------------

static const uint8_t rsvByteClassLookup[256] = {
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

static const uint8_t rsvStateTransitionLookup[180] = {
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
};

bool isValidRsv(vector<uint8_t>& bytes) {
	uint8_t lastState = 1;
	for (int i=0; i<bytes.size(); i++) {
		uint8_t currentByte = bytes[i];
		uint8_t currentByteClass = rsvByteClassLookup[currentByte];
		uint8_t newStateLookupIndex = lastState*15+currentByteClass;
		lastState = rsvStateTransitionLookup[newStateLookupIndex];
		if (lastState == 0) { return false; }
	}
	return (lastState == 1);
}

// ----------------------------------------------------------------------

vector<uint8_t> loadFile(string filePath) {
	ifstream file(filePath, ios_base::binary);
	if (!file) { throw runtime_error("Could not open file"); }
	
	file.seekg(0, std::ios_base::end);
	auto size = file.tellg();
	file.seekg(0, std::ios_base::beg);
	if (size < 0) { throw runtime_error("Could not determine file size"); }
	
	vector<uint8_t> bytes(size);
	file.read((char*)bytes.data(), size);
	if (file.gcount() != size) { throw runtime_error("Not all bytes read"); }
	return bytes;
}

void saveFile(vector<uint8_t>& bytes, string filePath) {
	ofstream file(filePath, ios_base::binary);
	file.write((char*)bytes.data(), bytes.size());
	if (!file.good()) { throw runtime_error("Writing all bytes failed"); }
}

// ----------------------------------------------------------------------

void saveRsv(vector<vector<optional<string>>>& rows, string filePath) {
	auto bytes = encodeRsv(rows);
	saveFile(bytes, filePath);
}

vector<vector<optional<string>>> loadRsv(string filePath) {
	auto bytes = loadFile(filePath);
	return decodeRsv(bytes);
}

void appendRsv(vector<vector<optional<string>>>& rows, string filePath, bool continueLastRow) {
	FILE* file = fopen(filePath.c_str(), "rb+");
	if (file == nullptr) {
		file = fopen(filePath.c_str(), "wb+");
		if (file == nullptr) { throw runtime_error("Opening file failed"); }
	}
	fseek(file, 0, SEEK_END);
	long fileSize = ftell(file);
	if (continueLastRow && fileSize > 0) {
		fseek(file, fileSize - 1, SEEK_SET);
		uint8_t lastByte;
		fread(&lastByte, 1, 1, file);
		if (lastByte != 0xFD) {
			fclose(file);
			throw runtime_error("Incomplete RSV document");
		}
		if (rows.size() == 0) {
			fclose(file);
		}
		fseek(file, fileSize - 1, SEEK_SET);
	}
	auto bytes = encodeRsv(rows);
	if (fwrite((char*)bytes.data(), bytes.size(), 1, file) != 1) {
		fclose(file);
		throw runtime_error("Writing all bytes failed");
	}
	fclose(file);
}

// ----------------------------------------------------------------------

string escapeJsonString(string& str) {
	stringstream result;
	result << "\"";
	for (int i=0; i<str.length(); i++) {
		char c = str[i];
		if (c == 0x08) { result << "\\b"; }
		else if (c == 0x09) { result << "\\t"; }
		else if (c == 0x0A) { result << "\\n"; }
		else if (c == 0x0C) { result << "\\f"; }
		else if (c == 0x0D) { result << "\\r"; }
		else if (c == 0x22) { result << "\\\""; }
		else if (c == 0x5C) { result << "\\\\"; }
		else if (c >= 0x00 && c <= 0x1F) {
			char escapeStr[7];
			sprintf(escapeStr, "\\u00%02x", c);
			result << escapeStr;
		} else { result << (char)c; }
	}
	result << "\"";
	return result.str();
}

string rsvToJsonString(vector<vector<optional<string>>>& rows) {
	stringstream result;
	result << "[";
	bool isFirstRow = true;
	for (auto row : rows) {
		if (!isFirstRow) { result << ","; }
		isFirstRow = false;
		result << "\n  [";
		bool isFirstValue = true;
		for (auto value : row) {
			if (!isFirstValue) { result << ", "; }
			isFirstValue = false;
			if (!value.has_value()) { result << "null"; }
			else {
				result << escapeJsonString(value.value());
			}
		}
		result << "]";
	}
	result << "\n]";
	return result.str();
}

// ----------------------------------------------------------------------

string loadTextFile(string filePath) {
	vector<uint8_t> bytes  = loadFile(filePath);
	string strValue(bytes.begin(), bytes.end());
	return strValue;
}

void checkTestFiles() {
	for (int i=1; i<=79; i++) {
		char filePath[256];
		sprintf(filePath, ".\\..\\TestFiles\\Valid_%03d.rsv", i);
		cout << "Checking valid test file: " << filePath << endl;
		vector<vector<optional<string>>> loadedRows = loadRsv(filePath);
		string jsonStr = rsvToJsonString(loadedRows);
		
		char jsonFilePath[256];
		sprintf(jsonFilePath, ".\\..\\TestFiles\\Valid_%03d.json", i);
		string loadedJsonStr = loadTextFile(jsonFilePath);
		if (jsonStr != loadedJsonStr) {
			throw runtime_error("JSON mismatch");
		}
	
		auto bytes = loadFile(filePath);
		if (!isValidRsv(bytes)) {
			throw new runtime_error("Validation mismatch");
		}
	}
	
	for (int i=1; i<=29; i++) {
		char filePath[256];
		sprintf(filePath, ".\\..\\TestFiles\\Invalid_%03d.rsv", i);
		cout << "Checking valid test file: " << filePath << endl;
		bool wasError = false;
		try {
			vector<vector<optional<string>>> loadedRows = loadRsv(filePath);
		} catch(...) {
			wasError = true;
		}
		if (!wasError) {
			throw runtime_error("RSV document is valid");
		}
		auto bytes = loadFile(filePath);
		if (isValidRsv(bytes)) {
			throw runtime_error("Validation mismatch");
		}
	}
}

// ----------------------------------------------------------------------
	
int main() {
	vector<vector<optional<string>>> rows {
		{"Hello", "🌎", {}, ""},
		{"A\0B\nC"s, "Test 𝄞"},
		{},
		{""}
	};
	cout << rsvToJsonString(rows) << endl;
	
	auto bytes = encodeRsv(rows);
	auto decodedRows = decodeRsv(bytes);
	
	saveRsv(rows, "Test.rsv");
	
	auto loadedRows = loadRsv("Test.rsv");
	cout << rsvToJsonString(loadedRows) << endl;
	
	saveRsv(loadedRows, "TestResaved.rsv");

	vector<vector<optional<string>>> append {
		{"ABC"}
	};
	appendRsv(append, "Append.rsv", false);

	checkTestFiles();

	cout << "Done" << endl;
	return 0;
}