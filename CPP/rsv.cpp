/* (C) Stefan John / Stenway / Stenway.com / 2023 */

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>
using namespace std;
using namespace std::string_literals;
namespace fs = std::filesystem;

// ----------------------------------------------------------------------
// Constants

enum class RSV {
	VALUE_TERMINAL = 0xFF,
	NULL_VALUE = 0xFE,
	ROW_TERMINAL = 0xFD,
};

enum class Escapes {
	BACKSPACE = 0x08,
	TAB = 0x09,
	NEW_LINE = 0x0A,
	CARRIAGE_RETURN = 0x0D,
	FORM_FEED = 0x0C,
	DOUBLE_QUOTE = 0x22,
	BACKSLASH = 0x5C,
	CONTROL_LOWERBOUND = 0x00,
	CONTROL_UPPERBOUND = 0x1F,
};

enum class Tests { MAX_TESTS = 79, MAX_INVALIDTESTS = 29 };

// ----------------------------------------------------------------------
// Helper class to handle file lifetime

class FileHandle {
  public:
	explicit FileHandle(const string &filePath, const string &mode) {
		file_ = unique_ptr<FILE, FileDeleter>(
			fopen(filePath.c_str(), mode.c_str()));
		if (file_ == nullptr) {
			throw runtime_error("Could not open file: "s + filePath +
								"; Error: " + strerror(errno));
		}
	}

	~FileHandle() = default;

	FileHandle(FileHandle &&other) noexcept = default;
	FileHandle &operator=(FileHandle &&other) noexcept = default;

	[[nodiscard]] FILE *get() const { return file_.get(); }

	FileHandle(const FileHandle &) = delete;
	FileHandle &operator=(const FileHandle &) = delete;

  private:
	struct FileDeleter {
		void operator()(FILE *ptr) const {
			if (ptr != nullptr) {
				// Since this class fully handles the lifetime of the file
				// handle, we can safely ignore any issues with ownership
				// transfer. NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
				if (fclose(ptr) != 0) {
					cerr << "Could not close file" << endl;
				}
			}
		}
	};

	unique_ptr<FILE, FileDeleter> file_;
};

// ----------------------------------------------------------------------
// UTF-8 Byte Lookup & Transition Tables

// clang-format off

static const array<uint8_t, 256> utf8ByteClassLookup = {
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
	3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
	4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
	4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
	0,  0,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
	5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
	6,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  8,  7,  7,
	9, 10, 10, 10, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

static const array<uint8_t, 108> utf8StateTransitionLookup = {
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

// clang-format on

// UTF-8 validation state machine

bool isValidUtf8(string &str) {
	uint8_t lastState = 1;
	const uint8_t unicodeTranslationWidth = 12;
	for (char chr : str) {
		auto currentByte = (uint8_t)chr;
		uint8_t currentByteClass = utf8ByteClassLookup.at(currentByte);
		uint8_t newStateLookupIndex =
			lastState * unicodeTranslationWidth + currentByteClass;
		lastState = utf8StateTransitionLookup.at(newStateLookupIndex);
		if (lastState == 0) {
			return false;
		}
	}
	return (lastState == 1);
}

// ----------------------------------------------------------------------
// RSV encoding and decoding

vector<uint8_t> encodeRsv(vector<vector<optional<string>>> &rows) {
	vector<uint8_t> result;
	for (auto &row : rows) {
		for (auto value : row) {
			if (!value.has_value()) {
				result.push_back((int)RSV::NULL_VALUE);
			} else if (value.value().length() > 0) {
				string strValue = value.value();
				if (!isValidUtf8(strValue)) {
					throw runtime_error("Invalid string value");
				}
				result.insert(result.end(), strValue.begin(), strValue.end());
			}
			result.push_back((int)RSV::VALUE_TERMINAL);
		}
		result.push_back((int)RSV::ROW_TERMINAL);
	}
	return result;
}

vector<vector<optional<string>>> decodeRsv(vector<uint8_t> &bytes) {
	if (!bytes.empty() && bytes[bytes.size() - 1] != (int)RSV::ROW_TERMINAL) {
		throw runtime_error("Incomplete RSV document");
	}
	vector<vector<optional<string>>> result{};
	vector<optional<string>> currentRow{};
	int valueStartIndex = 0;
	for (int i = 0; i < static_cast<int>(bytes.size()); i++) {
		if (bytes[i] == (int)RSV::VALUE_TERMINAL) {
			int length = i - valueStartIndex;
			if (length == 0) {
				currentRow.emplace_back("");
			} else if (length == 1 &&
					   bytes[valueStartIndex] == (int)RSV::NULL_VALUE) {
				currentRow.emplace_back();
			} else {
				string strValue(bytes.begin() + valueStartIndex,
								bytes.begin() + valueStartIndex + length);
				if (!isValidUtf8(strValue)) {
					throw runtime_error("Invalid string value");
				}
				currentRow.emplace_back(strValue);
			}
			valueStartIndex = i + 1;
		} else if (bytes[i] == (int)RSV::ROW_TERMINAL) {
			if (i > 0 && valueStartIndex != i) {
				throw runtime_error("Incomplete RSV row");
			}
			result.push_back(currentRow);
			currentRow = {};
			valueStartIndex = i + 1;
		}
	}
	return result;
}

// ----------------------------------------------------------------------
// Lookup and Transition Tables for RSV validation state machine
//
// clang-format off

static const array<uint8_t, 256> rsvByteClassLookup = {
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
	2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
	3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
	4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
	4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
	0,  0,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
	5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
	6,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  8,  7,  7,
	9, 10, 10, 10, 11,  0,  0,  0,  0,  0,  0,  0,  0, 12, 13, 14
};

static const array<uint8_t, 180> rsvStateTransitionLookup = {
	0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  2,  0,  0,  0,  3,  4,  6,  5,  7,  8,  9,  1, 10, 11,
	0,  2,  0,  0,  0,  3,  4,  6,  5,  7,  8,  9,  0,  0, 11,
	0,  0,  2,  2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  3,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  3,  3,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  0,  6,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  6,  6,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 11,
	0,  2,  0,  0,  0,  3,  4,  6,  5,  7,  8,  9,  1, 10, 11
};

// clang-format on

// RSV state machine

bool isValidRsv(vector<uint8_t> &bytes) {
	uint8_t lastState = 1;
	const uint8_t rsvStateTrasitionWidth = 15;
	for (unsigned char currentByte : bytes) {
		uint8_t currentByteClass = rsvByteClassLookup.at(currentByte);
		uint8_t newStateLookupIndex =
			lastState * rsvStateTrasitionWidth + currentByteClass;
		lastState = rsvStateTransitionLookup.at(newStateLookupIndex);
		if (lastState == 0) {
			return false;
		}
	}
	return (lastState == 1);
}

// ----------------------------------------------------------------------
// File utilities

void checkFileOperation(int result, const string &message) {
	if (result != 0) {
		throw runtime_error(message);
	}
}

vector<uint8_t> loadFile(const string &filePath) {
	ifstream file(filePath, ios_base::binary);
	if (!file) {
		throw runtime_error("Could not open file");
	}

	file.seekg(0, std::ios_base::end);
	auto size = file.tellg();
	file.seekg(0, std::ios_base::beg);
	if (size < 0) {
		throw runtime_error("Could not determine file size");
	}

	vector<uint8_t> bytes(size);

	// Dealing with char* here is not ideal, but it's the only way to get
	// ifstream to read binary data without any conversions.
	// Since we're dealing with raw binary data, reinterpret_cast is the
	// only way to go for casting between pointer types.
	// NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
	file.read(reinterpret_cast<char *>(bytes.data()), size);
	if (file.gcount() != size) {
		throw runtime_error("Not all bytes read");
	}
	return bytes;
}

void saveFile(vector<uint8_t> &bytes, const string &filePath) {
	ofstream file(filePath, ios_base::binary);
	if (!file) {
		throw runtime_error("Could not open file");
	}

	auto itr = bytes.begin();
	const size_t maxChunkSize = numeric_limits<streamsize>::max();

	while (distance(itr, bytes.end()) > 0) {
		size_t remaining = distance(itr, bytes.end());
		size_t chunkSize = min(remaining, maxChunkSize);

		// Again, since we're dealing with raw binary data, reinterpret_cast is
		// the only way to go for casting between pointer types.
		// NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
		file.write(reinterpret_cast<const char *>(&(*itr)),
				   static_cast<streamsize>(chunkSize));
		if (!file) {
			throw runtime_error("Writing all bytes failed");
		}
		advance(itr, chunkSize);
	}
}

// ----------------------------------------------------------------------
// RSV utilities

void saveRsv(vector<vector<optional<string>>> &rows, const string &filePath) {
	auto bytes = encodeRsv(rows);
	saveFile(bytes, filePath);
}

vector<vector<optional<string>>> loadRsv(const string &filePath) {
	auto bytes = loadFile(filePath);
	return decodeRsv(bytes);
}

void appendRsv(vector<vector<optional<string>>> &rows, string &filePath,
			   bool continueLastRow) {
	FileHandle fileHandle(
		filePath, "ab+"); // ab+ will create the file if it doesn't exist
	FILE *file = fileHandle.get();
	if (fseek(file, 0, SEEK_END) != 0) {
		throw runtime_error("Seeking to end of file failed");
	}
	long fileSize = ftell(file);
	if (continueLastRow && fileSize > 0) {
		if (fseek(file, fileSize - 1, SEEK_SET) != 0) {
			throw runtime_error("Seeking to last byte failed");
		}
		uint8_t lastByte = 0;
		if (fread(&lastByte, 1, 1, file) != 1) {
			throw runtime_error("Reading last byte failed");
		}
		if (lastByte != static_cast<int>(RSV::ROW_TERMINAL)) {
			throw runtime_error("Incomplete RSV document");
		}
		if (rows.empty()) {
			return;
		}
		if (fseek(file, fileSize - 1, SEEK_SET) != 0) {
			throw runtime_error("Seeking to last byte failed");
		}
	}
	auto bytes = encodeRsv(rows);
	if (fwrite(static_cast<unsigned char *>(bytes.data()), bytes.size(), 1,
			   file) != 1) {
		throw runtime_error("Writing all bytes failed");
	}
}

// ----------------------------------------------------------------------
// JSON utilities

string escapeJsonString(string &str) {
	stringstream result;
	result << "\"";
	for (char chr : str) {
		switch (chr) {
		case (int)Escapes::BACKSPACE:
			result << "\\b";
			break;
		case (int)Escapes::TAB:
			result << "\\t";
			break;
		case (int)Escapes::NEW_LINE:
			result << "\\n";
			break;
		case (int)Escapes::FORM_FEED:
			result << "\\f";
			break;
		case (int)Escapes::CARRIAGE_RETURN:
			result << "\\r";
			break;
		case (int)Escapes::DOUBLE_QUOTE:
			result << "\\\"";
			break;
		case (int)Escapes::BACKSLASH:
			result << "\\\\";
			break;
		default:
			if (chr >= (int)Escapes::CONTROL_LOWERBOUND &&
				chr <= (int)Escapes::CONTROL_UPPERBOUND) {
				result << "\\u00" << hex << setw(2) << setfill('0')
					   << static_cast<int>(static_cast<unsigned char>(chr));
			} else {
				result << chr;
			}
		}
	}
	result << "\"";
	return result.str();
}

string rsvToJsonString(vector<vector<optional<string>>> &rows) {
	stringstream result;
	result << "[";
	bool isFirstRow = true;
	for (auto &row : rows) {
		if (!isFirstRow) {
			result << ",";
		}
		isFirstRow = false;
		result << "\n  [";
		bool isFirstValue = true;
		for (auto value : row) {
			if (!isFirstValue) {
				result << ", ";
			}
			isFirstValue = false;
			if (!value.has_value()) {
				result << "null";
			} else {
				result << escapeJsonString(value.value());
			}
		}
		result << "]";
	}
	result << "\n]";
	return result.str();
}

// ----------------------------------------------------------------------
// Test utilities

string loadTextFile(const string &filePath) {
	vector<uint8_t> bytes = loadFile(filePath);
	string strValue(bytes.begin(), bytes.end());
	return strValue;
}

void checkTestFiles() {
	// using filesystem pathing so this will work on Windows and Linux
	fs::path testDir = fs::path(R"(./../TestFiles/)");
	if (!fs::exists(testDir)) {
		throw runtime_error("Test directory does not exist");
	}
	if (!fs::is_directory(testDir)) {
		throw runtime_error("Test directory is not a directory");
	}

	for (int i = 1; i <= (int)Tests::MAX_TESTS; i++) {
		ostringstream filePathStream;
		filePathStream << "Valid_" << setfill('0') << setw(3) << i << ".rsv";
		fs::path rsvPath = testDir / filePathStream.str();
		cout << "Checking valid test file: " << rsvPath << endl;

		vector<vector<optional<string>>> loadedRows = loadRsv(rsvPath.string());
		string jsonStr = rsvToJsonString(loadedRows);

		filePathStream.str("");
		filePathStream.clear();

		filePathStream << "Valid_" << setfill('0') << setw(3) << i << ".json";
		fs::path jsonPath = testDir / filePathStream.str();
		string loadedJsonStr = loadTextFile(jsonPath.string());
		if (jsonStr != loadedJsonStr) {
			throw runtime_error("JSON mismatch");
		}

		auto bytes = loadFile(rsvPath.string());
		if (!isValidRsv(bytes)) {
			throw runtime_error("Validation mismatch");
		}
	}

	for (int i = 1; i <= (int)Tests::MAX_INVALIDTESTS; i++) {
		ostringstream filePathStream;
		filePathStream << "Invalid_" << setfill('0') << setw(3) << i << ".rsv";
		fs::path rsvPath = testDir / filePathStream.str();
		cout << "Checking invalid test file: " << rsvPath << endl;
		bool wasError = false;
		try {
			vector<vector<optional<string>>> loadedRows =
				loadRsv(rsvPath.string());
		} catch (...) {
			wasError = true;
		}
		if (!wasError) {
			throw runtime_error("RSV document is valid");
		}

		auto bytes = loadFile(rsvPath.string());
		if (isValidRsv(bytes)) {
			throw runtime_error("Validation mismatch");
		}
	}
}

// ----------------------------------------------------------------------

int main() {
	try {
		vector<vector<optional<string>>> rows{
			{"Hello", "🌎", {}, ""},
            {"A\0B\nC"s, "Test 𝄞"},
            {},
            {""}};
		cout << rsvToJsonString(rows) << endl;

		auto bytes = encodeRsv(rows);
		auto decodedRows = decodeRsv(bytes);

		saveRsv(rows, "Test.rsv");

		auto loadedRows = loadRsv("Test.rsv");
		cout << rsvToJsonString(loadedRows) << endl;

		saveRsv(loadedRows, "TestResaved.rsv");

		vector<vector<optional<string>>> append{{"ABC"}};
		string appendFilePath = "Append.rsv";
		appendRsv(append, appendFilePath, false);
		auto appendedRows = loadRsv(appendFilePath);
		cout << rsvToJsonString(appendedRows) << endl;

		checkTestFiles();

		cout << "Done" << endl;
	} catch (exception &ex) {
		cout << "Error: " << ex.what() << endl;
		return 1;
	}
	return 0;
}
