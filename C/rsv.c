/* (C) Stefan John / Stenway / Stenway.com / 2023 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

// ----------------------------------------------------------------------

typedef struct Bytes {
	uint8_t* buffer;
	size_t length;
} Bytes;

Bytes* createBytes(size_t length) {
	Bytes* result = malloc(sizeof(Bytes));
	if (result == NULL) { return NULL; }
	result->length = length;
	result->buffer = malloc(result->length);
	if (result == NULL) {
		free(result);
		return NULL;
	}
	return result;
}

void deleteBytes(Bytes* bytes) {
	if (bytes == NULL) { return; }
	free(bytes->buffer);
	bytes->buffer = NULL;
	bytes->length = 0;
	free(bytes);
}

Bytes* singleByte(uint8_t value) {
	Bytes* result = createBytes(1);
	if (result == NULL) { return NULL; }
	result->buffer[0] = value;
	return result;
}

// ----------------------------------------------------------------------

typedef Bytes String;

String* createString(size_t length) {
	String* result = malloc(sizeof(String));
	if (result == NULL) { return NULL; }
	result->length = length;
	result->buffer = malloc(result->length);
	if (result == NULL) {
		free(result);
		return NULL;
	}
	return result;
}

String* copyString(char* source, size_t length) {
	String* result = malloc(sizeof(String));
	result->length = length;
	result->buffer = malloc(result->length);
	memcpy(result->buffer, source, result->length);
	return result;
}

void deleteString(String* string) {
	deleteBytes((Bytes*)string);
}

#define STRLIT(s) (copyString(s, sizeof(s)-1))

// ----------------------------------------------------------------------

#define DYNAMIC_ARRAY(typeName, itemType, itemsName, deleteItemFunction) \
typedef struct typeName { \
	itemType* itemsName; \
	size_t length; \
	size_t capacity; \
} typeName; \
\
typeName* create##typeName(size_t capacity) { \
	typeName* result = malloc(sizeof(typeName)); \
	if (result == NULL) { return NULL; } \
	result->itemsName = malloc(capacity*sizeof(itemType)); \
	if (result->itemsName == NULL) { \
		free(result); \
		return NULL; \
	} \
	result->length = 0; \
	result->capacity = capacity; \
	return result; \
} \
\
bool appendTo##typeName(typeName* array, itemType item) { \
	if (array->length == array->capacity) { \
		size_t newCapacity = array->capacity * 2; \
		itemType* temp = realloc(array->itemsName, newCapacity*sizeof(itemType)); \
		if (temp == NULL) { return false; } \
		array->itemsName = temp; \
		array->capacity = newCapacity; \
	} \
	array->itemsName[array->length] = item; \
	array->length++; \
	return true; \
} \
\
void delete##typeName(typeName* array) { \
	for (size_t i=0; i<array->length; i++) { \
		deleteItemFunction(array->itemsName[i]); \
	} \
	free(array->itemsName); \
	array->itemsName = NULL; \
	array->length = 0; \
	array->capacity = 0; \
	free(array); \
} \
\
void delete##typeName##Shallow(typeName* array) { \
	free(array->itemsName); \
	array->itemsName = NULL; \
	array->length = 0; \
	array->capacity = 0; \
	free(array); \
}

DYNAMIC_ARRAY(BytesList, Bytes*, values, deleteBytes)
DYNAMIC_ARRAY(Row, String*, values, deleteString)
DYNAMIC_ARRAY(Rows, Row*, rows, deleteRow)

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

bool isValidUtf8(String* string) {
	uint8_t lastState = 1;
	for (int i=0; i<string->length; i++) {
		uint8_t currentByte = string->buffer[i];
		uint8_t currentByteClass = utf8ByteClassLookup[currentByte];
		uint8_t newStateLookupIndex = lastState*12+currentByteClass;
		lastState = utf8StateTransitionLookup[newStateLookupIndex];
		if (lastState == 0) { return false; }
	}
	return (lastState == 1);
}

// ----------------------------------------------------------------------

#define DEFAULT_ROWS_CAPACITY 16
#define DEFAULT_LINE_CAPACITY 4

Bytes* encodeRsv(Rows* rows) {
	BytesList* parts = createBytesList(128);
	Bytes* valueTerminatorByte = singleByte(0xFF);
	Bytes* nullValueByte = singleByte(0xFE);
	Bytes* rowTerminatorByte = singleByte(0xFD);
	bool errorOccurred = false;
	size_t resultLength = 0;
	for (int i=0; i<rows->length; i++) {
		Row* row = rows->rows[i];
		for (int j=0; j<row->length; j++) {
			String* value = row->values[j];
			if (value == NULL) {
				if (appendToBytesList(parts, nullValueByte) == false) {
					errorOccurred = true;
					break;
				}
				resultLength++;
			} else if (value->length > 0) {
				if (isValidUtf8(value) == false || appendToBytesList(parts, value) == false) {
					errorOccurred = true;
					break;
				}
				resultLength += value->length;
			}
			if (appendToBytesList(parts, valueTerminatorByte) == false) {
				errorOccurred = true;
				break;
			}
			resultLength++;
		}
		if (errorOccurred) { break; }
		if (appendToBytesList(parts, rowTerminatorByte) == false) {
			errorOccurred = true;
			break;
		}
		resultLength++;
	}
	Bytes* result = NULL;
	if (errorOccurred == false) {
		result = createBytes(resultLength);
		if (result != NULL) {
			size_t currentPos = 0;
			for (size_t i=0; i<parts->length; i++) {
				Bytes* part = parts->values[i];
				memcpy(&result->buffer[currentPos], part->buffer, part->length);
				currentPos += part->length;
			}
		};
	}
	deleteBytes(valueTerminatorByte);
	deleteBytes(nullValueByte);
	deleteBytes(rowTerminatorByte);
	deleteBytesListShallow(parts);
	return result;
}

Rows* decodeRsv(Bytes bytes) {
	if (bytes.length > 0 && bytes.buffer[bytes.length-1] != 0xFD) { return NULL; }
	Rows* result = createRows(DEFAULT_ROWS_CAPACITY);
	Row* currentRow = createRow(DEFAULT_LINE_CAPACITY);
	bool errorOccurred = false;
	int valueStartIndex = 0;
	for (int i=0; i<bytes.length; i++) {
		if (bytes.buffer[i] == 0xFF) {
			int length = i-valueStartIndex;
			if (length == 0) {
				if (appendToRow(currentRow, STRLIT("")) == false) {
					errorOccurred = true;
					break;
				}
			}
			else if (length == 1 && bytes.buffer[valueStartIndex] == 0xFE) {
				if (appendToRow(currentRow, NULL) == false) {
					errorOccurred = true;
					break;
				}
			}
			else {
				String* value = copyString((char*)&bytes.buffer[valueStartIndex], length);
				if (isValidUtf8(value) == false) {
					errorOccurred = true;
					break;
				}
				if (appendToRow(currentRow, value) == false) {
					errorOccurred = true;
					break;
				}
			}
			valueStartIndex = i+1;
		} else if (bytes.buffer[i] == 0xFD) {
			if (i > 0 && valueStartIndex != i) {
				errorOccurred = true;
				break;
			}
			if (appendToRows(result, currentRow) == false) {
				errorOccurred = true;
				break;
			}
			currentRow = createRow(DEFAULT_LINE_CAPACITY);
			if (currentRow == NULL) {
				errorOccurred = true;
				break;
			}
			valueStartIndex = i+1;
		}
	}
	deleteRow(currentRow);
	if (errorOccurred) {
		deleteRows(result);
		return NULL;
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

bool isValidRsv(Bytes bytes) {
	uint8_t lastState = 1;
	for (int i=0; i<bytes.length; i++) {
		uint8_t currentByte = bytes.buffer[i];
		uint8_t currentByteClass = rsvByteClassLookup[currentByte];
		uint8_t newStateLookupIndex = lastState*15+currentByteClass;
		lastState = rsvStateTransitionLookup[newStateLookupIndex];
		if (lastState == 0) { return false; }
	}
	return (lastState == 1);
}

// ----------------------------------------------------------------------

long getFileSize(FILE* file) {
	if (fseek(file, 0, SEEK_END) == -1) { return -1; }
	long size = ftell(file);
	rewind(file);
	return size;
}

bool saveFile(Bytes* bytes, char* filePath) {
	FILE* file = fopen(filePath, "wb");
	if (file == NULL) { return false; }
	if (fwrite(bytes->buffer, bytes->length, 1, file) != 1) {
		fclose(file);
		return false;
	}
	return fclose(file) == 0;
}

bool loadFile(char* filePath, Bytes* output) {
	FILE* file = fopen(filePath, "rb");
	if (file == NULL) { return false; }
	long size = getFileSize(file);
	if (size < 0) {
		fclose(file);
		return false;
	}
	unsigned char* bytes = (unsigned char*)malloc(size * sizeof(unsigned char));
	if (bytes == NULL) { return false; }
	if (size > 0) {
		if (fread(bytes, size, 1, file) != 1) {
			free(bytes);
			fclose(file);
			return false;
		}
	}
	if (fclose(file) != 0) {
		free(bytes);
		return false;
	}
	output->buffer = bytes;
	output->length = size;
	return true;
}

// ----------------------------------------------------------------------

bool saveRsv(Rows* rows, char* filePath) {
	Bytes* bytes = encodeRsv(rows);
	if (bytes == NULL) { return false; }
	bool result = saveFile(bytes, filePath);
	deleteBytes(bytes);
	return result;
}

Rows* loadRsv(char* filePath) {
	Bytes bytes;
	if (loadFile(filePath, &bytes) != true) { return NULL; }
	Rows* result = decodeRsv(bytes);
	free(bytes.buffer);
	return result;
}

bool appendRsv(Rows* rows, char* filePath, bool continueLastRow) {
	FILE* file = fopen(filePath, "rb+");
	if (file == NULL) {
		file = fopen(filePath, "wb+");
		if (file == NULL) { return false; }
	}
	fseek(file, 0, SEEK_END);
	long fileSize = ftell(file);
	if (continueLastRow && fileSize > 0) {
		fseek(file, fileSize - 1, SEEK_SET);
		uint8_t lastByte;
		fread(&lastByte, 1, 1, file);
		if (lastByte != 0xFD) {
			fclose(file);
			return false;
		}
		if (rows->length == 0) {
			return fclose(file) == 0;
		}
		fseek(file, fileSize - 1, SEEK_SET);
	}
	Bytes* bytes = encodeRsv(rows);
	if (bytes == NULL) {
		fclose(file);
		return false;
	}
	if (fwrite(bytes->buffer, bytes->length, 1, file) != 1) {
		deleteBytes(bytes);
		fclose(file);
		return false;
	}
	deleteBytes(bytes);
	return fclose(file) == 0;
}

// ----------------------------------------------------------------------

int escapeJsonString(String* str, String* output, int offset) {
	int j = offset;
	output->buffer[j] = '\"';
	j++;
	for (int i=0; i<str->length; i++) {
		uint8_t b = str->buffer[i];
		if (b == 0x08) { output->buffer[j] = '\\'; output->buffer[j+1] = 'b'; j += 2; }
		else if (b == 0x09) { output->buffer[j] = '\\'; output->buffer[j+1] = 't'; j += 2; }
		else if (b == 0x0A) { output->buffer[j] = '\\'; output->buffer[j+1] = 'n'; j += 2; }
		else if (b == 0x0C) { output->buffer[j] = '\\'; output->buffer[j+1] = 'f'; j += 2; }
		else if (b == 0x0D) { output->buffer[j] = '\\'; output->buffer[j+1] = 'r'; j += 2; }
		else if (b == 0x22) { output->buffer[j] = '\\'; output->buffer[j+1] = '\"'; j += 2; }
		else if (b == 0x5C) { output->buffer[j] = '\\'; output->buffer[j+1] = '\\'; j += 2; }
		else if (b >= 0x00 && b <= 0x1F) { sprintf((char*)&(output->buffer[j]), "\\u%04x", b); j += 6; }
		else { output->buffer[j] = b; j++; }
	}
	output->buffer[j] = '\"';
	j++;
	return j;
}

String* rsvToJsonString(Rows* rows) {
	size_t resultLength = 1;
	for (int i=0; i<rows->length; i++) {
		if (i != 0) { resultLength++; }
		resultLength += 4;
		Row* row = rows->rows[i];
		for (int j=0; j<row->length; j++) {
			if (j != 0) { resultLength += 2; }
			String* value = row->values[j];
			if (value == NULL) {
				resultLength += 4;
			} else {
				resultLength += 2;
				for (int i=0; i<value->length; i++) {
					uint8_t b = value->buffer[i];
					if (b == 0x08 || b == 0x09 || b == 0x0A || b == 0x0C || b == 0x0D || b == 0x22 || b == 0x5C) {
						resultLength += 2;
					} else if (b >= 0x00 && b <= 0x1F) {
						resultLength += 6;
					} else {
						resultLength++;
					}
				}
			}
		}
		resultLength++;
	}
	resultLength += 2;
	String* result = createString(resultLength);
	
	int k=0;
	result->buffer[k] = '[';
	k++;
	for (int i=0; i<rows->length; i++) {
		if (i != 0) { result->buffer[k] = ','; k++; }
		result->buffer[k] = '\n'; result->buffer[k+1] = ' '; result->buffer[k+2] = ' '; result->buffer[k+3] = '[';
		k += 4;
		Row* row = rows->rows[i];
		for (int j=0; j<row->length; j++) {
			if (j != 0) { result->buffer[k] = ','; result->buffer[k+1] = ' '; k += 2; }
			String* value = row->values[j];
			if (value == NULL) {
				result->buffer[k] = 'n'; result->buffer[k+1] = 'u'; result->buffer[k+2] = 'l'; result->buffer[k+3] = 'l';
				k += 4;
			} else {
				k = escapeJsonString(value, result, k);
			}
		}
		result->buffer[k] = ']'; k++;
	}
	result->buffer[k] = '\n'; result->buffer[k+1] = ']';
	return result;
}

void printString(String* str) {
	printf("%.*s", str->length, str->buffer);
}

void printRsvToJson(Rows* rows) {
	String* str = rsvToJsonString(rows);
	printString(str);
	fflush(NULL);
	deleteString(str);
	printf("\n");
}

// ----------------------------------------------------------------------

String* loadTextFile(char* filePath) {
	Bytes bytes;
	if (loadFile(filePath, &bytes) != true) { return NULL; }
	String* result = copyString(bytes.buffer, bytes.length);
	free(bytes.buffer);
	return result;
}

bool compareStrings(String* str1, String* str2) {
	if (str1 == NULL || str2 == NULL) { return str1 == str2; }
	if (str1->length != str2->length) { return false; }
	return memcmp(str1->buffer, str2->buffer, str1->length) == 0;
}

// ----------------------------------------------------------------------

bool checkTestFiles() {
	for (int i=1; i<=79; i++) {
		char filePath[256];
		sprintf(filePath, ".\\..\\TestFiles\\Valid_%03d.rsv", i);
		printf("Checking valid test file: %s\n", filePath);
		fflush(NULL);
		
		Rows* loadedRows = loadRsv(filePath);
		if (loadedRows == NULL) { return false; }
		String* jsonStr = rsvToJsonString(loadedRows);
		
		char filePathJson[256];
		sprintf(filePathJson, ".\\..\\TestFiles\\Valid_%03d.json", i);
		String* loadedJsonString = loadTextFile(filePathJson);
		if (loadedJsonString == NULL) { return false; }
		
		if (!compareStrings(jsonStr, loadedJsonString)) {
			printf("JSON mismatch");
			return false;
		}
		
		Bytes bytes;
		if (loadFile(filePath, &bytes) != true) { return NULL; }
		if (!isValidRsv(bytes)) {
			printf("Validation mismatch");
			return false;
		}
		free(bytes.buffer);
		
		deleteString(loadedJsonString);
		deleteString(jsonStr);
		deleteRows(loadedRows);
	}
	for (int i=1; i<=29; i++) {
		char filePath[256];
		sprintf(filePath, ".\\..\\TestFiles\\Invalid_%03d.rsv", i);
		printf("Checking invalid test file: %s\n", filePath);
		fflush(NULL);
		
		Rows* loadedRows = loadRsv(filePath);
		if (loadedRows != NULL) { return false; }
		
		Bytes bytes;
		if (loadFile(filePath, &bytes) != true) { return NULL; }
		if (isValidRsv(bytes)) {
			printf("Validation mismatch");
			return false;
		}
		free(bytes.buffer);
	}
	return true;
}

// ----------------------------------------------------------------------

int main() {
	Rows* rows = createRows(4);
	Row* row1 = createRow(4);
	appendToRow(row1, STRLIT("Hello"));
	appendToRow(row1, STRLIT("🌎"));
	appendToRow(row1, NULL);
	appendToRow(row1, STRLIT(""));
	appendToRows(rows, row1);
	Row* row2 = createRow(2);
	appendToRow(row2, STRLIT("A\0B\nC"));
	appendToRow(row2, STRLIT("Test 𝄞"));
	appendToRows(rows, row2);
	Row* row3 = createRow(0);
	appendToRows(rows, row3);
	Row* row4 = createRow(1);
	appendToRow(row4, STRLIT(""));
	appendToRows(rows, row4);
	
	printRsvToJson(rows);
	
	if (saveRsv(rows, "Test.rsv") == false) { return -1; }
	
	Rows* loadedRows = loadRsv("Test.rsv");
	if (loadedRows == NULL) { return -1; }
	printRsvToJson(loadedRows);

	if (saveRsv(loadedRows, "TestResaved.rsv") == false) { return -1; }

	Rows* appendRows = createRows(1);
	Row* appendRow1 = createRow(1);
	appendToRow(appendRow1, STRLIT("ABC"));
	appendToRows(appendRows, appendRow1);
	if (appendRsv(appendRows, "Append.rsv", false) == false) { return -1; }
	
	if (checkTestFiles() == false) { return -1; }
	
	printf("Done");
	return 0;
}