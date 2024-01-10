/* (C) Stefan John / Stenway / Stenway.com / 2023 */

package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode/utf8"
)

type NullableString struct {
	Value  string
	IsNull bool
}

func Str(str string) NullableString { return NullableString{Value: str, IsNull: false} }
func Null() NullableString          { return NullableString{IsNull: true} }

// ----------------------------------------------------------------------

func EncodeXRsv(rows [][]NullableString) ([]byte, error) {
	parts := [][]byte{}
	valueTerminatorByte := []byte{0xFF}
	nullValueByte := []byte{0xFE}
	rowTerminatorByte := []byte{0xFD}
	for _, row := range rows {
		for _, value := range row {
			if value.IsNull {
				parts = append(parts, nullValueByte)
			} else {
				if len(value.Value) > 0 {
					if !utf8.ValidString(value.Value) {
						return nil, errors.New("Invalid string value")
					}
					parts = append(parts, []byte(value.Value))
				}
				parts = append(parts, valueTerminatorByte)
			}
		}
		parts = append(parts, rowTerminatorByte)
	}
	return bytes.Join(parts, nil), nil
}

func DecodeXRsv(bytes []byte) ([][]NullableString, error) {
	if len(bytes) > 0 && bytes[len(bytes)-1] != 0xFD {
		return nil, errors.New("Incomplete XRSV document")
	}
	result := [][]NullableString{}
	currentRow := []NullableString{}
	valueStartIndex := 0
	for i := 0; i < len(bytes); i++ {
		if bytes[i] == 0xFF {
			length := i - valueStartIndex
			if length == 0 {
				currentRow = append(currentRow, Str(""))
			} else {
				str := string(bytes[valueStartIndex : valueStartIndex+length])
				if !utf8.ValidString(str) {
					return nil, errors.New("Invalid string value")
				}
				currentRow = append(currentRow, Str(str))
			}
			valueStartIndex = i + 1
		} else if bytes[i] == 0xFE {
			currentRow = append(currentRow, Null())
			valueStartIndex = i + 1
		} else if bytes[i] == 0xFD {
			if i > 0 && valueStartIndex != i {
				return nil, errors.New("Incomplete XRSV row")
			}
			result = append(result, currentRow)
			currentRow = []NullableString{}
			valueStartIndex = i + 1
		}
	}
	return result, nil
}

// ----------------------------------------------------------------------

func SaveXRsv(rows [][]NullableString, filePath string, permissions os.FileMode) error {
	bytes, err := EncodeXRsv(rows)
	if err != nil {
		return err
	}
	return os.WriteFile(filePath, bytes, permissions)
}

func LoadXRsv(filePath string) ([][]NullableString, error) {
	bytes, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	return DecodeXRsv(bytes)
}

func AppendXRsv(rows [][]NullableString, filePath string, permissions os.FileMode, continueLastRow bool) error {
	file, err := os.OpenFile(filePath, os.O_RDWR|os.O_CREATE, permissions)
	if err != nil {
		return err
	}
	defer file.Close()
	fileInfo, err := file.Stat()
	if err != nil {
		return err
	}
	if continueLastRow && fileInfo.Size() > 0 {
		if _, err := file.Seek(fileInfo.Size()-1, io.SeekStart); err != nil {
			return err
		}
		var b [1]byte
		if _, err := file.Read(b[:]); err != nil {
			return err
		}
		if b[0] != 0xFD {
			return errors.New("Incomplete XRSV document")
		}
		if len(rows) == 0 {
			return nil
		}
		if _, err := file.Seek(fileInfo.Size()-1, io.SeekStart); err != nil {
			return err
		}
	} else {
		if _, err := file.Seek(0, io.SeekEnd); err != nil {
			return err
		}
	}
	bytes, err := EncodeXRsv(rows)
	if err != nil {
		return err
	}
	if _, err := file.Write(bytes); err != nil {
		return err
	}
	return nil
}

// ----------------------------------------------------------------------

var byteClassLookup = []byte{
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
	9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 14,
}

var stateTransitionLookup = []byte{
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
	0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 10,
}

func IsValidXRsv(bytes []byte) bool {
	lastState := 1
	for i := 0; i < len(bytes); i++ {
		currentByte := bytes[i]
		currentByteClass := byteClassLookup[currentByte]
		newStateLookupIndex := lastState*15 + int(currentByteClass)
		lastState = int(stateTransitionLookup[newStateLookupIndex])
		if lastState == 0 {
			return false
		}
	}
	return lastState == 1
}

func IsValidXRsvFile(filePath string) (bool, error) {
	bytes, err := os.ReadFile(filePath)
	if err != nil {
		return false, err
	}
	return IsValidXRsv(bytes), nil
}

// ----------------------------------------------------------------------

func EscapeJsonString(str string) string {
	var result strings.Builder
	result.WriteString("\"")
	for i := 0; i < len(str); i++ {
		c := str[i]
		if c == 0x08 {
			result.WriteString("\\b")
		} else if c == 0x09 {
			result.WriteString("\\t")
		} else if c == 0x0A {
			result.WriteString("\\n")
		} else if c == 0x0C {
			result.WriteString("\\f")
		} else if c == 0x0D {
			result.WriteString("\\r")
		} else if c == 0x22 {
			result.WriteString("\\\"")
		} else if c == 0x5C {
			result.WriteString("\\\\")
		} else if c >= 0x00 && c <= 0x1F {
			result.WriteString(fmt.Sprintf("\\u%04x", int(c)))
		} else {
			result.WriteByte(c)
		}
	}
	result.WriteString("\"")
	return result.String()
}

func XRsvToJson(rows [][]NullableString) string {
	var builder strings.Builder
	builder.WriteString("[")
	isFirstRow := true
	for _, row := range rows {
		if !isFirstRow {
			builder.WriteString(",")
		}
		isFirstRow = false
		builder.WriteString("\n  [")
		isFirstValue := true
		for _, value := range row {
			if !isFirstValue {
				builder.WriteString(", ")
			}
			isFirstValue = false
			if value.IsNull {
				builder.WriteString("null")
			} else {
				builder.WriteString(EscapeJsonString(value.Value))
			}
		}
		builder.WriteString("]")
	}
	builder.WriteString("\n]")
	return builder.String()
}

func PrintXRsvToJson(rows [][]NullableString) {
	fmt.Println(XRsvToJson(rows))
}

// ----------------------------------------------------------------------

func CheckTestFiles() {
	for i := 1; i <= 79; i++ {
		filePath := fmt.Sprintf("./../TestFiles/Valid_%03d", i)
		fmt.Println("Checking valid test file:", filePath)
		loadedRows, err := LoadXRsv(filePath + ".xrsv")
		if err != nil {
			panic("Could not load XRSV file")
		}
		jsonStr := XRsvToJson(loadedRows)

		loadedJsonBytes, err := os.ReadFile(filePath + ".json")
		if err != nil {
			panic("Could not load JSON file")
		}
		loadedJsonStr := string(loadedJsonBytes)
		if jsonStr != loadedJsonStr {
			panic("JSON mismatch")
		}

		isValid, err := IsValidXRsvFile(filePath + ".xrsv")
		if err != nil {
			panic("Could not load XRSV file")
		}
		if isValid == false {
			panic("Validation mismatch")
		}
	}

	for i := 1; i <= 28; i++ {
		filePath := fmt.Sprintf("./../TestFiles/Invalid_%03d", i)
		fmt.Println("Checking invalid test file:", filePath)
		_, err := LoadXRsv(filePath + ".xrsv")
		if err == nil {
			panic("XRSV document is valid")
		}

		isValid, err := IsValidXRsvFile(filePath + ".xrsv")
		if err != nil {
			panic("Could not load XRSV file")
		}
		if isValid == true {
			panic("Validation mismatch")
		}
	}
}

// ----------------------------------------------------------------------

func main() {
	rows := [][]NullableString{
		{Str("Hello"), Str("🌎"), Null(), Str("")},
		{Str("A\x00B\nC"), Str("Test 𝄞")},
		{},
		{Str("")},
	}
	PrintXRsvToJson(rows)
	bytes, err := EncodeXRsv(rows)
	if err != nil {
		panic(err)
	}
	fmt.Printf("% X\n", string(bytes))

	decodedRows, err := DecodeXRsv(bytes)
	if err != nil {
		panic(err)
	}
	PrintXRsvToJson(decodedRows)

	err = SaveXRsv(rows, "Test.xrsv", 0644)
	if err != nil {
		panic(err)
	}
	loadedRows, err := LoadXRsv("Test.xrsv")
	if err != nil {
		panic(err)
	}
	PrintXRsvToJson(loadedRows)

	err = AppendXRsv([][]NullableString{{Str("ABC")}}, "Append.xrsv", 0644, false)
	if err != nil {
		panic(err)
	}

	CheckTestFiles()
}
