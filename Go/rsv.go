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

func EncodeRsv(rows [][]NullableString) ([]byte, error) {
	parts := [][]byte{}
	valueTerminatorByte := []byte{0xFF}
	nullValueByte := []byte{0xFE}
	rowTerminatorByte := []byte{0xFD}
	for _, row := range rows {
		for _, value := range row {
			if value.IsNull {
				parts = append(parts, nullValueByte)
			} else if len(value.Value) > 0 {
				if !utf8.ValidString(value.Value) {
					return nil, errors.New("Invalid string value")
				}
				parts = append(parts, []byte(value.Value))
			}
			parts = append(parts, valueTerminatorByte)
		}
		parts = append(parts, rowTerminatorByte)
	}
	return bytes.Join(parts, nil), nil
}

func DecodeRsv(bytes []byte) ([][]NullableString, error) {
	result := [][]NullableString{}
	currentRow := []NullableString{}
	if len(bytes) > 0 && bytes[len(bytes)-1] != 0xFD {
		return nil, errors.New("Incomplete RSV document")
	}
	valueStartIndex := 0
	for i := 0; i < len(bytes); i++ {
		if bytes[i] == 0xFF {
			length := i - valueStartIndex
			if length == 0 {
				currentRow = append(currentRow, Str(""))
			} else if length == 1 && bytes[valueStartIndex] == 0xFE {
				currentRow = append(currentRow, Null())
			} else {
				str := string(bytes[valueStartIndex : valueStartIndex+length])
				if !utf8.ValidString(str) {
					return nil, errors.New("Invalid string value")
				}
				currentRow = append(currentRow, Str(str))
			}
			valueStartIndex = i + 1
		} else if bytes[i] == 0xFD {
			if i > 0 && valueStartIndex != i {
				return nil, errors.New("Incomplete RSV row")
			}
			result = append(result, currentRow)
			currentRow = []NullableString{}
			valueStartIndex = i + 1
		}
	}
	return result, nil
}

// ----------------------------------------------------------------------

func SaveRsv(rows [][]NullableString, filePath string, permissions os.FileMode) error {
	bytes, err := EncodeRsv(rows)
	if err != nil {
		return err
	}
	return os.WriteFile(filePath, bytes, permissions)
}

func LoadRsv(filePath string) ([][]NullableString, error) {
	bytes, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	return DecodeRsv(bytes)
}

func AppendRsv(rows [][]NullableString, filePath string, permissions os.FileMode, continueLastRow bool) error {
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
			return errors.New("Incomplete RSV document")
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
	bytes, err := EncodeRsv(rows)
	if err != nil {
		return err
	}
	if _, err := file.Write(bytes); err != nil {
		return err
	}
	return nil
}

// ----------------------------------------------------------------------

func DecodeRsvUsingSplit(rsvBytes []byte) ([][]NullableString, error) {
	result := [][]NullableString{}
	if len(rsvBytes) == 0 {
		return result, nil
	}
	if rsvBytes[len(rsvBytes)-1] != 0xFD {
		return nil, errors.New("Incomplete RSV document")
	}
	bytesOfRows := bytes.Split(rsvBytes[0:len(rsvBytes)-1], []byte{0xFD})
	for _, rowBytes := range bytesOfRows {
		currentRow := []NullableString{}
		if len(rowBytes) > 0 {
			if rowBytes[len(rowBytes)-1] != 0xFF {
				return nil, errors.New("Incomplete RSV row")
			}
			bytesOfValues := bytes.Split(rowBytes[0:len(rowBytes)-1], []byte{0xFF})
			for _, valueBytes := range bytesOfValues {
				if len(valueBytes) == 1 && valueBytes[0] == 0xFE {
					currentRow = append(currentRow, Null())
				} else {
					strValue := string(valueBytes)
					if !utf8.ValidString(strValue) {
						return nil, errors.New("Invalid string value")
					}
					currentRow = append(currentRow, Str(strValue))
				}
			}
		}
		result = append(result, currentRow)
	}
	return result, nil
}

func LoadRsvUsingSplit(filePath string) ([][]NullableString, error) {
	bytes, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	return DecodeRsvUsingSplit(bytes)
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

func RsvToJson(rows [][]NullableString) string {
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

func PrintRsvToJson(rows [][]NullableString) {
	fmt.Println(RsvToJson(rows))
}

// ----------------------------------------------------------------------

func CheckTestFiles() {
	for i := 1; i <= 79; i++ {
		filePath := fmt.Sprintf("./../TestFiles/Valid_%03d", i)
		fmt.Println("Checking valid test file:", filePath)
		loadedRows, err := LoadRsv(filePath + ".rsv")
		if err != nil {
			panic("Could not load RSV file")
		}
		jsonStr := RsvToJson(loadedRows)

		loadedJsonBytes, err := os.ReadFile(filePath + ".json")
		if err != nil {
			panic("Could not load JSON file")
		}
		loadedJsonStr := string(loadedJsonBytes)
		if jsonStr != loadedJsonStr {
			panic("JSON mismatch")
		}

		loadedRowsUsingSplit, err := LoadRsvUsingSplit(filePath + ".rsv")
		if err != nil {
			panic("Could not load RSV file")
		}
		jsonStrUsingSplit := RsvToJson(loadedRowsUsingSplit)
		if jsonStrUsingSplit != loadedJsonStr {
			panic("JSON mismatch")
		}
	}

	for i := 1; i <= 22; i++ {
		filePath := fmt.Sprintf("./../TestFiles/Invalid_%03d", i)
		fmt.Println("Checking invalid test file:", filePath)
		_, err := LoadRsv(filePath + ".rsv")
		if err == nil {
			panic("RSV document is valid")
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
	PrintRsvToJson(rows)
	bytes, err := EncodeRsv(rows)
	if err != nil {
		panic(err)
	}
	fmt.Printf("% X\n", string(bytes))

	decodedRows, err := DecodeRsv(bytes)
	if err != nil {
		panic(err)
	}
	PrintRsvToJson(decodedRows)

	err = SaveRsv(rows, "Test.rsv", 0644)
	if err != nil {
		panic(err)
	}
	loadedRows, err := LoadRsv("Test.rsv")
	if err != nil {
		panic(err)
	}
	PrintRsvToJson(loadedRows)

	err = AppendRsv([][]NullableString{{Str("ABC")}}, "Append.rsv", 0644, false)
	if err != nil {
		panic(err)
	}

	CheckTestFiles()
}
