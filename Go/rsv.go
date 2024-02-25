﻿/* (C) Stefan John / Stenway / Stenway.com / 2023 */

package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"unicode/utf8"
)

type NullableString struct {
	Value  string
	IsNull bool
}

func Str(s string) NullableString { return NullableString{Value: s, IsNull: false} }
func Null() NullableString        { return NullableString{Value: "", IsNull: true} }

// ----------------------------------------------------------------------

const (
	valTerm = 0xFF // Mark the end of a value
	nullVal = 0xFE // Mark a null value
	rowTerm = 0xFD // Mark the end of a row
)

var (
	errInvalidString = errors.New("invalid UTF-8 string")
	errIncompleteDoc = errors.New("incomplete RSV document")
	errIncompleteRow = errors.New("incomplete RSV row")
)

func EncodeRsv(rows [][]NullableString) ([]byte, error) {
	var (
		vt = []byte{valTerm}
		nv = []byte{nullVal}
		rt = []byte{rowTerm}

		parts = [][]byte{}
	)
	for _, row := range rows {
		for _, x := range row {
			if x.IsNull {
				parts = append(parts, nv)
			} else if v := x.Value; len(v) > 0 {
				if !utf8.ValidString(v) {
					return nil, errInvalidString
				}
				parts = append(parts, []byte(v))
			}
			parts = append(parts, vt)
		}
		parts = append(parts, rt)
	}
	return bytes.Join(parts, nil), nil
}

func DecodeRsv(bytes []byte) ([][]NullableString, error) {
	if len(bytes) > 0 && bytes[len(bytes)-1] != 0xFD {
		return nil, errIncompleteDoc
	}
	result := [][]NullableString{}
	currentRow := []NullableString{}
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
					return nil, errInvalidString
				}
				currentRow = append(currentRow, Str(str))
			}
			valueStartIndex = i + 1
		} else if bytes[i] == 0xFD {
			if i > 0 && valueStartIndex != i {
				return nil, errIncompleteRow
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
			return errIncompleteDoc
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
		return nil, errIncompleteDoc
	}
	bytesOfRows := bytes.Split(rsvBytes[0:len(rsvBytes)-1], []byte{0xFD})
	for _, rowBytes := range bytesOfRows {
		currentRow := []NullableString{}
		if len(rowBytes) > 0 {
			if rowBytes[len(rowBytes)-1] != 0xFF {
				return nil, errIncompleteRow
			}
			bytesOfValues := bytes.Split(rowBytes[0:len(rowBytes)-1], []byte{0xFF})
			for _, valueBytes := range bytesOfValues {
				if len(valueBytes) == 1 && valueBytes[0] == 0xFE {
					currentRow = append(currentRow, Null())
				} else {
					strValue := string(valueBytes)
					if !utf8.ValidString(strValue) {
						return nil, errInvalidString
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

var byteClassLookup = [256]int{
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

var stateTransitionLookup = [180]int{
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
	0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11,
}

func IsValidRsv(bytes []byte) bool {
	lastState := 1
	for i := 0; i < len(bytes); i++ {
		currentByte := bytes[i]
		currentByteClass := byteClassLookup[currentByte]
		newStateLookupIndex := lastState*15 + currentByteClass
		lastState = stateTransitionLookup[newStateLookupIndex]
		if lastState == 0 {
			return false
		}
	}
	return lastState == 1
}

func IsValidRsvFile(filePath string) (bool, error) {
	bytes, err := os.ReadFile(filePath)
	if err != nil {
		return false, err
	}
	return IsValidRsv(bytes), nil
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
		} else if c <= 0x1F {
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
	thisDir, err := filepath.Abs(".")
	if err != nil {
		panic(fmt.Errorf("could not get abs path of \".\": %v", err))
	}
	testDir, err := filepath.Abs("./../TestFiles")
	if err != nil {
		panic(fmt.Errorf("could not get abs path to TestFile: %v", err))
	}

	for i := 1; i <= 79; i++ {
		fname := fmt.Sprintf("Valid_%03d", i)
		fpath := filepath.Join(testDir, fname)
		relPath, _ := filepath.Rel(thisDir, fpath)

		fmt.Println("Checking valid test file:  ", relPath)

		loadedRows, err := LoadRsv(fpath + ".rsv")
		if err != nil {
			panic("Could not load RSV file")
		}
		jsonStr := RsvToJson(loadedRows)

		loadedJsonBytes, err := os.ReadFile(fpath + ".json")
		if err != nil {
			panic("Could not load JSON file")
		}
		loadedJsonStr := string(loadedJsonBytes)
		if jsonStr != loadedJsonStr {
			panic("JSON mismatch")
		}

		loadedRowsUsingSplit, err := LoadRsvUsingSplit(fpath + ".rsv")
		if err != nil {
			panic("Could not load RSV file")
		}
		jsonStrUsingSplit := RsvToJson(loadedRowsUsingSplit)
		if jsonStrUsingSplit != loadedJsonStr {
			panic("JSON mismatch")
		}

		isValid, err := IsValidRsvFile(fpath + ".rsv")
		if err != nil {
			panic("Could not load RSV file")
		}
		if !isValid {
			panic("Validation mismatch")
		}
	}

	for i := 1; i <= 29; i++ {
		fname := fmt.Sprintf("Invalid_%03d", i)
		fpath := filepath.Join(testDir, fname)
		relPath, _ := filepath.Rel(thisDir, fpath)

		fmt.Println("Checking invalid test file:", relPath)

		_, err := LoadRsv(fpath + ".rsv")
		if err == nil {
			panic("RSV document is valid")
		}

		isValid, err := IsValidRsvFile(fpath + ".rsv")
		if err != nil {
			panic("Could not load RSV file")
		}
		if isValid {
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
