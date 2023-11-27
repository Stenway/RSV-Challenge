/* (C) Stefan John / Stenway / Stenway.com / 2023 */

package main

import "core:fmt"
import "core:bytes"
import "core:strings"
import "core:unicode/utf8"
import "core:os"

RsvError :: enum {
	None,
	InvalidString,
	IncompleteRsvDocument,
	IncompleteRsvRow
}

encode_rsv :: proc(rows: [dynamic][dynamic]Maybe(string)) -> ([]u8, RsvError) {
	parts := [dynamic]string{}
	for row in rows {
		for value in row {
			if value == nil { append(&parts, "\xFE") }
			else if len(value.?) > 0 {
				if !utf8.valid_string(value.?) { return []u8{}, .InvalidString }
				append(&parts, value.?)
			}
			append(&parts, "\xFF")
		}
		append(&parts, "\xFD")
	}
	return transmute([]u8)strings.join(parts[:], ""), .None
}

decode_rsv :: proc(bytes: []u8) -> ([dynamic][dynamic]Maybe(string), RsvError) {
	if len(bytes) > 0 && bytes[len(bytes)-1] != 0xFD { return [dynamic][dynamic]Maybe(string){}, .IncompleteRsvDocument }
	result := [dynamic][dynamic]Maybe(string){}
	current_row := [dynamic]Maybe(string){}
	value_start_index := 0
	for i := 0; i<len(bytes); i += 1 {
		if bytes[i] == 0xFF {
			length := i-value_start_index
			if length == 0 { append(&current_row, "") }
			else if length == 1 && bytes[value_start_index] == 0xFE { append(&current_row, nil) }
			else {
				value_bytes := bytes[value_start_index : value_start_index + length]
				str := string(value_bytes)
				if !utf8.valid_string(str) { return [dynamic][dynamic]Maybe(string){}, .InvalidString }
				append(&current_row, strings.clone(string(str)))
			}
			value_start_index = i + 1
		} else if bytes[i] == 0xFD {
			if i > 0 && value_start_index != i { return [dynamic][dynamic]Maybe(string){}, .IncompleteRsvRow }
			append(&result, current_row)
			current_row = [dynamic]Maybe(string){}
			value_start_index = i + 1
		}
	}
	return result, .None
}

// ----------------------------------------------------------------------

isValidRsv :: proc(bytes: []u8) -> bool {
	rsv_byte_class_lookup := []u8{
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
	}
	rsv_state_transition_lookup := []u8{
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
	}
	last_state: u8 = 1
	for i := 0; i<len(bytes); i += 1 {
		current_byte := bytes[i]
		current_byte_class := rsv_byte_class_lookup[current_byte]
		new_state_lookup_index := last_state * 15 + current_byte_class
		last_state = rsv_state_transition_lookup[new_state_lookup_index]
		if last_state == 0 {
			return false
		}
	}
	return last_state == 1
}

// ----------------------------------------------------------------------

save_rsv :: proc(rows: [dynamic][dynamic]Maybe(string), file_path: string) -> bool {
	bytes, error := encode_rsv(rows)
	defer delete(bytes, context.allocator)
	if error != nil { return false }
	success := os.write_entire_file(file_path, bytes)
	return success
}

load_rsv :: proc(file_path: string) -> ([dynamic][dynamic]Maybe(string), bool) {
	bytes, success := os.read_entire_file_from_filename(file_path)
	defer delete(bytes, context.allocator)
	if !success {
		return [dynamic][dynamic]Maybe(string){}, false
	}
	rows, error := decode_rsv(bytes)
	if error != nil { return [dynamic][dynamic]Maybe(string){}, false }
	return rows, true
}

append_rsv :: proc(rows: [dynamic][dynamic]Maybe(string), file_path: string, continue_last_row: bool) -> bool {
	bytes, error := encode_rsv(rows)
	defer delete(bytes, context.allocator)
	if error != nil { return false }
	
	file, ferror := os.open(file_path, os.O_RDWR | os.O_CREATE)
	if ferror > 0 { return false }
	file_size, ferror2 := os.file_size(file)
	if ferror2 > 0 { return false }
	if continue_last_row && file_size > 0 {
		os.seek(file, file_size - 1, 0)
		buffer := []u8{0}
		os.read(file, buffer)
		if buffer[0] != 0xFD {
			return false
		}
		os.seek(file, file_size - 1, 0)
	} else {
		os.seek(file, file_size, 0)
	}
	os.write(file, bytes)
	os.close(file)
	return true
}

// ----------------------------------------------------------------------

escape_json_string :: proc(str: string) -> string {
	builder := [dynamic]string{}
	append(&builder, "\"")
	for ch in str {
		c := int(ch)
		if c == 0x08 { append(&builder, "\\b") }
		else if c == 0x09 { append(&builder, "\\t") }
		else if c == 0x0A { append(&builder, "\\n") }
		else if c == 0x0C { append(&builder, "\\f") }
		else if c == 0x0D { append(&builder, "\\r") }
		else if c == 0x22 { append(&builder, "\\\"") }
		else if c == 0x5C { append(&builder, "\\\\") }
		else if c >= 0x00 && c <= 0x1F { append(&builder, fmt.aprintf("\\u%04x", c)) }
		else { append(&builder, fmt.aprintf("%c", ch)) }
	}
	append(&builder, "\"")
	return strings.join(builder[:], "")
}

rsv_to_json_string :: proc(rows: [dynamic][dynamic]Maybe(string)) -> string {
	builder := [dynamic]string{}
	append(&builder, "[")
	is_first_row := true
	for row in rows {
		if !is_first_row { append(&builder, ",") }
		is_first_row = false
		append(&builder, "\n  [")
		is_first_value := true
		for value in row {
			if !is_first_value { append(&builder, ", ") }
			is_first_value = false
			if value == nil { append(&builder, "null") }
			else {
				append(&builder, escape_json_string(value.?))
			}
		}
		append(&builder, "]")
	}
	append(&builder, "\n]")
	return strings.join(builder[:], "")
}

// ----------------------------------------------------------------------

check_test_files :: proc() {
	for i := 1; i<=79; i += 1 {
		file_path_rsv := fmt.tprintf("./../TestFiles/Valid_%3d.rsv", i)
		fmt.println("Checking valid test file:", file_path_rsv)
		
		loaded_rows, success := load_rsv(file_path_rsv)
		if success == false {
			panic("Could not load")
		}
		json_str := rsv_to_json_string(loaded_rows)
		
		file_path_json := fmt.tprintf("./../TestFiles/Valid_%3d.json", i)
		loaded_json_bytes, _ := os.read_entire_file_from_filename(file_path_json)
		loaded_json_str := string(loaded_json_bytes)
		if strings.compare(json_str, loaded_json_str) != 0 {
			panic("JSON mismatch")
		}
		
		bytes, _ := os.read_entire_file_from_filename(file_path_rsv)
		defer delete(bytes, context.allocator)
		
		if !isValidRsv(bytes) {
			panic("Validation mismatch")
		}
	}
	
	for i := 1; i<=29; i += 1 {
		file_path_rsv := fmt.tprintf("./../TestFiles/Invalid_%3d.rsv", i)
		fmt.println("Checking invalid test file:", file_path_rsv)
		
		loaded_rows, success := load_rsv(file_path_rsv)
		if success == true {
			panic("RSV document is valid")
		}
		
		bytes, _ := os.read_entire_file_from_filename(file_path_rsv)
		defer delete(bytes, context.allocator)
		
		if isValidRsv(bytes) {
			panic("Validation mismatch")
		}
	}
}

// ----------------------------------------------------------------------

main :: proc() {
	rows := [dynamic][dynamic]Maybe(string){
		[dynamic]Maybe(string){"Hello", "🌎", nil, ""},
		[dynamic]Maybe(string){"A\x00B\nC", "Test 𝄞"},
		[dynamic]Maybe(string){},
		[dynamic]Maybe(string){""},
	}
	fmt.println(rsv_to_json_string(rows))
	
	//bytes, _ := encode_rsv(rows)
	//fmt.println(bytes)
	
	//decoded_rows, _ := decode_rsv(bytes)
	//fmt.println(decoded_rows)
	
	save_rsv(rows, "Test.rsv")
	loaded_rows, _ := load_rsv("Test.rsv")
	fmt.println(rsv_to_json_string(loaded_rows))
	
	save_rsv(loaded_rows, "TestResaved.rsv")
	
	append_rows := [dynamic][dynamic]Maybe(string){
		[dynamic]Maybe(string){"ABC"},
	}
	append_rsv(append_rows, "Append.rsv", false)
	
	check_test_files()
	
	fmt.println("Done")
}