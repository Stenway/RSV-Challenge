/* (C) Stefan John / Stenway / Stenway.com / 2023 */

module main

import os
import strings
import encoding.utf8

// ----------------------------------------------------------------------

fn encode_rsv(rows [][]?string) ![]u8 {
	mut result := []u8{}
	for row in rows {
		for value in row {
			if str_value := value {
				if str_value.len > 0 {
					if !utf8.validate_str(str_value) { return error('Invalid string value') }
					result << str_value.bytes()
				}
			} else {
				result << 0xFE
			}
			result << 0xFF
		}
		result << 0xFD
	}
	return result
}

fn decode_rsv(bytes []u8) ![][]?string {
	if bytes.len > 0 && bytes[bytes.len-1] != 0xFD { return error('Incomplete RSV document') }
	mut result := [][]?string{}
	mut current_row := []?string{}
	mut value_start_index := 0
	for i := 0; i < bytes.len; i++ {
		if bytes[i] == 0xFF {
			length := i-value_start_index
			if length == 0 { current_row << '' }
			else if length == 1 && bytes[value_start_index] == 0xFE { current_row << none }
			else {
				value_bytes := bytes[value_start_index..value_start_index + length]
				str_value := value_bytes.bytestr()
				if !utf8.validate_str(str_value) { return error('Invalid string value') }
				current_row << str_value
			}
			value_start_index = i+1
		} else if bytes[i] == 0xFD {
			if i > 0 && value_start_index != i { return error('Incomplete RSV row') }
			result << current_row
			current_row = []?string{}
			value_start_index = i+1
		}
	}
	return result
}

// ----------------------------------------------------------------------

fn is_valid_rsv(bytes []u8) bool {
	byte_class_lookup := [
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
	state_transition_lookup := [
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
	
	mut last_state := 1
	for i := 0; i < bytes.len; i++ {
		current_byte := bytes[i]
		current_byte_class := byte_class_lookup[current_byte]
		new_state_lookup_index := last_state*15 + current_byte_class
		last_state = state_transition_lookup[new_state_lookup_index]
		if last_state == 0 {
			return false
		}
	}
	return last_state == 1
}

// ----------------------------------------------------------------------

fn save_rsv(rows [][]?string, file_path string) ! {
	bytes := encode_rsv(rows)!
	return os.write_file_array(file_path, bytes)
}

fn load_rsv(file_path string) ![][]?string {
	bytes := os.read_bytes(file_path)!
	return decode_rsv(bytes)
}

fn append_rsv(rows [][]?string, file_path string, continue_last_row bool) ! {
	bytes := encode_rsv(rows)!
	mut file := os.open_file(file_path, 'rb+') or {
		os.open_file(file_path, 'wb+')!
	}
	file.seek(0, .end)!
	file_size := file.tell()!
	if continue_last_row && file_size > 0 {
		last_byte := file.read_bytes_at(1, u64(file_size - 1))
		if last_byte[0] != 0xFD {
			file.close()
			return error('Incomplete RSV document')
		}
		if rows.len == 0 {
			file.close()
			return
		}
		file.seek(file_size - 1, .start)!
	}
	file.write(bytes)!
	file.close()
}

// ----------------------------------------------------------------------

fn escape_json_string(str string, mut builder strings.Builder) {
	builder.write_string('"')
	for i := 0; i<str.len; i++ {
		b := str[i]
		if b == 0x08 { builder.write_string('\\b') }
		else if b == 0x09 { builder.write_string('\\t') }
		else if b == 0x0A { builder.write_string('\\n') }
		else if b == 0x0C { builder.write_string('\\f') }
		else if b == 0x0D { builder.write_string('\\r') }
		else if b == 0x22 { builder.write_string('\\\"') }
		else if b == 0x5C { builder.write_string('\\\\') }
		else if b >= 0x00 && b <= 0x1F { builder.write_string('\\u${b:04x}') }
		else { builder.write_u8(b) }
	}
	builder.write_string('"')
}

fn rsv_to_json_string(rows [][]?string) string {
	mut builder := strings.new_builder(1)
	builder.write_string('[')
	mut is_first_row := true
	for row in rows {
		if !is_first_row { builder.write_string(',') }
		is_first_row = false
		builder.write_string('\n  [')
		mut is_first_value := true
		for value in row {
			if !is_first_value { builder.write_string(', ') }
			is_first_value = false
			if str_value := value {
				escape_json_string(str_value, mut builder)
			} else {
				builder.write_string('null')
			}
			
		}
		builder.write_string(']')
	}
	builder.write_string('\n]')
	return builder.str()
}

// ----------------------------------------------------------------------

fn load_text_file(file_path string) !string {
	bytes := os.read_bytes(file_path)!
	return bytes.bytestr()
}

fn check_test_files() ! {
	for i := 1; i<=79; i++ {
		file_path := './../TestFiles/Valid_${i:03}'
		println('Checking valid test file: ' + file_path)
		
		loaded_rows := load_rsv(file_path + '.rsv')!
		json_str := rsv_to_json_string(loaded_rows)
		
		loaded_json_str := load_text_file(file_path + '.json')!
		if json_str != loaded_json_str {
			panic('JSON mismatch')
		}
		
		bytes := os.read_bytes(file_path + '.rsv')!
		if !is_valid_rsv(bytes) {
			panic('Validation mismatch')
		}
	}
	
	for i := 1; i<=29; i++ {
		file_path := './../TestFiles/Invalid_${i:03}'
		println('Checking invalid test file: ' + file_path)
		
		mut was_error := false
		_ := load_rsv(file_path + '.rsv') or {
			was_error = true
			[][]?string{}
		}
		if !was_error {
			panic('RSV document is valid')
		}

		bytes := os.read_bytes(file_path + '.rsv')!
		if is_valid_rsv(bytes) {
			panic('Validation mismatch')
		}
	}
}

// ----------------------------------------------------------------------

fn main() {
	mut rows := [][]?string{}
	mut row1 := []?string{}
	row1 << 'Hello'
	row1 << '🌎'
	row1 << none
	row1 << ''
	rows << row1
	mut row2 := []?string{}
	row2 << 'A\x00B\nC'
	row2 << 'Test 𝄞'
	rows << row2
	mut row3 := []?string{}
	rows << row3
	mut row4 := []?string{}
	row4 << ''
	rows << row4
	
	println(rsv_to_json_string(rows))
	
	/*bytes := encode_rsv(rows)!
	decoded_rows := decode_rsv(bytes)!
	println(rsv_to_json_string(decoded_rows))*/
	
	save_rsv(rows, 'Test.rsv')!
	loaded_rows := load_rsv('Test.rsv')!
	println(rsv_to_json_string(loaded_rows))
	save_rsv(loaded_rows, 'TestResaved.rsv')!
	
	mut append_rows := [][]?string{}
	mut append_row1 := []?string{}
	append_row1 << 'ABC'
	append_rows << append_row1
	append_rsv(append_rows, 'Append.rsv', true)!
	
	check_test_files()!
	
	println('Done')
}