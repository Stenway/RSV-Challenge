# (C) Stefan John / Stenway / Stenway.com / 2023

$utf8_byte_class_lookup = [
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
]

$utf8_state_transition_lookup = [
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8,
	0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0
]

def is_valid_utf8(bytes)
	lastState = 1
	for i in 0..bytes.size-1 do
		currentByte = bytes[i]
		currentByteClass = $utf8_byte_class_lookup[currentByte]
		newStateLookupIndex = lastState * 12 + currentByteClass
		lastState = $utf8_state_transition_lookup[newStateLookupIndex]
		if lastState == 0
			return false
		end
	end
	return lastState == 1
end

# ----------------------------------------------------------------------

def encode_rsv(rows)
	result = ""
	rows.each do |row|
		row.each do |value|
			if value.nil?
				result << "\xFE"
			elsif value.length > 0
				if not is_valid_utf8(value.bytes)
					raise "Invalid string value"
				end
				result << value
			end
			result << "\xFF"
		end
		result << "\xFD"
	end
	return result
end

def decode_rsv(bytes)
	if bytes.size > 0 && bytes[-1] != 0xFD
		raise "Incomplete RSV document"
	end
	result = []
	current_row = []
	value_start_index = 0
	(0...bytes.size).each do |i|
		if bytes[i] == 0xFF
			length = i - value_start_index
			if length == 0
				current_row.push("")
			elsif length == 1 && bytes[value_start_index] == 0xFE
				current_row.push(nil)
			else
				value_bytes = bytes[value_start_index...value_start_index+length]
				if not is_valid_utf8(value_bytes)
					raise "Invalid string value"
				end
				str_value = value_bytes.pack('C*').force_encoding('UTF-8')
				current_row.push(str_value)
			end
			value_start_index = i + 1
		elsif bytes[i] == 0xFD
			if i > 0 && value_start_index != i
				raise "Incomplete RSV row"
			end
			result.push(current_row)
			current_row = []
			value_start_index = i + 1
		end
	end
	return result
end

# ----------------------------------------------------------------------

$rsv_byte_class_lookup = [
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
$rsv_state_transition_lookup = [
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

def is_valid_rsv(bytes)
	last_state = 1
	bytes.each do |current_byte|
		current_byte_class = $rsv_byte_class_lookup[current_byte]
		new_state_lookup_index = last_state * 15 + current_byte_class
		last_state = $rsv_state_transition_lookup[new_state_lookup_index]
		if last_state == 0
			return false
		end
	end
	last_state == 1
end

# ----------------------------------------------------------------------

def save_rsv(rows, file_path)
	bytes = encode_rsv(rows)
	bytes_str = File.binwrite(file_path, bytes)
end

def load_rsv(file_path)
	bytes_str = File.binread(file_path)
	return decode_rsv(bytes_str.bytes)
end

def append_rsv(rows, file_path, continue_last_row)
	file = File.open(file_path, "rb+")
	if file.nil?
		file = File.new(file_path, "wb+")
		if file.nil?
			raise "Opening file failed"
		end
	end
	file.seek(0, IO::SEEK_END)
	file_size = file.tell
	if continue_last_row && file_size > 0
		file.seek(file_size - 1, IO::SEEK_SET)
		last_byte = file.read(1).unpack("C").first
		if last_byte != 0xFD
			file.close
			raise "Incomplete RSV document"
		end
		if rows.empty?
			file.close
		end
		file.seek(file_size - 1, IO::SEEK_SET)
	end
	bytes = encode_rsv(rows)
	if file.write(bytes) != bytes.size
		file.close
		raise "Writing all bytes failed"
	end
	file.close
end
  
# ----------------------------------------------------------------------

def escape_json_string(str)
	result = "\""
	str.each_char do |char|
		c = char.ord
		if c == 0x08
			result += "\\b"
		elsif c == 0x09
			result += "\\t"
		elsif c == 0x0A
			result += "\\n"
		elsif c == 0x0C
			result += "\\f"
		elsif c == 0x0D
			result += "\\r"
		elsif c == 0x22
			result += "\\\""
		elsif c == 0x5C
			result += "\\\\"
		elsif c >= 0x00 && c <= 0x1F
			result += "\\u00" + c.to_s(16).rjust(2, '0').downcase()
		else
			result += char
		end
	end
	result += "\""
	return result
end

def rsv_to_json_string(rows)
	result = "["
	is_first_row = true
	rows.each do |row|
		if !is_first_row
			result << ","
		end
		is_first_row = false
		result << "\n  ["
		is_first_value = true
		row.each do |value|
			if !is_first_value
				result << ", "
			end
			is_first_value = false
			if value.nil?
				result << "null"
			else
				result << escape_json_string(value)
			end
		end
		result << "]"
	end
	result << "\n]" 
	return result
end

# ----------------------------------------------------------------------

def check_test_files()
	(1..79).each do |i|
		file_path = "./../TestFiles/Valid_#{format('%03d', i)}"
		puts("Checking valid test file: #{file_path}")
		
		loaded_rows = load_rsv("#{file_path}.rsv")
		json_str = rsv_to_json_string(loaded_rows)
	
		loaded_json_str = File.read("#{file_path}.json", encoding: "UTF-8")
		if json_str != loaded_json_str
			raise "JSON mismatch"
		end
		bytes = File.binread("#{file_path}.rsv").bytes
		if !is_valid_rsv(bytes)
			raise "Validation mismatch"
		end
		STDOUT.flush
	end
	
	(1..29).each do |i|
		file_path = "./../TestFiles/Invalid_#{format('%03d', i)}"
		puts("Checking invalid test file: #{file_path}")
		
		was_error = false
		begin
			loaded_rows = load_rsv("#{file_path}.rsv")
		rescue
			was_error = true
		end
		if !was_error
			raise "RSV document is valid"
		end
		bytes = File.binread("#{file_path}.rsv").bytes
		if is_valid_rsv(bytes)
			raise "Validation mismatch"
		end
		STDOUT.flush
	end
end
	
# ----------------------------------------------------------------------

rows = [
	["Hello", "🌎", nil, ""],
	["A\0B\nC", "Test 𝄞"],
	[],
	[""]
]

puts rsv_to_json_string(rows)
save_rsv(rows, "Test.rsv")

loaded_rows = load_rsv("Test.rsv")
puts rsv_to_json_string(loaded_rows)

save_rsv(loaded_rows, "TestResaved.rsv")

append_rsv([["ABC"]], "Append.rsv", true)

check_test_files()

puts "Done"