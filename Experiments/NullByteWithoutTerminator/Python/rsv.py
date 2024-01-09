# (C) Stefan John / Stenway / Stenway.com / 2023

import json
import os

def encode_rsv(rows: list[list[str | None]]) -> bytes:
	parts: list[bytes] = []
	for row in rows:
		for value in row:
			if value is None: parts.append(b"\xFE")
			else:
				if len(value) > 0: parts.append(value.encode())
				parts.append(b"\xFF")
		parts.append(b"\xFD")
	return b"".join(parts)

def decode_rsv(bytes: bytes) -> list[list[str | None]]:
	if len(bytes) > 0 and bytes[-1] != 0xFD:
		raise Exception("Incomplete RSV document")
	result: list[list[str | None]] = []
	current_row: list[str | None] = []
	value_start_index = 0
	for i in range(len(bytes)):
		if bytes[i] == 0xFF:
			length = i - value_start_index
			if length == 0:
				current_row.append("")
			else:
				value_bytes = bytes[value_start_index:i]
				current_row.append(value_bytes.decode())
			value_start_index = i + 1
		elif bytes[i] == 0xFE:	
			current_row.append(None)
			value_start_index = i + 1
		elif bytes[i] == 0xFD:
			if i > 0 and value_start_index != i:
				raise Exception("Incomplete RSV row")
			result.append(current_row)
			current_row = []
			value_start_index = i + 1
	return result

# ----------------------------------------------------------------------

def save_rsv(rows: list[list[str | None]], file_path: str):
	with open(file_path, "wb") as file:
		file.write(encode_rsv(rows))

def load_rsv(file_path: str) -> list[list[str | None]]:
	with open(file_path, "rb") as file:
		return decode_rsv(file.read())

def append_rsv(rows: list[list[str | None]], file_path: str, continue_last_row: bool=False):
	try:
		file = open(file_path, "rb+")
	except FileNotFoundError as e:
		file = open(file_path, "bx")
	try:	
		file.seek(0, os.SEEK_END)
		if continue_last_row and file.tell() > 0:
			file.seek(-1, os.SEEK_END)
			if file.read(1) != b'\xFD':
				raise Exception("Incomplete RSV document")
			if len(rows) == 0:
				return
			file.seek(-1, os.SEEK_END)
		file.write(encode_rsv(rows))
	finally: file.close()

# ----------------------------------------------------------------------

def is_valid_rsv(bytes: bytes):
	byte_class_lookup = [
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
	state_transition_lookup = [
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
		0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 10
	]
	last_state = 1
	for i in range(len(bytes)):
		current_byte = bytes[i]
		current_byte_class = byte_class_lookup[current_byte]
		new_state_lookup_index = last_state * 15 + current_byte_class
		last_state = state_transition_lookup[new_state_lookup_index]
		if last_state == 0:
			return False
	return last_state == 1

# ----------------------------------------------------------------------

def rsv_to_json(rows: list[list[str | None]]) -> str:
	result = "["
	if len(rows) > 0:
		result += "\n"
	result += ",\n".join(["  [" + ", ".join(["null" if x is None else json.dumps(x, ensure_ascii=False) for x in row]) + "]" for row in rows])
	result += "\n]"
	return result

# ----------------------------------------------------------------------

def load_file(file_path: str) -> bytes:
	with open(file_path, "rb") as file:
		return file.read()
	
def check_test_files():
	for i in range(1, 80):
		file_path = ".\\..\\TestFiles\\Valid_" + str(i).zfill(3)
		print("Checking valid test file: " + file_path)
		loaded_rows = load_rsv(file_path + ".rsv")
		json_str = rsv_to_json(loaded_rows)
		loaded_json_str = load_file(file_path + ".json").decode()
		if json_str != loaded_json_str:
			raise Exception("JSON mismatch")
		
		if not is_valid_rsv(load_file(file_path + ".rsv")):
			raise Exception("Validation mismatch")
	
	for i in range(1, 29):
		file_path = ".\\..\\TestFiles\\Invalid_" + str(i).zfill(3)
		print("Checking invalid test file: " + file_path)
		was_error = False
		try:
			loaded_rows = load_rsv(file_path + ".rsv")
		except Exception as e:
			was_error = True
		if not was_error:
			raise Exception("RSV document is valid")
		if is_valid_rsv(load_file(file_path + ".rsv")):
			raise Exception("Validation mismatch")

# ----------------------------------------------------------------------

def main():
	print("------------")
	rows = [["Hello", "🌎", None, ""], ["A\0B\nC", "Test 𝄞"], [], [""]]
	print(rsv_to_json(rows))
	
	encoded_bytes = encode_rsv(rows)
	decoded = decode_rsv(encoded_bytes)
	
	save_rsv(rows, "Test.rsv")
	loaded = load_rsv("Test.rsv")
	print(rsv_to_json(loaded))
	
	save_rsv(loaded, "TestResaved.rsv")
	
	#append_rsv([["ABC"]], "Append.rsv", True)
	
	check_test_files()
	
	print("Done")
	
main()