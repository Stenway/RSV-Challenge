-- (C) Stefan John / Stenway / Stenway.com / 2023

utf8ByteClassLookup = {
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
}

utf8StateTransitionLookup = {
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8,
	0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0
}

function isValidUtf8(str)
	local lastState = 1
	for i = 1, #str do
		local currentByte = string.byte(str, i)
		local currentByteClass = utf8ByteClassLookup[currentByte + 1]
		local newStateLookupIndex = lastState * 12 + currentByteClass
		lastState = utf8StateTransitionLookup[newStateLookupIndex + 1]
		if lastState == 0 then
			return false
		end
	end
	return lastState == 1
end

-- ----------------------------------------------------------------------

function encodeRsv(rows)
	local result = ""
	for _, row in ipairs(rows) do
		for _, value in ipairs(row) do
			if type(value) == "table" then
				assert(#value == 0)
				result = result .. "\xFE"
			elseif #value > 0 then
				if not isValidUtf8(value) then
					error("Invalid string value")
				end
				result = result .. value
			end
			result = result .. "\xFF"
		end
		result = result .. "\xFD"
	end
	return result
end

function decodeRsv(bytes)
	if #bytes > 0 and string.byte(bytes, #bytes) ~= 0xFD then
		error("Incomplete RSV document")
	end
	local result = {}
	local currentRow = {}
	local valueStartIndex = 1
	for i = 1, #bytes do
		if string.byte(bytes, i) == 0xFF then
			local valueBytesLength = i - valueStartIndex
			if valueBytesLength == 0 then
				table.insert(currentRow, "")
			elseif valueBytesLength == 1 and string.byte(bytes, valueStartIndex) == 0xFE then
				table.insert(currentRow, {})
			else
				local strValue = string.sub(bytes, valueStartIndex, valueStartIndex + valueBytesLength - 1)
				if not isValidUtf8(strValue) then
					error("Invalid string value")
				end
				table.insert(currentRow, strValue)
			end
			valueStartIndex = i + 1
		elseif string.byte(bytes, i) == 0xFD then
			if i > 1 and valueStartIndex ~= i then
				error("Incomplete RSV row")
			end
			table.insert(result, currentRow)
			currentRow = {}
			valueStartIndex = i + 1
		end
	end

	return result
end

-- ----------------------------------------------------------------------

rsvByteClassLookup = {
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

rsvStateTransitionLookup = {
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

function isValidRsv(bytes)
	local lastState = 1
	for i = 1, #bytes do
		local currentByte = string.byte(bytes, i)
		local currentByteClass = rsvByteClassLookup[currentByte + 1]
		local newStateLookupIndex = lastState * 15 + currentByteClass
		lastState = rsvStateTransitionLookup[newStateLookupIndex + 1]
		if lastState == 0 then
			return false
		end
	end
	return lastState == 1
end

-- ----------------------------------------------------------------------

function saveRsv(rows, filePath)
	local bytes = encodeRsv(rows)
	local file = assert(io.open(filePath, "wb"))
	file:write(bytes)
	assert(file:close())
end

function loadRsv(filePath)
	local file = assert(io.open(filePath, "rb"))
	local bytes = file:read("*all")
	assert(file:close())
	return decodeRsv(bytes)
end

function appendRsv(rows, filePath, continueLastRow)
	local file = io.open(filePath, "r+b")
	if file == nil then
		file = io.open(filePath, "w+b")
		if file == nil then
			error("Opening file failed")
		end
	end
	local fileSize = file:seek("end")
	if continueLastRow and fileSize > 0 then
		file:seek("set", fileSize - 1)
		local lastByte = file:read(1)
		if string.byte(lastByte) ~= 0xFD then
			file:close()
			error("Incomplete RSV document")
		end
		if #rows == 0 then
			file:close()
			return
		end
		file:seek("set", fileSize - 1)
	end
	local bytes = encodeRsv(rows)
	if file:write(bytes) == nil then
		file:close()
		error("Writing all bytes failed")
	end
	file:close()
end

-- ----------------------------------------------------------------------

function escapeJsonString(str)
	local result = "\""
	for i = 1, #str do
		local c = string.byte(str, i)
		if c == 0x08 then result = result .. "\\b"
		elseif c == 0x09 then result = result .. "\\t"
		elseif c == 0x0A then result = result .. "\\n"
		elseif c == 0x0C then result = result .. "\\f"
		elseif c == 0x0D then result = result .. "\\r"
		elseif c == 0x22 then result = result .. "\\\""
		elseif c == 0x5C then result = result .. "\\\\"
		elseif c >= 0x00 and c <= 0x1F then result = result .. "\\u00" .. string.format("%02x", c)
		else result = result .. string.char(c) end
	end
	result = result .. "\""
	return result
end

function rsvToJsonString(rows)
	local result = "["
	local isFirstRow = true
	for _, row in ipairs(rows) do
		if not isFirstRow then
			result = result .. ","
		end
		isFirstRow = false
		result = result .. "\n  ["
		local isFirstValue = true
		for _, value in ipairs(row) do
			if not isFirstValue then
				result = result .. ", "
			end
			isFirstValue = false
			if type(value) == "table" then
				assert(#value == 0)
				result = result .. "null"
			else
				result = result .. escapeJsonString(value)
			end
		end
		result = result .. "]"
	end
	result = result .. "\n]"
	return result
end

-- ----------------------------------------------------------------------

function saveFile(content, filePath)
	local file = assert(io.open(filePath, "wb"))
	file:write(content)
	assert(file:close())
end

function checkTestFiles()
	for i=1, 79 do
		local filePath = "./../TestFiles/Valid_" .. string.format("%03d", i)
		print("Checking valid test file: " .. filePath)
		local loadedRows = loadRsv(filePath .. ".rsv")
		local jsonStr = rsvToJsonString(loadedRows)

		local loadedJsonStr = io.open(filePath .. ".json", "r"):read("*a")
		if jsonStr ~= loadedJsonStr then
			error("JSON mismatch")
		end

		local bytes = io.open(filePath .. ".rsv", "rb"):read("*a")
		if not isValidRsv(bytes) then
			error("Validation mismatch")
		end
	end

	for i=1, 29 do
		local filePath = "./../TestFiles/Invalid_" .. string.format("%03d", i)
		print("Checking invalid test file: " .. filePath)
		local success, _ = pcall(loadRsv, filePath .. ".rsv")
		if success then
			error("RSV document is valid")
		end
		local bytes = io.open(filePath .. ".rsv", "rb"):read("*a")
		if isValidRsv(bytes) then
			error("Validation mismatch")
		end
	end
end

-- ----------------------------------------------------------------------

rows = {
	{"Hello", "🌎", {}, ""},
	{"A\0B\nC", "Test 𝄞"},
	{},
	{""}
}
print(rsvToJsonString(rows))
saveRsv(rows, "Test.rsv")

loadedRows = loadRsv("Test.rsv")
print(rsvToJsonString(loadedRows))

saveRsv(loadedRows, "TestResaved.rsv")

appendRsv({{"ABC"}}, "Append.rsv", false)

checkTestFiles()

print("Done")