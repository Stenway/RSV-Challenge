# (C) Stefan John / Stenway / Stenway.com / 2023

const utf8ByteClassLookup = [
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
const utf8StateTransitionLookup = [
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

function isValidUtf8(bytes::Vector{UInt8})::Bool
	lastState = 1
	for i=1:length(bytes)
		currentByte = UInt8(bytes[i])
		currentByteClass = utf8ByteClassLookup[Int(currentByte) + 1]
		newStateLookupIndex = lastState * 12 + currentByteClass
		lastState = utf8StateTransitionLookup[newStateLookupIndex + 1]
		if lastState == 0
			return false
		end
	end
	return lastState == 1
end

# ----------------------------------------------------------------------

function encodeRsv(rows::Vector{Vector{Union{String, Nothing}}})::Vector{UInt8}
	bytes = Vector{UInt8}()
	for row in rows
		for value in row
			if value === nothing
				push!(bytes, 0xFE)
			elseif length(value) > 0
				valueBytes = Vector{UInt8}(value)
				if !isValidUtf8(valueBytes)
					throw(DomainError("Invalid string value"))
				end
				append!(bytes, valueBytes)
			end
			push!(bytes, 0xFF)
		end
		push!(bytes, 0xFD)
	end
	return bytes
end

function decodeRsv(bytes::Vector{UInt8})::Vector{Vector{Union{String, Nothing}}}
	if !isempty(bytes) && bytes[end] != 0xFD
		throw(DomainError("Incomplete RSV document"))
	end
	result = Vector{Vector{Union{String, Nothing}}}()
	currentRow = Vector{Union{String, Nothing}}()
	valueStartIndex = 1
	for i in eachindex(bytes)
		if bytes[i] == 0xFF
			length = i-valueStartIndex
			if length == 0
				push!(currentRow, "")
			elseif length == 1 && bytes[valueStartIndex] == 0xFE
				push!(currentRow, nothing)
			else
				valueBytes = bytes[valueStartIndex:valueStartIndex+length-1]
				if !isValidUtf8(valueBytes)
					throw(DomainError("Invalid string value"))
				end
				push!(currentRow, String(valueBytes))
			end
			valueStartIndex = i+1
		elseif bytes[i] == 0xFD
			if i > 0 && valueStartIndex != i
				throw(DomainError("Incomplete RSV row"))
			end
			push!(result, copy(currentRow))
			empty!(currentRow)
			valueStartIndex = i+1
		end
	end
	return result
end

# ----------------------------------------------------------------------

const rsvByteClassLookup = [
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
const rsvStateTransitionLookup = [
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

function isValidRsv(bytes::Vector{UInt8})::Bool
	lastState = 1
	for i=1:length(bytes)
		currentByte = UInt8(bytes[i])
		currentByteClass = rsvByteClassLookup[Int(currentByte) + 1]
		newStateLookupIndex = lastState * 15 + currentByteClass
		lastState = rsvStateTransitionLookup[newStateLookupIndex + 1]
		if lastState == 0
			return false
		end
	end
	return lastState == 1
end

# ----------------------------------------------------------------------

function saveRsv(rows, filePath::String)
	encodedRows = encodeRsv(rows)
	open(filePath, "w") do file
		write(file, encodedRows)
	end
end

function loadRsv(filePath::String)::Vector{Vector{Union{String, Nothing}}}
	bytes = read(filePath)
	return decodeRsv(bytes)
end

function openFileToAppend(filePath::String)
	try
		return open(filePath, "r+")
	catch e
		return open(filePath, "w+")
	end
end

function appendRsv(rows::Vector{Vector{Union{String, Nothing}}}, filePath::String, continueLastRow::Bool)
	file = openFileToAppend(filePath)
	fileSize = stat(filePath).size
	if continueLastRow && fileSize > 0
		seek(file, fileSize - 1)
		lastByte = UInt8[1]
		readbytes!(file, lastByte, 1)
		if lastByte[1] != 0xFD
			close(file)
			throw(DomainError("Incomplete RSV document"))
		end
		if length(rows) == 0
			close(file)
			return
		end
		seek(file, fileSize - 1)
	else
		seek(file, fileSize)
	end
	bytes = encodeRsv(rows)
	write(file, bytes)
	close(file)
end

# ----------------------------------------------------------------------

function escapeJsonString(str::String)::String
	result = IOBuffer()
	print(result, "\"")
	for ch in str
		c = Int(ch)
		if c == 0x08
			print(result, "\\b")
		elseif c == 0x09
			print(result, "\\t")
		elseif c == 0x0A
			print(result, "\\n")
		elseif c == 0x0C
			print(result, "\\f")
		elseif c == 0x0D
			print(result, "\\r")
		elseif c == 0x22
			print(result, "\\\"")
		elseif c == 0x5C
			print(result, "\\\\")
		elseif c >= 0x00 && c <= 0x1F
			print(result, "\\u00" * lpad(string(c, base = 16), 2, '0'))
		else
			print(result, ch)
		end
	end
	print(result, "\"")
	return String(take!(result))
end

function rsvToJson(rows::Vector{Vector{Union{String, Nothing}}})::String
	result = IOBuffer()
	print(result, "[")
	isFirstRow = true
	for row in rows
		if !isFirstRow
			print(result, ",")
		end
		isFirstRow = false
		print(result, "\n  [")
		isFirstValue = true
		for value in row
			if !isFirstValue
				print(result, ", ")
			end
			isFirstValue = false
			if value === nothing
				print(result, "null")
			else
				print(result, escapeJsonString(value))
			end
		end
		print(result, "]")
	end
	print(result, "\n]")
	return String(take!(result))
end

# ----------------------------------------------------------------------

function checkTestFiles()
	for i in 1:79
		filePath = "./../TestFiles/Valid_$(string(i, pad=3))"
		println("Checking valid test file: $(filePath)")
		loadedRows = loadRsv("$(filePath).rsv")
		jsonStr = rsvToJson(loadedRows)

		loadedJsonStr = read(filePath * ".json", String)
		if jsonStr != loadedJsonStr
			error("JSON mismatch")
		end

		if !isValidRsv(read(filePath * ".rsv"))
			error("Validation mismatch")
		end
	end

	for i in 1:29
		filePath = "./../TestFiles/Invalid_$(string(i, pad=3))"
		println("Checking invalid test file: $(filePath)")
		
		wasError = false
		try
			loadRsv("$(filePath).rsv")
		catch e
			wasError = true
		end
		
		if !wasError
			error("RSV document is valid")
		end
		
		if isValidRsv(read(string(filePath, ".rsv")))
			error("Validation mismatch")
		end
	end
end

# ----------------------------------------------------------------------

rows::Vector{Vector{Union{String, Nothing}}} = [
	["Hello", "🌎", nothing, ""],
	["A\0B\nC", "Test 𝄞"],
	[],
	[""]
]
println(rsvToJson(rows))
saveRsv(rows, "Test.rsv")

loadedRows = loadRsv("Test.rsv")
println(rsvToJson(loadedRows))
saveRsv(loadedRows, "TestResaved.rsv")

appendRows::Vector{Vector{Union{String, Nothing}}} = [["ABC"]]
appendRsv(appendRows, "Append.rsv", true)

checkTestFiles()

println("Done")