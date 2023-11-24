import std/options
import strutils
import std/strformat

const utf8ByteClassLookup: array[256, byte] = [
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

const utf8StateTransitionLookup: array[108, byte] = [
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

proc isValidUtf8(str: string): bool =
    var lastState: byte = 1;
    for c in str:
        var currentByteClass = utf8ByteClassLookup[c.byte]
        var newStateLookupIndex = lastState*12+currentByteClass
        lastState = utf8StateTransitionLookup[newStateLookupIndex]
        if lastState == 0: return false
    return (lastState == 1)

# ----------------------------------------------------------------------

proc encodeRsv(rows: seq[seq[Option[string]]]): string =
    result = ""
    for row in rows:
        for value in row:
            if value.issome == false: result.add("\xFE")
            else:
                var strValue = value.get
                if strValue.len > 0:
                    if isValidUtf8(strValue) == false: raise newException(ValueError, "Invalid string value")
                    result.add(strValue)
            result.add("\xFF")
        result.add("\xFD")
    return result

proc decodeRsv(bytes: string): seq[seq[Option[string]]] =
    if bytes.len > 0 and bytes[bytes.len-1].byte != 0xFD: raise newException(ValueError, "Incomplete RSV document")
    result = @[]
    var currentRow: seq[Option[string]] = @[]
    var valueStartIndex = 0
    for i in 0..<bytes.len:
        if bytes[i].byte == 0xFF:
            var length = i-valueStartIndex
            if length == 0: currentRow.add(some(""))
            elif length == 1 and bytes[valueStartIndex].byte == 0xFE: currentRow.add(none(string))
            else:
                var valueBytes = bytes[valueStartIndex..valueStartIndex+length-1]
                if isValidUtf8(valueBytes) == false: raise newException(ValueError, "Invalid string value")
                currentRow.add(some(valueBytes))
            valueStartIndex = i+1
        elif bytes[i].byte == 0xFD:
            if i > 0 and valueStartIndex != i: raise newException(ValueError, "Incomplete RSV row")
            result.add(currentRow)
            currentRow = @[]
            valueStartIndex = i+1
    return result

# ----------------------------------------------------------------------

const rsvByteClassLookup: array[256, byte] = [
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

const rsvStateTransitionLookup: array[180, byte] = [
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

proc isValidRsv(str: string): bool =
    var lastState: byte = 1;
    for c in str:
        var currentByteClass = rsvByteClassLookup[c.byte]
        var newStateLookupIndex = lastState*15+currentByteClass
        lastState = rsvStateTransitionLookup[newStateLookupIndex]
        if lastState == 0: return false
    return (lastState == 1)

# ----------------------------------------------------------------------

proc saveRsv(rows: seq[seq[Option[string]]], filePath: string) =
    writeFile(filePath, encodeRsv(rows))

proc loadRsv(filePath: string): seq[seq[Option[string]]] =
    return decodeRsv(readFile(filePath))

proc appendRsv(rows: seq[seq[Option[string]]], filePath: string, continueLastRow: bool) =
    var file: File
    try:
        file = open(filePath, fmReadWriteExisting)
    except:
        file = open(filePath, fmReadWrite)
    var fileSize = getFileSize(file)
    if continueLastRow and fileSize > 0:
        setFilePos(file, fileSize-1)
        if readChar(file).byte != 0xFD: raise newException(ValueError, "Incomplete RSV document")
        if rows.len == 0:
            close(file)
            return
        setFilePos(file, fileSize-1)
    else:
        setFilePos(file, fileSize)
    var bytes = encodeRsv(rows)
    write(file, bytes)
    close(file)

# ----------------------------------------------------------------------

proc escapeJsonString(str: string): string =
    result = "\""
    for ch in str:
        var c = ch.byte
        if c == 0x08: result.add("\\b")
        elif c == 0x09: result.add("\\t")
        elif c == 0x0A: result.add("\\n")
        elif c == 0x0C: result.add("\\f")
        elif c == 0x0D: result.add("\\r")
        elif c == 0x22: result.add("\\\"")
        elif c == 0x5C: result.add("\\\\")
        elif c >= 0x00 and c <= 0x1F: result.add("\\u00" & toLowerAscii(toHex(c, 2)))
        else: result.add(ch)
    result.add("\"")
    return result

proc rsvToJson(rows: seq[seq[Option[string]]]): string =
    result = "["
    var isFirstRow = true
    for row in rows:
        if not isFirstRow: result.add(",")
        isFirstRow = false
        var isFirstValue = true
        result.add("\n  [")
        for value in row:
            if not isFirstValue: result.add(", ")
            isFirstValue = false
            if value.issome == false: result.add("null")
            else:
                result.add(escapeJsonString(value.get))
        result.add("]")
    result.add("\n]")
    return result

# ----------------------------------------------------------------------

proc checkTestFiles() =
    for i in 1..79:
        var filePath: string = "./../TestFiles/Valid_" & fmt"{i:03}"
        echo "Checking valid test file: " & filePath
        var loadedRows = loadRsv(filePath & ".rsv")
        var jsonStr = rsvToJson(loadedRows)
        var loadedJsonStr = readFile(filePath & ".json")
        if jsonStr != loadedJsonStr:
            raise newException(ValueError, "JSON mismatch")
        
        var bytes = readFile(filePath & ".rsv")
        if not isValidRsv(bytes):
            raise newException(ValueError, "Validation mismatch")
        
    for i in 1..29:
        var filePath: string = "./../TestFiles/Invalid_" & fmt"{i:03}"
        echo "Checking invalid test file: " & filePath
        var wasError = false
        try:
            var _ = loadRsv(filePath & ".rsv")
        except:
            wasError = true
        if not wasError:
            raise newException(ValueError, "RSV document is valid")
        
        var bytes = readFile(filePath & ".rsv")
        if isValidRsv(bytes):
            raise newException(ValueError, "Validation mismatch")

# ----------------------------------------------------------------------

var rows: seq[seq[Option[string]]] = @[
    @[some("Hello"), some("🌎"), none(string), some("")],
    @[some("A\0B\nC"), some("Test 𝄞")],
    @[],
    @[some("")],
]
echo rsvToJson(rows)

#var bytes = encodeRsv(rows)
#var decodedRows = decodeRsv(bytes)
#echo decodedRows

saveRsv(rows, "Test.rsv")

var loadedRows = loadRsv("Test.rsv")
echo rsvToJson(loadedRows)

saveRsv(loadedRows, "TestResaved.rsv")

var appendRows: seq[seq[Option[string]]] = @[@[some("Hello")]]
appendRsv(appendRows, "Append.rsv", false)

checkTestFiles()

echo "Done"