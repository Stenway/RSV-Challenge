/* (C) Stefan John / Stenway / Stenway.com / 2023 */

import java.io.IOException
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.util.*

fun encodeRsv(rows: Array<Array<String?>>): ByteArray {
	val parts = ArrayList<ByteArray>()
	val valueTerminatorByte = byteArrayOf(0xFF.toByte())
	val nullValueByte = byteArrayOf(0xFE.toByte())
	val rowTerminatorByte = byteArrayOf(0xFD.toByte())
	val encoder = StandardCharsets.UTF_8.newEncoder()
	var resultLength = 0
	for (row in rows) {
		for (value in row) {
			if (value == null) {
				parts.add(nullValueByte)
				resultLength++
			} else if (value.length > 0) {
				var valueBytes: ByteArray
				try {
					val byteBuffer = encoder.encode(CharBuffer.wrap(value))
					valueBytes = ByteArray(byteBuffer.limit())
					byteBuffer[valueBytes]
				} catch (e: Exception) {
					throw RuntimeException("Invalid string value", e)
				}
				parts.add(valueBytes)
				resultLength += valueBytes.size
			}
			parts.add(valueTerminatorByte)
			resultLength++
		}
		parts.add(rowTerminatorByte)
		resultLength++
	}
	val result = ByteArray(resultLength)
	val resultBuffer = ByteBuffer.wrap(result)
	for (part in parts) {
		resultBuffer.put(part)
	}
	return result
}

fun decodeRsv(bytes: ByteArray): Array<Array<String?>> {
	if (bytes.size > 0 && bytes[bytes.size - 1].toUByte() != 0xFD.toUByte()) {
		throw RuntimeException("Incomplete RSV document")
	}
	val decoder = StandardCharsets.UTF_8.newDecoder()
	decoder.onMalformedInput(CodingErrorAction.REPORT)
	decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
	val byteBuffer = ByteBuffer.wrap(bytes)
	val result = ArrayList<Array<String?>>()
	val currentRow = ArrayList<String?>()
	var valueStartIndex = 0
	for (i in bytes.indices) {
		val currentByte: UByte = bytes[i].toUByte()
		if (currentByte == 0xFF.toUByte()) {
			val length = i - valueStartIndex
			if (length == 0) {
				currentRow.add("")
			} else if (length == 1 && bytes[valueStartIndex].toUByte() == 0xFE.toUByte()) {
				currentRow.add(null)
			} else {
				val valueBytes = (byteBuffer.duplicate().position(valueStartIndex).limit(i) as ByteBuffer).slice()
				try {
					currentRow.add(decoder.decode(valueBytes).toString())
				} catch (e: Exception) {
					throw RuntimeException("Invalid string value", e)
				}
			}
			valueStartIndex = i + 1
		} else if (currentByte == 0xFD.toUByte()) {
			if (i > 0 && valueStartIndex != i) {
				throw RuntimeException("Incomplete RSV row")
			}
			result.add(currentRow.toTypedArray())
			currentRow.clear()
			valueStartIndex = i + 1
		}
	}
	return result.toTypedArray()
}

// ----------------------------------------------------------------------
@Throws(IOException::class)
fun saveRsv(rows: Array<Array<String?>>, filePath: String) {
	Objects.requireNonNull(filePath)
	Files.write(Paths.get(filePath), encodeRsv(rows))
}

@Throws(IOException::class)
fun loadRsv(filePath: String): Array<Array<String?>> {
	Objects.requireNonNull(filePath)
	return decodeRsv(Files.readAllBytes(Paths.get(filePath)))
}

@JvmOverloads
@Throws(IOException::class)
fun appendRsv(rows: Array<Array<String?>>, filePath: String?, continueLastRow: Boolean = false) {
	val file = RandomAccessFile(filePath, "rw")
	try {
		if (continueLastRow && file.length() > 0) {
			file.seek(file.length() - 1)
			if (file.readByte().toUByte() != 0xFD.toUByte()) {
				throw RuntimeException("Incomplete RSV document")
			}
			if (rows.size == 0) return
			file.seek(file.length() - 1)
		} else {
			file.seek(file.length())
		}
		file.write(encodeRsv(rows))
	} finally {
		file.close()
	}
}

// ----------------------------------------------------------------------

val byteClassLookup = intArrayOf(
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
)
val stateTransitionLookup = intArrayOf(
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
)

fun isValidRsv(bytes: ByteArray): Boolean {
	var lastState = 1
	for (i in bytes.indices) {
		val currentByte: Int = bytes[i].toUByte().toInt()
		val currentByteClass = byteClassLookup[currentByte]
		val newStateLookupIndex = lastState * 15 + currentByteClass
		lastState = stateTransitionLookup[newStateLookupIndex]
		if (lastState == 0) {
			return false
		}
	}
	return lastState == 1
}

// ----------------------------------------------------------------------

fun escapeJsonString(str: String): String {
	val result = StringBuilder()
	result.append("\"")
	for (i in 0 until str.length) {
		val c = str[i].toInt()
		if (c == 0x08) {
			result.append("\\b")
		} else if (c == 0x09) {
			result.append("\\t")
		} else if (c == 0x0A) {
			result.append("\\n")
		} else if (c == 0x0C) {
			result.append("\\f")
		} else if (c == 0x0D) {
			result.append("\\r")
		} else if (c == 0x22) {
			result.append("\\\"")
		} else if (c == 0x5C) {
			result.append("\\\\")
		} else if (c >= 0x00 && c <= 0x1F) {
			result.append("\\u00" + String.format("%02x", c))
		} else {
			result.append(c.toChar())
		}
	}
	result.append("\"")
	return result.toString()
}

fun rsvToJson(rows: Array<Array<String?>>): String {
	val sb = StringBuilder()
	sb.append("[")
	var isFirstRow = true
	for (row in rows) {
		if (!isFirstRow) {
			sb.append(",")
		}
		isFirstRow = false
		sb.append("\n  [")
		var isFirstValue = true
		for (value in row) {
			if (!isFirstValue) {
				sb.append(", ")
			}
			isFirstValue = false
			if (value == null) {
				sb.append("null")
			} else {
				sb.append(escapeJsonString(value))
			}
		}
		sb.append("]")
	}
	sb.append("\n]")
	return sb.toString()
}

// ----------------------------------------------------------------------

@Throws(IOException::class)
fun checkTestFiles(stdout: PrintWriter) {
	for (i in 1..79) {
		val filePath = "./../TestFiles/Valid_" + String.format("%03d", i)
		stdout.println("Checking valid test file: $filePath")
		val loadedRows = loadRsv("$filePath.rsv")
		val jsonStr = rsvToJson(loadedRows)
		val loadedJsonStr = String(Files.readAllBytes(Paths.get("$filePath.json")), StandardCharsets.UTF_8)
		if (jsonStr != loadedJsonStr) {
			throw RuntimeException("JSON mismatch")
		}
		if (!isValidRsv(Files.readAllBytes(Paths.get("$filePath.rsv")))) {
			throw RuntimeException("Validation mismatch")
		}
	}
	for (i in 1..29) {
		val filePath = "./../TestFiles/Invalid_" + String.format("%03d", i)
		stdout.println("Checking invalid test file: $filePath")
		var wasError = false
		try {
			loadRsv("$filePath.rsv")
		} catch (e: Exception) {
			wasError = true
		}
		if (!wasError) {
			throw RuntimeException("RSV document is valid")
		}
		if (isValidRsv(Files.readAllBytes(Paths.get("$filePath.rsv")))) {
			throw RuntimeException("Validation mismatch")
		}
	}
}

// ----------------------------------------------------------------------

fun main(args: Array<String>) {
	try {
		val stdout = PrintWriter(OutputStreamWriter(System.out, StandardCharsets.UTF_8), true)
		val rows = arrayOf(
			arrayOf("Hello", "\uD83C\uDF0E", null, ""),
			arrayOf<String?>("A\u0000B\nC", "Test \uD834\uDD1E"),
			arrayOf(),
			arrayOf<String?>("")
		)
		stdout.println(rsvToJson(rows))
		saveRsv(rows, "Test.rsv")
		val loadedRows = loadRsv("Test.rsv")
		stdout.println(rsvToJson(loadedRows))
		saveRsv(loadedRows, "TestResaved.rsv")
		appendRsv(arrayOf(arrayOf<String?>("ABC")), "Append.rsv", true)
		checkTestFiles(stdout)
		stdout.println("Done")
	} catch (e: Exception) {
		println(e.toString())
	}
}