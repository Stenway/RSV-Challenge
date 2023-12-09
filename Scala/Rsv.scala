import java.io.PrintWriter
import java.io.OutputStreamWriter
import java.nio.charset.{CharsetDecoder, CodingErrorAction}
import java.nio.charset.StandardCharsets
import util.boundary, boundary.break
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import java.io.{IOException, RandomAccessFile}

def encodeRsv(rows: Array[Array[String]]): Array[Byte] = {
	var bytes = new ArrayBuffer[Byte]()
	for (row <- rows) {
		for (value <- row) {
			if (value == null) { bytes += 0xFE.toByte }
			else if (value.length > 0) { bytes ++= value.getBytes("UTF-8") }
			bytes += 0xFF.toByte
		}
		bytes += 0xFD.toByte
	}
	bytes.toArray
}


def decodeRsv(bytes: Array[Byte]): Array[Array[String]] = {
	if (bytes.length > 0 && (bytes(bytes.length-1) & 0xFF) != 0xFD) {
		throw new RuntimeException("Incomplete RSV document")
	}
	val decoder: CharsetDecoder = StandardCharsets.UTF_8.newDecoder()
	decoder.onMalformedInput(CodingErrorAction.REPORT)
	decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
	val result: ArrayBuffer[Array[String]] = ArrayBuffer.empty[Array[String]]
	val currentRow: ArrayBuffer[String] = ArrayBuffer.empty[String]
	var valueStartIndex: Int = 0
	for (i <- 0 until bytes.length) {
		val currentByte: Int = bytes(i) & 0xFF
		if (currentByte == 0xFF) {
			val length: Int = i - valueStartIndex
			if (length == 0) {
				currentRow += ""
			} else if (length == 1 && (bytes(valueStartIndex) & 0xFF) == 0xFE) {
				currentRow += null
			} else {
				val valueBytes = bytes.slice(valueStartIndex, valueStartIndex+length)
				try {
					currentRow += decoder.decode(ByteBuffer.wrap(valueBytes)).toString()
				} catch {
					case e: Exception => throw new RuntimeException("Invalid string value", e)
				}
			}
			valueStartIndex = i + 1
		} else if (currentByte == 0xFD) {
			if (i > 0 && valueStartIndex != i) {
				throw new RuntimeException("Incomplete RSV row")
			}
			result += currentRow.toArray
			currentRow.clear()
			valueStartIndex = i + 1
		}
	}
	result.toArray
}

// ----------------------------------------------------------------------

val byteClassLookup: Array[Int] = Array(
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

val stateTransitionLookup: Array[Int] = Array(
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

def isValidRsv(bytes: Array[Byte]): Boolean = {
	var lastState: Int = 1
	boundary {
		for (i <- 0 until bytes.length) {
			val currentByte: Int = bytes(i) & 0xFF
			val currentByteClass: Int = byteClassLookup(currentByte)
			val newStateLookupIndex: Int = lastState * 15 + currentByteClass
			lastState = stateTransitionLookup(newStateLookupIndex)
			if (lastState == 0) {
				break(false)
			}
		}
	}
	lastState == 1
}

// ----------------------------------------------------------------------

def saveRsv(rows: Array[Array[String]], filePath: String): Unit = {
	val bytes: Array[Byte] = encodeRsv(rows)
	Files.write(Paths.get(filePath), bytes)
}

def loadRsv(filePath: String): Array[Array[String]] = {
	val bytes: Array[Byte] = Files.readAllBytes(Paths.get(filePath))
	return decodeRsv(bytes)
}

def appendRsv(rows: Array[Array[String]], filePath: String, continueLastRow: Boolean): Unit = {
	val file = new RandomAccessFile(filePath, "rw")
	try {
		if (continueLastRow && file.length() > 0) {
			file.seek(file.length() - 1)
			if ((file.readByte() & 0xFF) != 0xFD) throw new RuntimeException("Incomplete RSV document")
			if (rows.length == 0) return
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

def escapeJsonString(str: String): String = {
	val result = new StringBuilder()
	result.append("\"")
	for (i <- 0 until str.length()) {
		val c = str.charAt(i)
		if (c == 0x08) { result.append("\\b") }
		else if (c == 0x09) { result.append("\\t") }
		else if (c == 0x0A) { result.append("\\n") }
		else if (c == 0x0C) { result.append("\\f") }
		else if (c == 0x0D) { result.append("\\r") }
		else if (c == 0x22) { result.append("\\\"") }
		else if (c == 0x5C) { result.append("\\\\") }
		else if (c >= 0x00 && c <= 0x1F) { result.append("\\u00" + f"$c%02x") }
		else { result.append(c) }
	}
	result.append("\"")
	result.toString()
}

def rsvToJson(rows: Array[Array[String]]): String = {
	val sb = new StringBuilder()
	sb.append("[")
	var isFirstRow = true
	for (row <- rows) {
		if (!isFirstRow) { sb.append(",") }
		isFirstRow = false
		sb.append("\n  [")
		var isFirstValue = true
		for (value <- row) {
			if (!isFirstValue) { sb.append(", ") }
			isFirstValue = false
			if (value == null) { sb.append("null") }
			else { sb.append(escapeJsonString(value)) }
		}
		sb.append("]")
	}
	sb.append("\n]")
	sb.toString()
}

// ----------------------------------------------------------------------

def checkTestFiles(stdout: PrintWriter): Unit = {
	for (i <- 1 to 79) {
		val filePath = s"./../TestFiles/Valid_${"%03d".format(i)}"
		stdout.println(s"Checking valid test file: $filePath")
		val loadedRows = loadRsv(filePath + ".rsv")
		val jsonStr = rsvToJson(loadedRows)
		val loadedJsonStr = new String(Files.readAllBytes(Paths.get(filePath + ".json")), StandardCharsets.UTF_8)
		if (jsonStr != loadedJsonStr) {
			throw new RuntimeException("JSON mismatch")
		}
		if (!isValidRsv(Files.readAllBytes(Paths.get(filePath + ".rsv")))) {
			throw new RuntimeException("Validation mismatch")
		}
	}
	for (i <- 1 to 29) {
		val filePath = s"./../TestFiles/Invalid_${"%03d".format(i)}"
		stdout.println(s"Checking invalid test file: $filePath")
		var wasError = false
		try {
			loadRsv(filePath + ".rsv")
		} catch {
			case e: Exception => wasError = true
		}
		if (!wasError) {
			throw new RuntimeException("RSV document is valid")
		}
		if (isValidRsv(Files.readAllBytes(Paths.get(filePath + ".rsv")))) {
			throw new RuntimeException("Validation mismatch")
		}
	}
}

// ----------------------------------------------------------------------

@main def main() = {
	val stdout = new PrintWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8), true)
	val rows: Array[Array[String]] = Array(
		Array("Hello", "🌎", null, ""),
		Array("A\u0000B\nC", "Test 𝄞"),
		Array[String](),
		Array("")
	)
	stdout.println(rsvToJson(rows))	
	saveRsv(rows, "Test.rsv")
	
	var loadedRows: Array[Array[String]] = loadRsv("Test.rsv")
	stdout.println(rsvToJson(loadedRows))
	saveRsv(loadedRows, "TestResaved.rsv")

	val appendRows: Array[Array[String]] = Array(Array("ABC"))
	appendRsv(appendRows, "Append.rsv", false)

	checkTestFiles(stdout)

	println("Done")
}
