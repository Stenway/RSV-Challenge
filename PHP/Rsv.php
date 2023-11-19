<?php
/* (C) Stefan John / Stenway / Stenway.com / 2023 */

function encodeRsv(array $rows) : string {
	$parts = [];
	foreach ($rows as $row) {
		if (!is_array($row)) { throw new \Exception("Invalid rows"); }
		foreach ($row as $value) {
			if ($value === null) { array_push($parts, "\xFE"); }
			else if (!is_string($value)) { throw new \Exception("Invalid rows value"); }
			else if (strlen($value) > 0) {
				if (mb_check_encoding($value, "utf8") === false) { throw new \Exception("Invalid UTF-8 string"); }
				array_push($parts, $value); 
			}
			array_push($parts, "\xFF");
		}
		array_push($parts, "\xFD"); 
	}
	return join($parts);
}

function decodeRsv(string $bytes) : array {
	if (strlen($bytes) > 0 && ord($bytes[strlen($bytes)-1]) != 0xFD) { throw new \Exception("Incomplete RSV document"); }
	$result = [];
	$currentRow = [];
	$valueStartIndex = 0;
	for ($i=0; $i<strlen($bytes); $i++) {
		if (ord($bytes[$i]) == 0xFF) {
			$length = $i-$valueStartIndex;
			if ($length == 0) { array_push($currentRow, ""); }
			else if ($length == 1 && ord($bytes[$valueStartIndex]) == 0xFE) { array_push($currentRow, null); }
			else {
				$valueBytes = substr($bytes, $valueStartIndex, $length);
				if (mb_check_encoding($valueBytes, "utf8") === false) { throw new \Exception("Invalid UTF-8 string"); }
				array_push($currentRow, $valueBytes); 
			}
			$valueStartIndex = $i+1;
		} else if (ord($bytes[$i]) == 0xFD) {
			if ($i > 0 && $valueStartIndex != $i) { throw new \Exception("Incomplete RSV row"); }
			array_push($result, $currentRow);
			$currentRow = [];
			$valueStartIndex = $i+1;
		}
	}
	return $result;
}

// ----------------------------------------------------------------------

function isValidRsv(string $bytes) : bool {
	$byteClassLookup = [
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
	];
	
	$stateTransitionLookup = [
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
	];
	
	$lastState = 1;
	for ($i=0; $i<strlen($bytes); $i++) {
		$currentByte = ord($bytes[$i]);
		$currentByteClass = $byteClassLookup[$currentByte];
		$newStateLookupIndex = $lastState*15+$currentByteClass;
		$lastState = $stateTransitionLookup[$newStateLookupIndex];
		if ($lastState == 0) { return false; }
	}
	return ($lastState == 1);
}

// ----------------------------------------------------------------------

function saveRsv(array $rows, string $filePath) {
	$bytes = encodeRsv($rows);
	$result = file_put_contents($filePath, $bytes);
	if ($result !== strlen($bytes)) { throw new \Exception("RSV file not fully written"); }
}

function loadRsv(string $filePath) : array {
	$bytes = file_get_contents($filePath);
	if ($bytes === false) { throw new \Exception("RSV file could not be read"); }
	return decodeRsv($bytes);
}

function appendRsv(array $rows, string $filePath, bool $continueLastRow = false) {
	$file = fopen($filePath, "c+");
	if ($file === false) { throw new \Exception("Could not append RSV"); }
	fseek($file, 0, SEEK_END);
	$fileSize = ftell($file);
	if ($continueLastRow && $fileSize > 0) {
		fseek($file, -1, SEEK_END);
		$lastByte = fread($file, 1);
		if ($lastByte === false) { throw new \Exception("Could not read last byte"); }
		if (ord($lastByte[0]) != 0xFD) {
			throw new RuntimeException("Incomplete RSV document");
		}
		if (count($rows) == 0) {
			return;
		}
		fseek($file, -1, SEEK_END);
	} else {
		fseek($file, 0, SEEK_END);
	}
	$bytes = encodeRsv($rows);
	if (fwrite($file, $bytes) !== strlen($bytes)) { throw new \Exception("Could write RSV bytes"); }
	if (fclose($file) === false) { throw new \Exception("Could not close file handle"); }
}

// ----------------------------------------------------------------------

function escapeJsonString(string $str) : string {
	$result = '"';
	for ($i = 0; $i < strlen($str); $i++) {
		$c = $str[$i];
		if ($c == "\x08") { $result .= "\\b"; }
		else if ($c == "\x09") { $result .= "\\t"; }
		else if ($c == "\x0A") { $result .= "\\n"; }
		else if ($c == "\x0C") { $result .= "\\f"; }
		else if ($c == "\x0D") { $result .= "\\r"; }
		else if ($c == "\x22") { $result .= "\\\""; }
		else if ($c == "\x5C") { $result .= "\\\\"; }
		else if (ord($c) >= 0x00 && ord($c) <= 0x1F) { $result .= "\\u" . sprintf("%04x", ord($c)); }
		else { $result .= $c; }
	}
	$result .= '"';
	return $result;
}

function rsvToJson(array $rows) : string {
	$result = "[";
	$isFirstRow = true;
	foreach ($rows as $row) {
		if (!is_array($row)) { throw new \Exception("Invalid rows"); }
		if (!$isFirstRow) { $result .= ","; }
		$isFirstRow = false;
		$result .= "\n  [";
		$isFirstValue = true;
		foreach ($row as $value) {
			if (!$isFirstValue) { $result .= ", "; }
			$isFirstValue = false;
			if ($value === null) { $result .= "null"; }
			else if (!is_string($value)) { throw new \Exception("Invalid rows value"); }
			else { $result .= escapeJsonString($value); }
		}
		$result .= "]";
	}
	$result .= "\n]";
	return $result;
}

function printRsvToJson(array $rows) {
	echo rsvToJson($rows). "\n";
}

// ----------------------------------------------------------------------

function checkTestFiles() {
	for ($i=1; $i<=79; $i++) {
		$filePath = "./../TestFiles/Valid_" . sprintf("%03d", $i);
		echo "Checking valid test file: " . $filePath . "\n";
		$loadedRows = loadRsv($filePath . ".rsv");
		$jsonStr = rsvToJson($loadedRows);
		$loadedJsonStr = file_get_contents($filePath . ".json");
		if ($jsonStr !== $loadedJsonStr) {
			throw new RuntimeException("JSON mismatch");
		}
		if (!isValidRsv(file_get_contents($filePath . ".rsv"))) {
			throw new RuntimeException("Validation mismatch");
		}
	}
	for ($i=1; $i<=29; $i++) {
		$filePath = "./../TestFiles/Invalid_" . sprintf("%03d", $i);
		echo "Checking invalid test file: " . $filePath . "\n";
		$wasError = false;
		try {
			loadRsv($filePath . ".rsv");
		} catch(Exception $e) {
			$wasError = true;
		}
		if (!$wasError) {
			throw new RuntimeException("RSV document is valid");
		}
		if (isValidRsv(file_get_contents($filePath . ".rsv"))) {
			throw new RuntimeException("Validation mismatch");
		}
	}
}

// ----------------------------------------------------------------------

$rows = [
	["Hello", "🌎", null, ""],
	["A\0B\nC", "Test 𝄞"],
	[],
	[""]
];
printRsvToJson($rows);

$bytes = encodeRsv($rows);
$decodedRows = decodeRsv($bytes);

saveRsv($rows, "Test.rsv");

$loadedRows = loadRsv("Test.rsv");
printRsvToJson($loadedRows);

saveRsv($loadedRows, "TestResaved.rsv");

appendRsv([["ABC"]], "Append.rsv", false);

checkTestFiles();

echo "Done";

?>