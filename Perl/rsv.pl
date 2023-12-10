use strict;
use warnings;
use Encode;
use utf8;
use Encode qw( _utf8_on );
use Fcntl qw(SEEK_SET O_CREAT O_RDWR);

my @utf8ByteClassLookup = (
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
);
my @utf8StateTransitionLookup = (
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8,
	0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

sub isValidUtf8 {
	my ($bytesStr) = @_;
	my @bytes = unpack("C*", $bytesStr);
	my $lastState = 1;
	for (my $i = 0; $i < scalar(@bytes); $i++) {
		my $currentByte = $bytes[$i];
		my $currentByteClass = $utf8ByteClassLookup[$currentByte];
		my $newStateLookupIndex = $lastState * 12 + $currentByteClass;
		$lastState = $utf8StateTransitionLookup[$newStateLookupIndex];
		if ($lastState == 0) {
			return 0;
		}
	}
	return ($lastState == 1) ? 1 : 0;
}

# ----------------------------------------------------------------------

sub encodeRsv {
	my ($rows) = @_;
	my $result = "";
	foreach my $row (@$rows) {
		foreach my $value (@$row) {
			if (!defined $value) { $result .= pack('C', 254); }
			elsif (length($value) > 0) {
				my $valueBytes = encode("utf8", $value);
				if (!isValidUtf8($valueBytes)) { die "Invalid string value"; }
				$result .= $valueBytes;
			}
			$result .= pack('C', 255);
		}
		$result .= pack('C', 253);
	}
	return $result;
}

sub decodeRsv {
	my ($bytesStr) = @_;
	my @bytes = unpack("C*", $bytesStr);
	if (scalar(@bytes) > 0 && $bytes[$#bytes] != 0xFD) {
		die "Incomplete RSV document";
	}
	my @result;
	my @currentRow;
	my $valueStartIndex = 0;
	for (my $i = 0; $i < scalar(@bytes); $i++) {
		if ($bytes[$i] == 0xFF) {
			my $valueLength = $i-$valueStartIndex;
			if ($valueLength == 0) {
				push @currentRow, "";
			} elsif ($valueLength == 1 && $bytes[$valueStartIndex] == 0xFE) {
				push @currentRow, undef;
			} else {
				my @valueBytes = @bytes[$valueStartIndex..$valueStartIndex+$valueLength-1];
				my $value = pack('C*', @valueBytes);
				if (!isValidUtf8($value)) { die "Invalid string value"; }
				_utf8_on($value);
				push @currentRow, $value;
			}
			$valueStartIndex = $i + 1;
		} elsif ($bytes[$i] == 0xFD) {
			if ($i > 0 && $valueStartIndex != $i) {
				die "Incomplete RSV row";
			}
			push @result, [@currentRow];
			@currentRow = ();
			$valueStartIndex = $i + 1;
		}
	}

	return \@result;
}

# ----------------------------------------------------------------------

my @rsvByteClassLookup = (
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
);
my @rsvStateTransitionLookup = (
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
);

sub isValidRsv {
	my ($bytesStr) = @_;
	my @bytes = unpack("C*", $bytesStr);
	my $lastState = 1;
	for (my $i = 0; $i < scalar(@bytes); $i++) {
		my $currentByte = $bytes[$i];
		my $currentByteClass = $rsvByteClassLookup[$currentByte];
		my $newStateLookupIndex = $lastState * 15 + $currentByteClass;
		$lastState = $rsvStateTransitionLookup[$newStateLookupIndex];
		if ($lastState == 0) {
			return 0;
		}
	}
	return ($lastState == 1) ? 1 : 0;
}

# ----------------------------------------------------------------------

sub writeAllBytes {
	my ($content, $filePath) = @_;
	my $file;
	open $file, '>:raw', $filePath or die $!;
	binmode $file;
	print $file $content;
	close $file;
}

sub saveRsv {
	my ($rows, $filePath) = @_;
	my $bytes = encodeRsv($rows);
	writeAllBytes($bytes, $filePath);
}

sub readAllBytes {
	my ($filePath) = @_;
	my $file;
	open $file, '<:raw', $filePath or die $!;
	binmode $file;
	my @stat = stat $file;
	my $fileSize = $stat[7];
	my $content = '';
	read $file, $content, $fileSize;
	close $file;
	return $content;
}

sub loadRsv {
	my ($filePath) = @_;
	my $bytesStr = readAllBytes($filePath);
	return decodeRsv($bytesStr);
}

sub appendRsv {
	my ($rows, $filePath, $continueLastRow) = @_;
	my $file;
	unless (sysopen($file, $filePath, O_RDWR)) {
		unless (sysopen($file, $filePath, O_CREAT | O_RDWR)) {
			die "Opening file failed";
		}
	}
	my $fileSize = (stat($file))[7];
	if ($continueLastRow && $fileSize > 0) {
		seek($file, $fileSize - 1, SEEK_SET);
		my $lastByte;
		read($file, $lastByte, 1);
		if (ord($lastByte) != 0xFD) {
			close($file);
			die "Incomplete RSV document";
		}
		if (@$rows == 0) {
			close($file);
			return;
		}
		seek($file, $fileSize - 1, SEEK_SET);
	} else {
		seek($file, $fileSize, SEEK_SET);
	}
	my $bytes = encodeRsv($rows);
	unless (syswrite($file, $bytes) == length($bytes)) {
		close($file);
		die "Writing all bytes failed";
	}
	close($file);
}

# ----------------------------------------------------------------------

sub escapeJsonString {
	my ($str) = @_;
	my $result = '"';
	for (my $i = 0; $i < length($str); $i++) {
		my $c = ord(substr($str, $i, 1));
		if ($c == 0x08) { $result .= "\\b"; }
		elsif ($c == 0x09) { $result .= "\\t"; }
		elsif ($c == 0x0A) { $result .= "\\n"; }
		elsif ($c == 0x0C) { $result .= "\\f"; }
		elsif ($c == 0x0D) { $result .= "\\r"; }
		elsif ($c == 0x22) { $result .= "\\\""; }
		elsif ($c == 0x5C) { $result .= "\\\\"; }
		elsif ($c >= 0x00 && $c <= 0x1F) { $result .= "\\u00" . sprintf("%02x", $c); }
		else { $result .= chr($c); }
	}
	$result .= '"';
	return $result;
}

sub rsvToJson {
	my ($rows) = @_;
	my $sb = "[";
	my $isFirstRow = 1;
	foreach my $row (@$rows) {
		if (!$isFirstRow) { $sb .= ","; }
		$isFirstRow = 0;
		$sb .= "\n  [";
		my $isFirstValue = 1;
		foreach my $value (@$row) {
			if (!$isFirstValue) { $sb .= ", "; }
			$isFirstValue = 0;
			if (!defined $value) { $sb .= "null"; }
			else { $sb .= escapeJsonString($value); }
		}
		$sb .= "]";
	}
	$sb .= "\n]";
	return $sb;
}

sub printRsv {
	my ($rows) = @_;
	print rsvToJson($rows) . "\n";
}

# ----------------------------------------------------------------------

sub checkTestFiles {
	for (my $i = 1; $i <= 79; $i++) {
		my $filePath = './../TestFiles/Valid_' . sprintf("%03d", $i);
		print("Checking valid test file: $filePath\n");
		select()->flush();
		
		my $loadedRows = loadRsv("$filePath.rsv");
		my $jsonStr = rsvToJson($loadedRows);

		my $loadedJsonStr = readAllBytes("$filePath.json");
		_utf8_on($loadedJsonStr);
		if ($jsonStr ne $loadedJsonStr) {
			die "JSON mismatch";
		}

		my $rsvFileContent = readAllBytes("$filePath.rsv");
		if (!isValidRsv($rsvFileContent)) {
			die "Validation mismatch";
		}
	}
	
	for (my $i = 1; $i <= 29; $i++) {
		my $filePath = './../TestFiles/Invalid_' . sprintf("%03d", $i);
		print("Checking invalid test file: $filePath\n");
		my $wasError = 0;
		eval { loadRsv("$filePath.rsv"); 1 } or do { $wasError = 1 };
		if (!$wasError) {
			die "RSV document is valid";
		}
		my $rsvFileContent = readAllBytes("$filePath.rsv");
		if (isValidRsv($rsvFileContent)) {
			die "Validation mismatch";
		}
	}
}

# ----------------------------------------------------------------------

use open ":std", ":encoding(UTF-8)";

my $rows = [
	["Hello", "\x{1F30E}", undef, ""],
	["A\x{0}B\nC", "Test \x{1D11E}"],
	[],
	[""]
];
printRsv($rows);
saveRsv($rows, "Test.rsv");

my $loadedRows = loadRsv("Test.rsv");
printRsv($loadedRows);
saveRsv($loadedRows, "TestResaved.rsv");

my $appendRows = [["ABC"]];
appendRsv($appendRows, "Append.rsv", 1);

checkTestFiles();

print "Done";