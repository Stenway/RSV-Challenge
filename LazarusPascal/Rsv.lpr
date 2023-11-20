program Rsv;
{$mode objfpc}{$H+}
{$codepage utf-8}

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	{$IFDEF WINDOWS}
	Windows,
	{$ENDIF}
	SysUtils,
	Nullable,
	Classes;

type
	TNullableString = specialize TNullable<UTF8String>;
	TRow = array of TNullableString;
	TRows = array of TRow;

const
	utf8ByteClassLookup : array[0..255] of byte =
	(
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

	utf8StateTransitionLookup : array[0..107] of byte =
	(
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

	rsvByteClassLookup : array[0..255] of byte =
	(
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

	rsvStateTransitionLookup : array[0..179] of byte =
	(
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

function IsValidUtf8(bytes: TBytes): boolean;
var
	i: integer;
	lastState: byte;
	currentByte: byte;
	currentByteClass: byte;
	newStateLookupIndex: byte;
begin
	lastState := 1;
	for i := 0 to Length(bytes)-1 do
	begin
		currentByte := bytes[i];
		currentByteClass := utf8ByteClassLookup[currentByte];
		newStateLookupIndex := lastState*12+currentByteClass;
		lastState := utf8StateTransitionLookup[newStateLookupIndex];
		if lastState = 0 then Exit(false);
	end;
	Result := (lastState = 1);
end;

function IsValidUtf8x(str: UTF8String): boolean;
var
	startPointer, endPointer: PChar;
	lastState: byte;
	currentByte: byte;
	currentByteClass: byte;
	newStateLookupIndex: byte;
begin
	startPointer := PChar(str);
	endPointer := startPointer + Length(str);
	lastState := 1;
	while startPointer < endPointer do
	begin
		currentByte := byte(startPointer[0]);
		currentByteClass := utf8ByteClassLookup[currentByte];
		newStateLookupIndex := lastState*12+currentByteClass;
		lastState := utf8StateTransitionLookup[newStateLookupIndex];
		if lastState = 0 then Exit(false);
		inc(startPointer, 1);
	end;
	Result := (lastState = 1);
end;

function EncodeRsv(rows: TRows): TBytes;
var
	stream: TBytesStream;
	row: TRow;
	value: TNullableString;
	valueBytes: TBytes;
begin
	stream := TBytesStream.Create;
	try
		for row in rows do
		begin
			for value in row do
			begin
				if value.IsNull then stream.WriteByte($FE)
				else if Length(value.Value) > 0 then
				begin
					valueBytes := BytesOf(value.Value);
					if not IsValidUtf8(valueBytes) then raise Exception.Create('Invalid string value');
					stream.Write(valueBytes[0], Length(valueBytes));
				end;
				stream.WriteByte($FF);
			end;
			stream.WriteByte($FD);
		end;
		result := Copy(stream.Bytes, 0, stream.Size);
	finally
		stream.Free;
	end;
end;

function DecodeRsv(bytes: TBytes): TRows;
var
	i: integer;
	valueStartIndex: integer;
	valueBytesLength: integer;
	valueBytes: TBytes;
	currentRow: TRow;
	currentValue: UTF8String;
begin
	if (Length(bytes) > 0) and (bytes[Length(bytes)-1] <> $FD) then
		raise Exception.Create('Incomplete RSV document');
	result := TRows.Create;
	currentRow := TRow.Create;
	valueStartIndex := 0;
	for i := 0 to Length(bytes)-1 do
	begin
		if bytes[i] = $FF then
		begin
			valueByteslength := i - valueStartIndex;
			if valueByteslength = 0 then
			begin
				SetLength(currentRow, Length(currentRow)+1);
				currentRow[Length(currentRow)-1] := '';
			end
			else if (valueByteslength = 1) and (bytes[valueStartIndex] = $FE) then
			begin
				SetLength(currentRow, Length(currentRow)+1);
				currentRow[Length(currentRow)-1] := Default(TNullableString);
			end
			else
			begin
				SetLength(valueBytes, valueBytesLength);
				Move(bytes[valueStartIndex], valueBytes[0], valueBytesLength);
				if not IsValidUtf8(valueBytes) then
					raise Exception.Create('Invalid string value');
				currentValue := TEncoding.UTF8.GetString(valueBytes, 0, valueBytesLength);
				SetLength(currentRow, Length(currentRow)+1);
				currentRow[Length(currentRow)-1] := currentValue;
			end;
			valueStartIndex := i + 1;
		end
		else if bytes[i] = $FD then
		begin
			if (i > 0) and (valueStartIndex <> i) then
				raise Exception.Create('Incomplete RSV row');
			SetLength(result, Length(result)+1);
			result[Length(result)-1] := currentRow;
			currentRow := TRow.Create;
			valueStartIndex := i + 1;
		end;
	end;
end;

function IsValidRsv(bytes: TBytes): boolean;
var
	i: integer;
	lastState: byte;
	currentByte: byte;
	currentByteClass: byte;
	newStateLookupIndex: byte;
begin
	lastState := 1;
	for i := 0 to Length(bytes)-1 do
	begin
		currentByte := bytes[i];
		currentByteClass := rsvByteClassLookup[currentByte];
		newStateLookupIndex := lastState*15+currentByteClass;
		lastState := rsvStateTransitionLookup[newStateLookupIndex];
		if lastState = 0 then Exit(false);
	end;
	Result := (lastState = 1);
end;

procedure SaveBytes(bytes: TBytes; filePath: string);
var
	fileStream: TFileStream;
begin
	fileStream := TFileStream.Create(filePath, fmCreate);
	try
		if bytes <> nil then
			fileStream.WriteBuffer(bytes[0], Length(bytes));
	finally
		fileStream.Free();
	end;
end;

function LoadBytes(filePath: string): TBytes;
var
	fileStream: TFileStream;
	size: integer;
begin
	fileStream := TFileStream.Create(filePath, fmOpenRead);
	try
		size := fileStream.Size;
		SetLength(result, size);
		fileStream.ReadBuffer(result[0], size);
	finally
		fileStream.Free();
	end;
end;

procedure SaveRsv(rows: TRows; filePath: string);
begin
	SaveBytes(EncodeRsv(rows), filePath);
end;

function LoadRsv(filePath: string): TRows;
begin
	result := DecodeRsv(LoadBytes(filePath));
end;

procedure AppendRsv(rows: TRows; filePath: string; continueLastRow: Boolean);
var
	fileStream: TFileStream;
	bytes: TBytes;
begin
	try
		fileStream := TFileStream.Create(filePath, fmOpenReadWrite);
	except
		on E:Exception do
			fileStream := TFileStream.Create(filePath, fmCreate);
	end;
	try
		if continueLastRow = true and (fileStream.Size > 0) then
		begin
			fileStream.Seek(-1, soEnd);
			if fileStream.ReadByte() <> $FD then
				raise Exception.Create('Incomplete RSV document');
			if Length(rows) = 0 then
				Exit;
			fileStream.Seek(-1, soEnd);
		end
		else
		begin
			fileStream.Seek(0, soEnd);
		end;
		bytes := EncodeRsv(rows);
		if bytes <> nil then
			fileStream.WriteBuffer(bytes[0], Length(bytes));
	finally
		fileStream.Free();
	end;
end;

function EscapeJsonString(str: UTF8String): UTF8String;
var
	i, c: integer;
begin
	result := '"';
	for i := 0 to length(str)-1 do
	begin
		c := ord(str[i + 1]);
		if c = $08 then
			result := result + '\b'
		else if c = $09 then
			result := result + '\t'
		else if c = $0A then
			result := result + '\n'
		else if c = $0C then
			result := result + '\f'
		else if c = $0D then
			result := result + '\r'
		else if c = $22 then
			result := result + '\"'
		else if c = $5C then
			result := result + '\\'
		else if (c >= $00) and (c <= $1F) then
			result := result + '\u00' + LowerCase(IntToHex(c, 2))
		else
			result := result + chr(c);
	end;
	result := result + '"';
end;

function RsvToJsonString(rows: TRows): UTF8String;
var
	row: TRow;
	isFirstRow: boolean;
	isFirstValue: boolean;
	value: TNullableString;
begin
	result := '[';
	isFirstRow := true;
	for row in rows do
	begin
		if not isFirstRow then result := result + ',';
		isFirstRow := false;
		result := result + #$0A'  [';
		isFirstValue := true;
		for value in row do
		begin
			if not isFirstValue then result := result + ', ';
			isFirstValue := false;
			if value.IsNull then result := result + 'null'
			else result := result + EscapeJsonString(value.Value);
		end;
		result := result + ']';
	end;
	result := result + #$0A']';
end;

function LoadTextFile(filePath: string): Utf8String;
var
	bytes: TBytes;
begin
	bytes := LoadBytes(filePath);
	result := TEncoding.UTF8.GetString(bytes, 0, length(bytes));
end;

procedure CheckTestFiles();
var
	i: Integer;
	filePath: string;
	jsonFilePath: string;
	loadedRows: TRows;
	jsonStr: UTF8String;
	loadedJsonStr: UTF8String;
	bytes: TBytes;
	wasError: boolean;
begin
	for i := 1 to 79 do
	begin
		filePath := '.\..\TestFiles\Valid_' + Format('%0.3d', [i]);
		WriteLn('Checking valid test file: ', filePath);
		loadedRows := LoadRsv(filePath+'.rsv');
		jsonStr := RsvToJsonString(loadedRows);

		loadedJsonStr := LoadTextFile(filePath+'.json');
		if jsonStr <> loadedJsonStr then
			raise Exception.Create('JSON mismatch');
		bytes := LoadBytes(filePath+'.rsv');
		if not IsValidRsv(bytes) then
			raise Exception.Create('Validation mismatch');
	end;

	for i := 1 to 29 do
	begin
		filePath := '.\..\TestFiles\Invalid_' + Format('%0.3d', [i]);
		WriteLn('Checking invalid test file: ', filePath);
		wasError := false;
		try
			loadedRows := LoadRsv(filePath+'.rsv');
		except
			wasError := true;
		end;
		if not wasError then
			raise Exception.Create('RSV document is valid');
		bytes := LoadBytes(filePath+'.rsv');
		if IsValidRsv(bytes) then
			raise Exception.Create('Validation mismatch');
	end;
end;

procedure Main();
var
	rows: TRows;
	bytes: TBytes;
	decodedRows: TRows;
	loadedRows: TRows;
	appendRows: TRows;
begin
	rows := TRows.create(
		TRow.create('Hello', '🌎', Default(TNullableString), ''),
		TRow.create('A'#$00'B'#$0A'C', 'Test 𝄞'),
		TRow.create,
		TRow.create('')
	);

	WriteLn(RsvToJsonString(rows));
	SaveRsv(rows, 'Test.rsv');

	bytes := EncodeRsv(rows);
	decodedRows := DecodeRsv(bytes);

	loadedRows := LoadRsv('Test.rsv');
	WriteLn(RsvToJsonString(loadedRows));
	SaveRsv(loadedRows, 'TestResaved.rsv');

	appendRows := TRows.create(TRow.create('ABC'));
	AppendRsv(appendRows, 'Append.rsv', false);

	CheckTestFiles();

	WriteLn('Done');
	ReadLn;
end;

begin
	SetConsoleOutputCP(CP_UTF8);
	try
		Main();
	except
		on E:Exception do
		begin
			WriteLn(E.ToString);
			ReadLn;
		end;
	end;
end.
