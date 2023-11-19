# (C) Stefan John / Stenway / Stenway.com / 2023
1;

function result = isValidUtf8(str)
  utf8ByteClassLookup = [
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ...
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, ...
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
    0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, ...
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, ...
    6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 7, 7, ...
    9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ];

  utf8StateTransitionLookup = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8, ...
    0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ];

  bytes = uint8(str);
  lastState = 1;
  for i = 1:length(bytes)
    currentByte = bytes(i);
    currentByteClass = utf8ByteClassLookup(currentByte+1);
    newStateLookupIndex = lastState*12 + currentByteClass;
    lastState = utf8StateTransitionLookup(newStateLookupIndex+1);
    if lastState == 0
      result = false;
      return;
    end
  end
  result = (lastState == 1);
end

# ----------------------------------------------------------------------

function result = encodeRsv(rows)
  result = uint8("");
  for i = 1:length(rows)
    row = rows{i};
    for j = 1:length(row)
      value = row{j};
      if ~isequal(value, "") && isempty(value)
        result(end+1) = uint8("\xFE");
      elseif length(value) > 0
        if ~isValidUtf8(value)
            error("Invalid string value");
        end
        valueBytes = uint8(value);
        for k = 1:length(valueBytes)
          result(end+1) = valueBytes(k);
        end
      end
      result(end+1) = uint8("\xFF");
    end
    result(end+1) = uint8("\xFD");
  end
end

function result = decodeRsv(bytes)
  if (length(bytes) > 0 && bytes(end) ~= 253)
    error("Incomplete RSV document");
  end
  result = {};
  currentRow = {};
  valueStartIndex = 1;
  for i = 1:length(bytes)
    if bytes(i) == 255
      length = i-valueStartIndex;
      if length == 0
        currentRow{end+1} = "";
      elseif length == 1 && bytes(valueStartIndex) == 254
        currentRow{end+1} = {};
      else
        valueBytes = bytes(valueStartIndex:valueStartIndex+length-1);
        strValue = native2unicode(transpose(valueBytes), "UTF-8");
        if ~isValidUtf8(strValue)
          error("Invalid string value");
        end
        currentRow{end+1} = strValue;
      end
      valueStartIndex = i+1;
    elseif bytes(i) == 253
      if i > 0 && valueStartIndex != i
        error('Incomplete RSV row');
      end
      result{end+1} = currentRow;
      currentRow = {};
      valueStartIndex = i+1;
    end
  end
end

# ----------------------------------------------------------------------

function result = isValidRsv(bytes)
  byteClassLookup = [
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ...
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, ...
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, ...
    0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, ...
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, ...
    6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 7, 7, ...
    9, 10, 10, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 14
  ];

  stateTransitionLookup = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11, ...
    0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 0, 0, 11, ...
    0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 0, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 6, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, ...
    0, 2, 0, 0, 0, 3, 4, 6, 5, 7, 8, 9, 1, 10, 11
  ];

  lastState = 1;
  for i = 1:length(bytes)
    currentByte = bytes(i);
    currentByteClass = byteClassLookup(currentByte+1);
    newStateLookupIndex = lastState*15 + currentByteClass;
    lastState = stateTransitionLookup(newStateLookupIndex+1);
    if lastState == 0
      result = false;
      return;
    end
  end
  result = (lastState == 1);
end

# ----------------------------------------------------------------------

function [] = saveRsv(rows, filePath)
  file = fopen(filePath, "w");
  bytes = encodeRsv(rows);
  fwrite(file, bytes);
  fclose(file);
end

function result = loadRsv(filePath)
  file = fopen(filePath, "r");
  bytes = fread(file);
  fclose(file);
  result = decodeRsv(bytes);
end

function appendRsv(rows, filePath, continueLastRow)
  file = fopen(filePath, "rb+");
  if file == -1
    file = fopen(filePath, "wb+");
    if file == -1
      error("Opening file failed");
    end
  end
  fseek(file, 0, "eof");
  fileSize = ftell(file);
  if continueLastRow && fileSize > 0
    fseek(file, fileSize - 1, "bof");
    lastByte = fread(file, 1, "uint8");
    if lastByte ~= 253
      fclose(file);
      error("Incomplete RSV document");
    end
    if isempty(rows)
      fclose(file);
    end
    fseek(file, fileSize - 1, "bof");
  end
  bytes = encodeRsv(rows);
  fwrite(file, bytes);
  fclose(file);
end

# ----------------------------------------------------------------------

function result = escapeJsonString(str)
  resultBytes = uint8("\"");
  bytes = uint8(str);
  for i = 1:length(bytes)
    c = bytes(i);
    if c == 8
      resultBytes = [resultBytes uint8("\\b")];
    elseif c == 9
      resultBytes = [resultBytes uint8("\\t")];
    elseif c == 10
      resultBytes = [resultBytes uint8("\\n")];
    elseif c == 12
      resultBytes = [resultBytes uint8("\\f")];
    elseif c == 13
      resultBytes = [resultBytes uint8("\\r")];
    elseif c == 34
      resultBytes = [resultBytes uint8("\\\"")];
    elseif c == 92
      resultBytes = [resultBytes uint8("\\\\")];
    elseif c >= 0 && c <= 31
      resultBytes = [resultBytes uint8("\\u00") uint8(lower(dec2hex(c, 2)))];
    else
      resultBytes = [resultBytes c];
    end
  end
  resultBytes = [resultBytes uint8("\"")];
  result = native2unicode(resultBytes, 'UTF-8');
end

function result = rsvToJson(rows)
  result = "[";
  isFirstRow = true;
  for i = 1:length(rows)
    if ~isFirstRow
      result = [result ","];
    end
    isFirstRow = false;
    result = [result "\n  ["];
    isFirstValue = true;
    currentRow = rows{i};
    for j = 1:length(currentRow)
      value = currentRow{j};
      if ~isFirstValue
        result = [result ", "];
      end
      isFirstValue = false;
      if isequal(value, "")
        result = [result "\"\""];
      elseif isempty(value)
        result = [result "null"];
      else
        result = [result escapeJsonString(value)];
      end
    end
    result = [result "]"];
  end
  result = [result "\n]"];
end

# ----------------------------------------------------------------------

function result = loadFile(filePath)
  file = fopen(filePath, "r");
  result = fread(file);
  fclose(file);
end

function [] = checkTestFiles()
  for i = 1:79
    filePath = [".\\..\\TestFiles\\Valid_" sprintf("%03d", i)];
    disp(["Checking valid test file: " filePath]);

    loadedRows = loadRsv([filePath ".rsv"]);
    jsonStr = rsvToJson(loadedRows);

    loadedJsonStr = fileread([filePath ".json"]);
    if ~strcmp(jsonStr, loadedJsonStr)
      error("JSON mismatch");
    end

    bytes = loadFile([filePath ".rsv"]);
    if ~isValidRsv(bytes)
      error("Validation mismatch");
    end
  end

  for i = 1:29
    filePath = [".\\..\\TestFiles\\Invalid_" sprintf("%03d", i)];
    disp(["Checking invalid test file: " filePath]);

    wasError = false;
    try
      loadedRows = loadRsv([filePath ".rsv"]);
    catch e
      wasError = true;
    end
    if ~wasError
      error("RSV document is valid");
    end

    bytes = loadFile([filePath ".rsv"]);
    if isValidRsv(bytes)
      error("Validation mismatch");
    end
  end
end

# ----------------------------------------------------------------------

rows = {
  {"Hello", "🌎", {}, ""};
  {"A\0B\nC", "Test 𝄞"};
  {};
  {""}
};

disp(rsvToJson(rows))
saveRsv(rows, "Test.rsv")

loadedRows = loadRsv("Test.rsv");
disp(rsvToJson(loadedRows))

saveRsv(loadedRows, "TestResaved.rsv")

append = {{"ABC"}};
appendRsv(append, "Append.rsv", false);

checkTestFiles();

disp("Done")