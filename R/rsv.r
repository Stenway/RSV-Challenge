# (C) Stefan John / Stenway / Stenway.com / 2023

utf8_byte_class_lookup <- c(
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
)
utf8_state_transition_lookup <- c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 2, 3, 5, 4, 6, 7, 8,
  0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

is_valid_utf8 <- function(bytes) {
  if (length(bytes) == 0) {
    return(TRUE)
  }
  last_state <- 1
  for (i in 1:length(bytes)) {
    current_byte <- as.integer(bytes[i])
    current_byte_class <- utf8_byte_class_lookup[current_byte + 1]
    new_state_lookup_index <- last_state * 12 + current_byte_class
    last_state <- utf8_state_transition_lookup[new_state_lookup_index + 1]
    if (last_state == 0) {
      return(FALSE)
    }
  }
  return(last_state == 1)
}

# ----------------------------------------------------------------------

string_contains_nul_char <- function(bytes) {
  if (length(bytes) == 0) {
    return(FALSE)
  }
  for (i in 1:length(bytes)) {
    if (bytes[i] == 0x00) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# ----------------------------------------------------------------------

encode_rsv <- function(rows) {
  if (!is.list(rows)) {
    stop("Rows are not a list")
  }
  result <- charToRaw("")
  for (row in rows) {
    if (!is.list(row)) {
      print(typeof(row))
      stop("Row is not a list")
    }
    for (value in row) {
      if (is.null(value)) {
        result <- append(result, as.raw(254))
      } else if (is.character(value)) {
        if (nchar(value) > 0) {
          value_bytes <- charToRaw(value)
          if (!is_valid_utf8(value_bytes)) {
            stop("Invalid string value")
          }
          result <- c(result, value_bytes)
        }
      } else if (is.raw(value)) {
        if (!is_valid_utf8(value)) {
          stop("Invalid string value")
        }
        result <- c(result, value)
      } else {
        stop("Not a string, raw string or null")
      }
      result <- append(result, as.raw(255))
    }
    result <- append(result, as.raw(253))
  }
  return(result)
}

decode_rsv <- function(bytes) {
  if (length(bytes) > 0 && bytes[length(bytes)] != 0xFD) {
    stop("Incomplete RSV document")
  }
  result <- list()
  if (length(bytes) == 0) {
    return(result)
  }
  current_row <- list()
  value_start_index <- 1
  for (i in 1:length(bytes)) {
    if (bytes[i] == 0xFF) {
      length <- i - value_start_index
      if (length == 0) {
        current_row[[length(current_row) + 1]] <- ""
      } else if (length == 1 && bytes[value_start_index] == 0xFE) {
        current_row[length(current_row) + 1] <- list(NULL)
      } else {
        value_bytes <- bytes[value_start_index:(value_start_index + length - 1)]
        if (!is_valid_utf8(value_bytes)) {
          stop("Invalid string value")
        }
        if (string_contains_nul_char(value_bytes)) {
          current_row[[length(current_row) + 1]] <- value_bytes
        } else {
          str_value <- rawToChar(value_bytes)
          current_row[[length(current_row) + 1]] <- str_value
        }
      }
      value_start_index <- i + 1
    } else if (bytes[i] == 0xFD) {
      if (i > 0 && value_start_index != i) {
        stop("Incomplete RSV row")
      }
      result[[length(result) + 1]] <- current_row
      current_row <- list()
      value_start_index <- i + 1
    }
  }
  return(result)
}

# ----------------------------------------------------------------------

rsv_byte_class_lookup <- c(
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
rsv_state_transition_lookup <- c(
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

is_valid_rsv <- function(bytes) {
  if (length(bytes) == 0) {
    return(TRUE)
  }
  last_state <- 1
  for (i in 1:length(bytes)) {
    current_byte <- as.integer(bytes[i])
    current_byte_class <- rsv_byte_class_lookup[current_byte + 1]
    new_state_lookup_index <- last_state * 15 + current_byte_class
    last_state <- rsv_state_transition_lookup[new_state_lookup_index + 1]
    if (last_state == 0) {
      return(FALSE)
    }
  }
  return(last_state == 1)
}

# ----------------------------------------------------------------------

save_rsv <- function(rows, file_path) {
  file <- file(file_path, "wb")
  bytes <- encode_rsv(rows)
  writeBin(bytes, file)
  close(file)
}

load_rsv <- function(file_path) {
  file <- file(file_path, "rb")
  bytes <- readBin(file, what = "raw", n = file.info(file_path)$size)
  close(file)
  return(decode_rsv((bytes)))
}

append_rsv <- function(rows, file_path, continue_last_row) {
  if (!file.exists(file_path)) {
    file <- file(file_path, "wb")
    close(file)
  }
  file_size <- file.info(file_path)$size
  file <- file(file_path, "rb")
  file_content <- readBin(file, what = "raw", n = file_size)
  close(file)
  if (continue_last_row && file_size > 0) {
    last_byte <- file_content[length(file_content)]
    if (last_byte != 0xFD) {
      stop("Incomplete RSV document")
    }
    if (length(rows) == 0) {
      return()
    }
  }
  file <- file(file_path, "wb")
  if (length(file_content) > 0) {
    writeBin(file_content, file)
  }
  if (continue_last_row && file_size > 0) {
    seek(file, file_size - 1)
  }
  bytes <- encode_rsv(rows)
  writeBin(bytes, file)
  close(file)
}

# ----------------------------------------------------------------------

escape_json_string <- function(str) {
  bytes <- str
  if (is.character(str)) {
    bytes <- charToRaw(str)
  }
  if (length(bytes) == 0) {
    return("\"\"")
  }
  result <- as.raw(0x22)
  for (i in 1:length(bytes)) {
    b <- bytes[i]
    if (b == 0x08) {
      result <- append(result, charToRaw("\\b"))
    } else if (b == 0x09) {
      result <- append(result, charToRaw("\\t"))
    } else if (b == 0x0A) {
      result <- append(result, charToRaw("\\n"))
    } else if (b == 0x0C) {
      result <- append(result, charToRaw("\\f"))
    } else if (b == 0x0D) {
      result <- append(result, charToRaw("\\r"))
    } else if (b == 0x22) {
      result <- append(result, charToRaw("\\\""))
    } else if (b == 0x5C) {
      result <- append(result, charToRaw("\\\\"))
    } else if (b >= 0x00 && b <= 0x1F) {
      result <- append(result, charToRaw(sprintf("\\u%04x", as.integer(b))))
    } else {
      result <- append(result, b)
    }
  }
  result <- append(result, as.raw(0x22))
  return(rawToChar(result))
}

rsv_to_json <- function(rows) {
  sb <- "["
  is_first_row <- TRUE
  for (row in rows) {
    if (!is_first_row) {
      sb <- paste0(sb, ",")
    }
    is_first_row <- FALSE
    sb <- paste0(sb, "\n  [")
    is_first_value <- TRUE
    for (value in row) {
      if (!is_first_value) {
        sb <- paste0(sb, ", ")
      }
      is_first_value <- FALSE
      if (is.null(value)) {
        sb <- paste0(sb, "null")
      } else {
        sb <- paste0(sb, escape_json_string(value))
      }
    }
    sb <- paste0(sb, "]")
  }
  sb <- paste0(sb, "\n]")
  return(sb)
}

# ----------------------------------------------------------------------

check_test_files <- function() {
  for (i in 1:79) {
    file_path <- paste0("./../TestFiles/Valid_", sprintf("%03d", i))
    cat("Checking valid test file: ", file_path, "\n")
    file_path_rsv <- paste0(file_path, ".rsv")
    loaded_rows <- load_rsv(file_path_rsv)
    json_str <- rsv_to_json(loaded_rows)

    file_path_json <- paste0(file_path, ".json")
    loaded_json_str <- rawToChar(readBin(file_path_json, "raw", n = file.size(file_path_json)))
    if (json_str != loaded_json_str) {
      writeLines(json_str)
      writeLines(loaded_json_str)
      stop("JSON mismatch")
    }
    bytes <- readBin(file_path_rsv, "raw", n = file.size(file_path_rsv))
    if (!is_valid_rsv(bytes)) {
      stop("Validation mismatch")
    }
  }

  for (i in 1:29) {
    file_path <- paste0("./../TestFiles/Invalid_", sprintf("%03d", i))
    cat("Checking invalid test file: ", file_path, "\n")
    file_path_rsv <- paste0(file_path, ".rsv")
    was_error <- tryCatch(
      {
        loaded_rows <- load_rsv(file_path_rsv)
        return(FALSE)
      },
      error = function(e) {
        return(TRUE)
      }
    )
    if (!was_error) {
      stop("RSV document is valid")
    }

    bytes <- readBin(file_path_rsv, "raw", n = file.size(file_path_rsv))
    if (is_valid_rsv(bytes)) {
      stop("Validation mismatch")
    }
  }
}

# ----------------------------------------------------------------------

main <- function() {
  rows <- list(
    list("Hello", "🌎", NULL, ""),
    list(c(charToRaw("A"), as.raw(0), charToRaw("B\nC")), "Test 𝄞"),
    list(),
    list("")
  )
  writeLines(rsv_to_json(rows))
  save_rsv(rows, "Test.rsv")

  loaded_rows <- load_rsv("Test.rsv")
  writeLines(rsv_to_json(loaded_rows))

  save_rsv(loaded_rows, "TestResaved.rsv")

  append_rows <- list(list("ABC"))
  append_rsv(append_rows, "Append.rsv", FALSE)

  check_test_files()

  writeLines("Done")
}

main()