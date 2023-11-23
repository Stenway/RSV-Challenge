/* (C) Stefan John / Stenway / Stenway.com / 2023 */

use std::error::Error;
use std::fs::{self, OpenOptions, File};
use std::io::{Seek, SeekFrom, Write, Read};

fn encode_rsv(rows: &Vec<Vec<Option<String>>>) -> Vec<u8> {
    let mut parts: Vec<&[u8]> = vec![];
    for row in rows {
        for value in row {
            match value {
                None => parts.push(b"\xFE"),
                Some(str_value) => {
                    if !str_value.is_empty() { parts.push(str_value.as_bytes()); }
                }
            }
            parts.push(b"\xFF");
        }
        parts.push(b"\xFD");
    }
    parts.concat()
}

fn decode_rsv(bytes: Vec<u8>) -> Result<Vec<Vec<Option<String>>>, Box<dyn Error>> {
    if bytes.len() > 0 && bytes[bytes.len()-1] != 0xFD {
        return Err("Incomplete RSV document".into());
    }
    let mut result: Vec<Vec<Option<String>>> = Vec::new();
    let mut current_row: Vec<Option<String>> = Vec::new();
    let mut value_start_index = 0;
    for i in 0..bytes.len() {
        if bytes[i] == 0xFF {
            let length = i - value_start_index;
            if length == 0 {
                current_row.push(Some("".to_string()));
            } else if length == 1 && bytes[value_start_index] == 0xFE {
                current_row.push(None);
            } else {
                let value_bytes = bytes[value_start_index..value_start_index+length].to_vec();
                if let Ok(str_value) = String::from_utf8(value_bytes) {
                    current_row.push(Some(str_value));
                } else {
                    return Err("Invalid string value".into());
                }
                
            }
            value_start_index = i + 1;
        } else if bytes[i] == 0xFD {
            if i > 0 && value_start_index != i {
                return Err("Incomplete RSV row".into());
            }
            result.push(current_row);
            current_row = Vec::new();
            value_start_index = i + 1;
        }
    }
    return Ok(result)
}

// ----------------------------------------------------------------------

const RSV_BYTE_CLASS_LOOKUP: [u8; 256] = [
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
const RSV_STATE_TRANSITION_LOOKUP: [u8; 180] = [
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

fn is_valid_rsv(bytes: &Vec<u8>) -> bool {
    let mut last_state: u8 = 1;
    for i in 0..bytes.len() {
        let current_byte = bytes[i];
        let current_byte_class = RSV_BYTE_CLASS_LOOKUP[current_byte as usize];
        let new_state_lookup_index = (last_state * 15) + current_byte_class;
        last_state = RSV_STATE_TRANSITION_LOOKUP[new_state_lookup_index as usize];
        if last_state == 0 {
            return false;
        }
    }
    last_state == 1
}

// ----------------------------------------------------------------------

fn save_rsv(rows: &Vec<Vec<Option<String>>>, file_path: &str) {
    let bytes = encode_rsv(rows);
    fs::write(file_path, bytes).expect("Could not save RSV file");
}

fn load_rsv(file_path: &str) -> Result<Vec<Vec<Option<String>>>, Box<dyn Error>> {
    let bytes = fs::read(file_path).expect("Could not load RSV file");
    return decode_rsv(bytes);
}

fn append_rsv(rows: &Vec<Vec<Option<String>>>, file_path: &str, continue_last_row: bool) {
    let mut file = match OpenOptions::new().read(true).write(true).open(file_path) {
        Ok(file) => file,
        Err(_) => File::create(file_path).unwrap(),
    };
    let file_size = file.metadata().unwrap().len();
    if continue_last_row && file_size > 0 {
        file.seek(SeekFrom::End(-1)).unwrap();
        let mut last_byte = [0u8; 1];
        file.read_exact(&mut last_byte).unwrap();
        if last_byte[0] != 0xFD {
            panic!("Incomplete RSV document");
        }
        if rows.is_empty() {
            return 
        }
        file.seek(SeekFrom::End(-1)).unwrap();
    } else {
        file.seek(SeekFrom::End(0)).unwrap();
    }
    let bytes = encode_rsv(rows);
    file.write_all(&bytes).unwrap();
}

// ----------------------------------------------------------------------

fn escape_json_string(str: &String) -> String {
    let mut result = String::new();
    result.push('"');
    for c in str.chars() {
        match c {
            '\u{0008}' => result.push_str("\\b"),
            '\u{0009}' => result.push_str("\\t"),
            '\u{000A}' => result.push_str("\\n"),
            '\u{000C}' => result.push_str("\\f"),
            '\u{000D}' => result.push_str("\\r"),
            '\u{0022}' => result.push_str("\\\""),
            '\u{005C}' => result.push_str("\\\\"),
            '\u{0000}'..='\u{001F}' => result.push_str(&format!("\\u{:04x}", c as u32)),
            _ => result.push(c),
        }
    }
    result.push('"');
    result
}

fn rsv_to_json(rows: &Vec<Vec<Option<String>>>) -> String {
    let mut sb = String::new();
    sb.push('[');
    let mut is_first_row = true;
    for row in rows {
        if !is_first_row {
            sb.push(',');
        }
        is_first_row = false;
        sb.push('\n');
        sb.push_str("  [");
        let mut is_first_value = true;
        for value in row {
            if !is_first_value {
                sb.push_str(", ");
            }
            is_first_value = false;
            match value {
                None => sb.push_str("null"),
                Some(value) => sb.push_str(&escape_json_string(value)),
            }
        }
        sb.push(']');
    }
    sb.push('\n');
    sb.push(']');
    sb
}

// ----------------------------------------------------------------------

fn check_test_files() {
    for i in 1..=79 {
        let file_path = format!("./../../TestFiles/Valid_{:03}", i);
        println!("Checking valid test file: {}", file_path);
        
        let loaded_rows = load_rsv(&format!("{}.rsv", file_path)).unwrap();
        let json_str = rsv_to_json(&loaded_rows);
        
        let loaded_json_str = fs::read_to_string(&format!("{}.json", file_path)).unwrap();
        if json_str != loaded_json_str {
            panic!("JSON mismatch");
        }
        
        let bytes = fs::read(&format!("{}.rsv", file_path)).unwrap();
        if !is_valid_rsv(&bytes) {
            panic!("Validation mismatch");
        }
    }
    
    for i in 1..=29 {
        let file_path = format!("./../../TestFiles/Invalid_{:03}", i);
        println!("Checking invalid test file: {}", file_path);
        
        if let Ok(_rows) = load_rsv(&format!("{}.rsv", file_path)) {
            panic!("RSV document is valid");
        }

        let bytes = fs::read(&format!("{}.rsv", file_path)).unwrap();
        if is_valid_rsv(&bytes) {
            panic!("Validation mismatch");
        }
    }
}

// ----------------------------------------------------------------------

fn main() {
    let rows = vec![
        vec![Some("Hello".to_string()), Some("🌎".to_string()), None, Some("".to_string())],
        vec![Some("A\0B\nC".to_string()), Some("Test 𝄞".to_string())],
        vec![],
        vec![Some("".to_string())]
    ];
    println!("{}", rsv_to_json(&rows));
    //let bytes = encode_rsv(&rows);
    //println!("{:?}", bytes);
    
    //let decoded_rows = decode_rsv(bytes);
    //println!("{:?}", decoded_rows);
    
    save_rsv(&rows, "Test.rsv");
    
    let loaded_rows = load_rsv("Test.rsv").unwrap();
    println!("{}", rsv_to_json(&loaded_rows));
    
    save_rsv(&loaded_rows, "TestResaved.rsv");
    
    let append_rows = vec![vec![Some("ABC".to_string())]];
    append_rsv(&append_rows, "Append.rsv", true);
    
    check_test_files();    
    
    println!("Done");
}
