let test_path = "../TestFiles/"

module RsvReader = Rsv.Reader(In_channel)
module RsvListEncoder = Rsv.Encoder(List)

let test_invalid fname =
  let file = open_in_bin fname in
  Fun.protect ~finally:(fun () -> close_in file) @@ fun () ->
  if RsvReader.validate_all file then begin
    print_endline @@ "File " ^ fname ^ " is valid and should not be!";
    false end
  else true

let test_valid_encode fname expected =
  let file = open_in_bin fname in
  let file_contents =
    Fun.protect ~finally:(fun () -> close_in file) (fun () -> Bytes.unsafe_of_string @@ In_channel.input_all file)
  in
  if Rsv.Decoder.validate_all file_contents then begin
    let decoded = Rsv.Decoder.read_all file_contents in
    let out_buffer = Buffer.create 100 in
    RsvListEncoder.output_table out_buffer decoded;
    let encoded = Buffer.contents out_buffer in
    if (Bytes.unsafe_to_string file_contents) = encoded then
      if decoded = expected then
        true
      else begin
        print_endline @@ "File " ^ fname ^ " does not decode to the expected value";
        false end
    else begin
      print_endline @@ "File " ^ fname ^ " does not encode back to the same contents";
      false end
  end else begin
    print_endline @@ "File " ^ fname ^ " is not valid";
    false end

let decode_json fname =
  let json = Yojson.Basic.from_file fname in
  let open Yojson.Basic.Util in
  let to_string_or_none = function
    | `String s -> Some s
    | `Null -> None
    | _ -> failwith "Unexpected type in JSON"
  in
  json |> to_list |> List.map (fun row -> row |> to_list |> List.map to_string_or_none)

let run_all_tests () =
  let test_files = Sys.readdir test_path in
  let valid_tests, invalid_tests =
    test_files |> Array.to_list |> List.filter (fun fname -> Filename.check_suffix fname ".rsv") |>
    List.partition (fun fname -> String.starts_with fname ~prefix:"Valid")
  in

  let valid_tests = List.map (fun fname -> test_path ^ fname) valid_tests in
  let invalid_tests = List.map (fun fname -> test_path ^ fname) invalid_tests in

  print_endline "Running invalid encoding tests...";
  let passed_invalid_tests = List.fold_left (fun passed fname ->
    if test_invalid fname then passed + 1 else passed
  ) 0 invalid_tests in

  print_endline @@ "Passed " ^ string_of_int passed_invalid_tests ^ " of " ^ string_of_int (List.length invalid_tests) ^ " invalid encoding tests";

  print_endline "Running valid encoding tests...";
  let passed_valid_tests = List.fold_left (fun passed fname ->
    let expected = decode_json ((Filename.chop_suffix fname ".rsv") ^ ".json") in
    if test_valid_encode fname expected then passed + 1 else passed
  ) 0 valid_tests in
  print_endline @@ "Passed " ^ string_of_int passed_valid_tests ^ " of " ^ string_of_int (List.length valid_tests) ^ " valid encoding tests";

  if passed_invalid_tests = List.length invalid_tests && passed_valid_tests = List.length valid_tests then
    print_endline "All tests passed"
  else
    print_endline "Some tests failed"

;;
run_all_tests ()
