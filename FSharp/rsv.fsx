module Rsv =
    open System
    open System.IO
    
    let private splitArray separator postProcessor source = 
        Array.unfold (fun position -> 
            match Array.IndexOf(source, separator, position) with
            | p when p >= source.Length -> None
            | -1 when position = source.Length -> None
            | -1 -> failwith $"Incomplete RSV data - missing %2X{separator}"
            | i -> Some (source[position..i-1] |> postProcessor, i+1)
        ) 0

    let utf8 = Text.UTF8Encoding(false, true)
    let private decodeValue = function
        | [|254uy|] -> null
        | value -> utf8.GetString value

    let encodeValue (value:string) =
        [|
            match value with
            | null -> 254uy
            | _    -> yield! utf8.GetBytes(value)
            255uy
        |]

    let private decodeRow = 
        splitArray 255uy decodeValue

    let private encodeRow (values : string array) = 
        [|
            yield! Array.collect encodeValue values
            253uy
        |]

    let decode bytes = 
        try splitArray 253uy decodeRow bytes |> Ok
        with exn -> Error exn.Message

    let encode (rows:string array array) = Array.collect encodeRow rows

    // ----------------------------------------------------------------------

    let save path rows = File.WriteAllBytes (path, encode rows)
    let load path = File.ReadAllBytes(path) |> decode

    // ---------------------------------------------------------------------

    let byteClassLookup = [|
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
        2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
        3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
        4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
        4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
        0; 0; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
        5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
        6; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 8; 7; 7;
        9; 10; 10; 10; 11; 0; 0; 0; 0; 0; 0; 0; 0; 12; 13; 14
        |]
    let stateTransitionLookup = [|
        0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 2; 0; 0; 0; 3; 4; 6; 5; 7; 8; 9; 1; 10; 11;
        0; 2; 0; 0; 0; 3; 4; 6; 5; 7; 8; 9; 0; 0; 11;
        0; 0; 2; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 3; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 6; 6; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 6; 6; 6; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 11;
        0; 2; 0; 0; 0; 3; 4; 6; 5; 7; 8; 9; 1; 10; 11
    |]

    let isValid (bytes: byte array) = 
        let rec validate state idx = 
            if idx >= bytes.Length then
                if state = 1 then Ok bytes else Error "Incomplete RSV file"
            else
                let currentByteClass = byteClassLookup[int bytes[idx]]
                let newStateLookupIndex = state * 15 + currentByteClass
                let state = stateTransitionLookup[newStateLookupIndex]
                if state = 0 then Error "Invalid RSV file"
                else validate state (idx + 1)
        validate 1 0

let toJson (values:string array array) = 
    System.Text.Json.JsonSerializer.Serialize(values)


let testFile path = 
    let bytes = System.IO.File.ReadAllBytes $"{path}.rsv"
    bytes
    |> Rsv.isValid
    |> Result.bind Rsv.decode
    |> Result.bind (fun values -> 
        let correctValues = 
            System.IO.File.ReadAllBytes $"{path}.json"
            |> fun x -> System.Text.Json.JsonSerializer.Deserialize<string[][]>(x)
        if correctValues = values then Result.Ok values
        else Result.Error "Bad decoding"
        )
    |> Result.bind (fun values -> 
        if Rsv.encode values = bytes then Result.Ok values
        else Result.Error "Bad encoding")
    |> Result.map (fun _ -> $"Test succeeded: {path}")
    |> Result.mapError (fun e -> $"Test failed: {e}: {path}")
    
[<Literal>]
let basePath = __SOURCE_DIRECTORY__

[1..79]
|> List.map (fun i -> $"{basePath}\\..\\TestFiles\\Valid_%03i{i}")
|> List.map testFile
|> List.forall (function Ok _ -> true | Error _ -> false)

[1..29]
|> List.map (fun i -> $"{basePath}\\..\\TestFiles\\Invalid_%03i{i}")
|> List.map testFile
|> List.forall (function Ok _ -> false | Error _ -> true)

let rows = [|
    [|"Hello"; "🌎"; null; ""|]
    [|"A\0B\nC"; "Test 𝄞"|]
    [||]
    [|""|]
|]

rows
|> Rsv.encode
|> Rsv.decode
|> Result.map ((=) rows)

Rsv.save "Test.rsv" rows
let loaded = Rsv.load "Test.rsv"
loaded |> Result.map ((=) rows)



