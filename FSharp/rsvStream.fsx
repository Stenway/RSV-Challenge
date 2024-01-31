module RsvStream =
    open System
    open System.IO
    let inline (|Byte|) n = byte n
    let utf8 = Text.UTF8Encoding(false, true)

    let decodeStream (reader:Stream) = 
        let getValue = 
            function
            | [254uy] -> null
            | chars -> chars |> List.toArray |> Array.rev |> utf8.GetString

        let rec readValue (values: string list) (chars: byte list) =
            match reader.ReadByte() with
            // No more bytes
            | -1 when values.IsEmpty -> None
            // No more bytes, but we're in the middle of a row
            | -1  -> failwith "Incomplete RSV file"
            // Row terminator
            | 253 when chars.IsEmpty -> Some (values |> List.toArray |> Array.rev, ())
            // Value terminator
            | 255 -> readValue ((getValue chars)::values) []
            // Row terminator encountered in the middle of a value
            | 253 -> failwith "Invalid RSV file"
            // Read bytes
            | Byte c -> readValue values (c::chars)

        try Array.unfold (fun _ -> readValue [] []) () |> Ok
        with exn -> Error exn.Message

    let encodeToStream (writer:Stream) (rows:string seq seq) = 
        let writeValue (value:string) =
            match value with
            | null -> writer.Write([|254uy|])
            | string -> utf8.GetBytes(string) |> writer.Write
            writer.WriteByte 255uy
        
        let writeRow values =
            values |> Seq.iter writeValue
            writer.WriteByte 253uy

        rows |> Seq.iter writeRow

    // ------------------------------------------------------------

    let decodeBytes (bytes: byte array) = 
        use ms = new MemoryStream(bytes)
        decodeStream ms

    let encodeSeq (rows: string seq seq) = 
        use ms = new MemoryStream()
        encodeToStream ms rows
        ms.ToArray()

    let encodeArray (rows: string array array) = 
        encodeSeq (rows|> Seq.map (Seq.map id))

    let encodeList (rows: string list list) = 
        encodeSeq (rows|> Seq.map (Seq.map id))

    // ------------------------------------------------------------

    let saveSeq path rows = 
        use stream = File.OpenWrite path
        encodeToStream stream rows
    
    let saveArray path (rows: string array array) = 
        saveSeq path (rows |> Seq.map (Seq.map id))

    let load path = 
        use stream = File.OpenRead path
        decodeStream stream

    // ------------------------------------------------------------

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

    let isValid (stream: Stream) = 
        let rec validate state idx = 
            match stream.ReadByte() with 
            | -1 -> if state = 1 then Ok "Valid" else Error "Incomplete RSV file"
            | Byte b -> 
                let currentByteClass = byteClassLookup[int b]
                let newStateLookupIndex = state * 15 + currentByteClass
                let state = stateTransitionLookup[newStateLookupIndex]
                if state = 0 then Error "Invalid RSV file"
                else validate state (idx + 1)
        validate 1 0

    // ------------------------------------------------------------

let testFile path = 
    let bytes = System.IO.File.ReadAllBytes $"{path}.rsv"
    use stream = System.IO.File.OpenRead $"{path}.rsv"
    let getStream() = 
        stream.Position <- 0
        stream
    RsvStream.isValid (getStream())
    |> Result.bind (fun _ -> RsvStream.decodeStream (getStream()))
    |> Result.bind (fun values -> 
        let correctValues = 
            System.IO.File.ReadAllBytes $"{path}.json"
            |> fun x -> System.Text.Json.JsonSerializer.Deserialize<string[][]>(x)
        if correctValues = values then Ok values
        else Error "Bad decoding"
        )
    |> Result.bind (fun values -> 
        let encoded = RsvStream.encodeArray values
        if encoded = bytes then Ok values
        else Error "Bad encoding")
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

System.IO.File.OpenRead(@"C:\Git\RSV-Challenge\TestFiles\Valid_001.rsv")
|> RsvStream.decodeStream

let rows = [|
    [|"Hello"; "🌎"; null; ""|]
    [|"A\0B\nC"; "Test 𝄞"|]
    [||]
    [|""|]
|] 

rows
|> RsvStream.encodeArray
|> RsvStream.decodeBytes
|> Result.map ((=) rows)

RsvStream.saveArray "Test.rsv" rows
let loaded = RsvStream.load "Test.rsv"
loaded |> Result.map ((=) rows)

RsvStream.load @"C:\Git\RSV-Challenge\TestFiles\Valid_001.rsv"

