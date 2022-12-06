open System.IO

let rec GetFirstUniqueSubstring packetLength index (state: char list) (line: char list) =
  match line with
  | x :: xs ->
    match state with
    | _ :: q when ((List.distinct state).Length = packetLength) -> (index, q @ [ x ])
    | s when s.Length < packetLength -> GetFirstUniqueSubstring packetLength (index + 1) (s @ [ x ]) xs
    | s when s.Length = packetLength -> GetFirstUniqueSubstring packetLength (index + 1) ((List.skip 1 s) @ [ x ]) xs
  | _ -> (index, state)

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Tuning Trouble"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let [ startOfPacket; startOfMessage ] =
        [ 4; 14 ]
        |> List.map (fun f -> GetFirstUniqueSubstring f 0 [] ((File.ReadAllText argv[0]) |> Seq.toList))

      [ ("* ", startOfPacket)
        ("**", startOfMessage) ]
      |> List.iter (fun f ->
        printfn
          $"[%s{fst f}] Start of first packet: %i{fst (snd f)} - %s{(snd (snd f))
                                                                    |> List.map string
                                                                    |> (List.reduce (+))}")

      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
