open System.IO

type ElfAssignment(line: string) =
  member this.Range =
    line.Split [| '-' |]
    |> fun f -> [ (f[0] |> int) .. (f[1] |> int) ]

  member this.FullyOverlaps(other: ElfAssignment) =
    List.forall (fun f -> (List.contains f other.Range)) this.Range

  member this.PartialOverlap(other: ElfAssignment) =
    List.exists (fun f -> (List.contains f other.Range)) this.Range

type ElfPair(line: string) =
  member this.Assignments =
    line.Split [| ',' |]
    |> fun f ->
         [ (ElfAssignment f[0])
           (ElfAssignment f[1]) ]

  member this.HasPartialOverlap =
    match this.Assignments[0].Range.Length
          <= (this.Assignments[1].Range.Length)
      with
    | true ->
      this
        .Assignments[ 0 ]
        .PartialOverlap this.Assignments[1]
    | false ->
      this
        .Assignments[ 1 ]
        .PartialOverlap this.Assignments[0]

  member this.HasFullOverlap =
    match this.Assignments[0].Range.Length
          <= (this.Assignments[1].Range.Length)
      with
    | true ->
      this
        .Assignments[ 0 ]
        .FullyOverlaps this.Assignments[1]
    | false ->
      this
        .Assignments[ 1 ]
        .FullyOverlaps this.Assignments[0]

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Camp Cleanup"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let inputPairs =
        argv[0] |> File.ReadAllLines |> Array.map ElfPair

      let overlappingPairs =
        inputPairs
        |> Array.filter (fun f -> f.HasFullOverlap)

      printfn "[*] Number of overlapping pairs: %i" overlappingPairs.Length

      let partialOverlappingPairs =
        inputPairs
        |> Array.filter (fun f -> f.HasPartialOverlap)

      printfn "[**] Number of partial overlapping pairs: %i" partialOverlappingPairs.Length
      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
