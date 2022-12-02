open System.IO

type Move =
  | Rock
  | Paper
  | Scissors
  member this.Value() =
    match this with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
    
  member this.WinsAgainst() =
    match this with
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper
  
  member this.LosesTo() =
    match this with
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

type MatchResult =
  | Win
  | Loss
  | Draw
  member this.Value() =
    match this with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

type Round =
  | MoveMove of Move * Move
  | MoveDesiredOutcome of Move * MatchResult

let CalculateMatchResult round =
  match round with
  | MoveMove (x, y) when y = x.LosesTo() -> Win
  | MoveMove (x, y) when y = x.WinsAgainst() -> Loss
  | _ -> Draw

let CalculateRoundScore round =
  match round with
  | MoveMove (a: Move, b: Move), (result: MatchResult) -> b.Value() + result.Value()

let CalculateDesiredRoundScore round =
  match round with
  | MoveDesiredOutcome (opponentMove: Move, desiredResult: MatchResult), (ownMove: Move) -> desiredResult.Value() + ownMove.Value()

let Move x =
  match x with
  | "A" -> Rock
  | "X" -> Rock
  | "B" -> Paper
  | "Y" -> Paper
  | "C" -> Scissors
  | "Z" -> Scissors

let DesiredResult x =
  match x with
  | "X" -> Loss
  | "Y" -> Draw
  | "Z" -> Win

let CalculateLineResult (line: string) =
  match line.Split [| ' ' |] with
  | [| x; y |] -> MoveMove(Move x, Move y)
  | x -> failwithf $"Faulty input: %A{x}"

let CalculateLineDesiredOutcome (line: string) =
  match line.Split [| ' ' |] with
  | [| x; y |] -> MoveDesiredOutcome(Move x, DesiredResult y)

let ReadPuzzleInputPartOne file =
  file
  |> File.ReadAllLines
  |> Array.map CalculateLineResult
  |> fun f -> Array.zip f (f |> Array.map CalculateMatchResult)
  |> fun f -> Array.zip f (f |> Array.map CalculateRoundScore)

let DecideMoveForOutcome move =
  match move with
  | MoveDesiredOutcome(move, Win) -> move.LosesTo()
  | MoveDesiredOutcome(move, Loss) -> move.WinsAgainst()
  | MoveDesiredOutcome (move, Draw) -> move

let ReadPuzzleInputPartTwo file =
  file
  |> File.ReadAllLines
  |> Array.map CalculateLineDesiredOutcome
  |> fun f -> Array.zip f (f |> Array.map DecideMoveForOutcome)
  |> fun f -> Array.zip f (f |> Array.map CalculateDesiredRoundScore)

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Rock Paper Scissors"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      printfn
        "Total score: %i"
        ((ReadPuzzleInputPartOne argv[0])
         |> Array.sumBy (fun f -> snd f))

      printfn
        "Part two total score: %i"
        ((ReadPuzzleInputPartTwo argv[0])
         |> Array.sumBy (fun f -> snd f))

      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
