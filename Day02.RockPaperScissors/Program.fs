﻿open System.IO

type Move =
  | Rock
  | Paper
  | Scissors
  member this.Value() =
    match this with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

type MatchResult =
  | Win
  | Loss
  | Draw
  member this.Value() =
    match this with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

type Round = Move * Move

let CalculateMatchResult round =
  match round with
  | Rock, Scissors -> Loss
  | Paper, Rock -> Loss
  | Scissors, Paper -> Loss
  | x, y when x = y -> Draw
  | _ -> Win

let CalculateRoundScore round =
  match round with
  | (a: Move, b: Move), (result: MatchResult) -> b.Value() + result.Value()

let Move x =
  match x with
  | "A" -> Rock
  | "X" -> Rock
  | "B" -> Paper
  | "Y" -> Paper
  | "C" -> Scissors
  | "Z" -> Scissors

let CalculateLineResult (line: string) =
  match line.Split [| ' ' |] with
  | [| x; y |] -> Round(Move x, Move y)
  | x -> failwithf $"Faulty input: %A{x}"

let ReadPuzzleInput file =
  file
  |> File.ReadAllLines
  |> Array.map CalculateLineResult
  |> fun f -> Array.zip f (f |> Array.map CalculateMatchResult)
  |> fun f -> Array.zip f (f |> Array.map CalculateRoundScore)

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Calorie Counting"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let matchResults = ReadPuzzleInput argv[0]

      let totalScore =
        matchResults |> Array.sumBy (fun f -> snd f)

      printfn "Total score: %i" totalScore
      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
