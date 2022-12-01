open System.IO

let InputAccumulator (acc: string list list) curr =
  match curr with
  | "" -> List.append acc [[]]
  | f -> match List.rev acc with
         | x::xs -> List.append xs [(List.append x [f])]

let CalculateCaloriesPerElf (input: string list) =
  input
  |> List.map(int)
  |> List.sum
  
let ReadPuzzleInput file =
  file
  |> File.ReadAllLines
  |> Array.fold InputAccumulator [[]]
  |> List.map CalculateCaloriesPerElf

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Calorie Counting"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let totalCaloriesPerElf = ReadPuzzleInput argv[0] |> List.sortDescending
      
      printfn "Max calories for given elves: %i" (totalCaloriesPerElf |> List.head)
      printfn "Total calories top three elves: %i" (totalCaloriesPerElf |> List.take 3 |> List.sum)
      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
