open System.IO

let ItemValue (c: char) =
    ([ 0..64 ] // Padding
     @ [ 27..52 ] // Lowercase
       @ [ 0..5 ] // padding
         @ [ 1..26 ])[((c |> int))]

type Compartment(line: string) =
    member this.Items =
        line
        |> Seq.groupBy id
        |> Seq.fold (fun acc curr -> (acc |> Map.add (fst curr) (snd curr |> Seq.length))) (Map<char, int> [])

type Rucksack(compartments: (Compartment * Compartment)) =
    member this.Compartments = compartments

    member this.AllItems =
        Array.append ((fst compartments).Items |> Map.toArray) ((snd compartments).Items |> Map.toArray)
        |> Seq.groupBy (fun f -> fst f)
        |> Seq.fold (fun acc curr -> (acc |> Map.add (fst curr) (snd curr |> Seq.length))) (Map<char, int> [])

    member this.SharedItems =
        Map.toArray (fst compartments).Items
        |> Array.allPairs (Map.toArray (snd compartments).Items)
        |> Array.filter (fun f ->
            match f with
            | ((x, _), (y, _)) when x = y -> true
            | _ -> false)
        |> Array.map (fun f -> (f, (fst (fst f)) |> ItemValue))

let ParseRucksack (line: string) =
    [| line.Substring(0, line.Length / 2); line.Substring(line.Length / 2) |]
    |> Array.map (Compartment)
    |> fun f -> Rucksack((f[0], f[1]))

let FindSharedItemInRucksacks (rucksacks: Rucksack []) =
    rucksacks[0].AllItems.Keys
    |> Seq.find (fun f ->
        (Map.containsKey f rucksacks[1].AllItems)
        && (Map.containsKey f rucksacks[2].AllItems))

let ParsePuzzleInput file = file |> File.ReadAllLines |> Array.map ParseRucksack

[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Rucksack Reorganization"

    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let rucksacks = (ParsePuzzleInput argv[0])

            // Part one
            let sharedItemsPerRucksack = rucksacks |> Array.collect (fun f -> f.SharedItems)
            let totalSumSharedItems = sharedItemsPerRucksack |> Array.sumBy (snd)
            printfn "[*] Total sum shared items: %i" totalSumSharedItems

            // Part two
            let elfGroups = rucksacks |> Array.chunkBySize 3
            let elfGroupsWithBadges = elfGroups |> Array.map (FindSharedItemInRucksacks)
            let totalSumBadges = elfGroupsWithBadges |> Array.sumBy (ItemValue)
            printfn "[**] Total sum shared badges: %i" totalSumBadges
            0
        | _ ->
            printfn "Did not find file!"
            1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
