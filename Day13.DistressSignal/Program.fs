
type PacketContent =
    | Number of int
    | NumberList of PacketContent list

let rec ParseLine (line: string list) =
    match line with
    | x::xs ->
        match x with
        | "[" -> NumberList [(ParseLine xs)]
        | _ -> Number 1
    | [] -> Number 1


[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Hill Climbing Algorithm"

    match Array.length argv with
    | x when x = 1 ->
        let line = "[1,1,3,1,1]"
        let parsedLine = ParseLine (line.Split "" |> Array.toList)
        0
    | _ ->
        printfn "Did not find file!"
        let a = NumberList [Number 1; Number 2]
        1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
