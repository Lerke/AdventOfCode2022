open System.IO

[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Treetop Tree House"

    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            0
        | _ ->
            printfn "Did not find file!"
            1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1