open System.IO

type FSNode =
    | Directory of DirectoryNode
    | File of FileNode

and FileNode = { Name: string; Size: int; Parent: DirectoryNode option }
and DirectoryNode = { Name: string; Nodes: FSNode list; Parent: DirectoryNode option }

let ParseFSNode (line: string) =
    match (line.Split " ") with
    | x when x[0] = "dir" -> Directory { Name = x[1]; Nodes = []; Parent = None }
    | x -> File { Name = x[1]; Size = (x[0] |> int); Parent = None }

type OSCommand =
    | ListDirectory
    | UpDirectory
    | ChangeDirectory of string

let ParseCommand (c: string) =
    match (c.Split " ") with
    | x when x[1] = "ls" -> ListDirectory
    | x when x[1] = "cd" && x[2] = ".." -> UpDirectory
    | x when x[1] = "cd" -> ChangeDirectory x[2]

type Input = { Command: OSCommand; Output: string list }

let rec ParseInput (lines: string list) (input: Input list) (current: Input) =
    match lines with
    | x :: xs when x.StartsWith "$" ->
        // Start new command
        ParseInput xs (input @ [ current ]) { Command = ParseCommand x; Output = [] }
    | x :: xs -> ParseInput xs input { current with Output = (current.Output @ [ x ]) }
    | _ -> (input @ [ current ])

let rec BuildTreeFromInput (input: Input list) (current) =
    match input with
    | x :: xs ->
        match x.Command with
        | ListDirectory -> BuildTreeFromInput xs ({ current with Nodes = (x.Output |> List.map ParseFSNode) })
        | ChangeDirectory d ->
            ({ current with
                 Nodes =
                     (current.Nodes
                      |> List.map (fun f ->
                          match f with
                          | Directory n when n.Name = d -> Directory(BuildTreeFromInput xs { n with Parent = (Some current) })
                          | _ -> f))
             })
        | UpDirectory -> { current with Parent = Some (BuildTreeFromInput xs current.Parent.Value) }
    | _ -> current

let rec PrettyTree idt (root: DirectoryNode) =
    let indent =
        List.init idt (fun f -> "    ") |> List.fold (fun acc curr -> acc + curr) ""

    printfn "%s- %s (dir)" indent root.Name

    root.Nodes
    |> List.iter (fun f ->
        match f with
        | Directory d -> (PrettyTree (idt + 1) d)
        | File file -> ())

    root.Nodes
    |> List.iter (fun f ->
        match f with
        | Directory d -> ()
        | File file -> (printfn "%s- %s (file, size=%i)" ("    " + indent) file.Name file.Size))
// [ 0..idt ] |> List.iter (fun f -> printf "\t")


[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Tuning Trouble"

    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let initialCommand = { Command = ChangeDirectory "/"; Output = [] }

            let input =
                ParseInput (File.ReadAllLines argv[0] |> Array.skip 1 |> Array.toList) [] initialCommand

            let root =
                BuildTreeFromInput (input |> List.skip 1) { Name = "/"; Nodes = []; Parent = None }

            // printf "%A" input
            // printf "%A" root

            PrettyTree 0 root
            0
        | _ ->
            printfn "Did not find file!"
            1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
