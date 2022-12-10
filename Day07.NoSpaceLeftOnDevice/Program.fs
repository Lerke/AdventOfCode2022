open System.IO

type FSNode =
    | Directory of DirectoryNode
    | File of FileNode

and FileNode(name: string, size: int, parent: FSNode option) =
    member val Name = name
    member val Size = size
    member val Parent = parent

and DirectoryNode(name: string, nodes: FSNode list, parent: FSNode option) =
    member val Name = name
    member val Nodes = nodes with get, set
    member val Parent = parent

    member this.Size() =
        this.Nodes
        |> List.map (fun f ->
            match f with
            | Directory directoryNode -> directoryNode.Size()
            | File file -> file.Size)
        |> List.sum

let ParseFSNode (parent: DirectoryNode) (line: string) =
    match (line.Split " ") with
    | x when x[0] = "dir" -> Directory(DirectoryNode(x[1], [], Some(Directory parent)))
    | x -> File(FileNode(x[1], (x[0] |> int), Some(Directory parent)))

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

let rec BuildTreeFromInput (input: Input list) (current: DirectoryNode) =
    match input with
    | x :: xs ->
        match x.Command with
        | UpDirectory ->
            match current.Parent.Value with
            | Directory d -> BuildTreeFromInput xs d
        | ListDirectory ->
            current.Nodes <- (x.Output |> List.map (ParseFSNode current))
            BuildTreeFromInput xs current
        | ChangeDirectory d ->
            let destination =
                (current.Nodes)
                |> (List.find (fun f ->
                    match f with
                    | Directory dd when dd.Name = d -> true
                    | _ -> false))

            match destination with
            | Directory directoryNode -> BuildTreeFromInput xs directoryNode
    | _ ->
        match current.Parent with
        | Some x ->
            BuildTreeFromInput
                []
                (match x with
                 | Directory dd -> dd)
        | None -> current

let rec CollectDirectories fn (root: DirectoryNode) =
    List.append
        (root.Nodes
         |> List.collect (fun f ->
             match f with
             | Directory d when (fn d) -> [ d ]
             | _ -> []))
        (root.Nodes
         |> List.collect (fun f ->
             match f with
             | Directory d -> CollectDirectories fn d
             | _ -> []))

let rec PrettyTree idt (root: DirectoryNode) =
    let indent =
        List.init idt (fun f -> "    ") |> List.fold (fun acc curr -> acc + curr) ""

    printfn "%s- %s (dir, total_size=%i)" indent root.Name (root.Size())

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


[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - No Space Left On Device"

    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let initialCommand = { Command = ChangeDirectory "/"; Output = [] }

            let input =
                ParseInput (File.ReadAllLines argv[0] |> Array.skip 1 |> Array.toList) [] initialCommand

            let rootNode = (DirectoryNode("/", [], None))
            let root = BuildTreeFromInput (input |> List.skip 1) (rootNode)
            let largeDirs = root |> (CollectDirectories(fun f -> f.Size() <= 100000))

            PrettyTree 0 root
            printfn "[*] Directories smaller than 100000 sum: %i" (largeDirs |> List.sumBy (fun d -> d.Size()))

            let totalDiskSpace = 70000000
            let updateRequiredDiskSpace = 30000000

            let unusedSpace = (totalDiskSpace - root.Size())
            let necessaryToRemove = (abs (unusedSpace - updateRequiredDiskSpace))

            let removeCandidates =
                (root
                 |> (CollectDirectories(fun d -> d.Size() >= necessaryToRemove))
                 |> List.sortBy (fun f -> f.Size()))
                @ [ root ]

            let toRemove = (List.head removeCandidates)

            printfn
                "[**] In order to get enough space, we would have to remove the smallest possible directory %s - which has a total size of: %i"
                toRemove.Name
                (toRemove.Size())

            0
        | _ ->
            printfn "Did not find file!"
            1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
