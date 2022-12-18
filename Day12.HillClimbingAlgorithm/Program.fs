open System
open System.IO

type Position(x: int, y: int) =
    member val x = x
    member val y = y
    static member Origin = Position(0, 0)
    static member Left = Position(-1, 0)
    static member Right = Position(1, 0)
    static member Up = Position(0, 1)
    static member Down = Position(0, -1)
    static member (+)(a: Position, b: Position) = Position(a.x + b.x, a.y + b.y)
    static member (=)(a: Position, b: Position) = Position(a.x + b.x, a.y + b.y)

    override this.Equals(o) =
        match o with
        | :? Position as p -> p.x = x && p.y = y

type Node(Position: Position, Links: Node list, Height: char) =
    member val Position = Position
    member val Links = Links with get, set
    member val Height = Height

type DijkstraState = { Vertex: Node; ShortestDistance: int option; Previous: Node option }

type HeightMap = { Tiles: Node [,]; Start: Node; End: Node; Current: Node }

let ParseInput (lines: string []) =
    let mutable startPosition = Position(0, 0)
    let mutable endPosition = Position(0, 0)

    let tiles =
        Array2D.init (lines.Length) (lines[0].Length) (fun x y ->
            match lines[x].[y] with
            | 'S' ->
                startPosition <- Position(y, x)
                Node(Position(y, x), [], 'a')
            | 'E' ->
                endPosition <- Position(y, x)
                Node(Position(y, x), [], 'z')
            | c -> Node(Position(y, x), [], c))


    tiles
    |> Array2D.iteri (fun i j v ->
        v.Links <-
            ([
                v.Position + Position.Up
                v.Position + Position.Down
                v.Position + Position.Left
                v.Position + Position.Right
            ])
            |> List.map (fun f ->
                match (f.x, f.y) with
                | (x, y) when x < 0 || y < 0 || (x >= tiles[0, *].Length) || (y >= tiles[*, 0].Length) -> None
                | _ -> Some(tiles[f.y, f.x]))
            |> List.filter (fun f -> f.IsSome && ((f.Value.Height |> int) <= ((tiles[i, j].Height |> int) + 1)))
            |> List.map (fun f -> f.Value))

    {
        Tiles = tiles
        Start = tiles[startPosition.y, startPosition.x]
        End = tiles[endPosition.y, endPosition.x]
        Current = tiles[startPosition.y, startPosition.x]
    }

let rec SolveMap (current: Node) (endNode: Node) (state: Node list) =
    match (current.Position.x = endNode.Position.x
           && current.Position.y = endNode.Position.y)
        with
    | true -> [ (state, current) ]
    | false ->
        match (List.exists (fun (n: Node) -> n.Position.x = current.Position.x && n.Position.y = current.Position.y) state) with
        | true -> []
        | false ->
            match (current.Links) with
            | [] -> []
            | x when x.Length > 0 -> x |> List.collect (fun f -> SolveMap f endNode (current :: state))

let SolveMapDijkstra (startNode: Node) (endNode: Node) (vertices: Node list) =
    let initialState =
        vertices
        |> List.map (fun f ->
            match (f = startNode) with
            | true -> { Vertex = f; ShortestDistance = Some 0; Previous = None }
            | false -> { Vertex = f; ShortestDistance = None; Previous = None })

    let rec SolveMapRec (visited: Node list) (unvisited: Node list) (state: DijkstraState list) =
        // Take an element from unvisited node.
        match unvisited with
        | x :: xs ->
            // Calculate distance to connected nodes
            let currentDistance =
                (state |> List.find (fun f -> f.Vertex = x))
                    .ShortestDistance

            let newState =
                state
                |> List.map (fun f ->
                    match (x.Links |> List.tryFind (fun g -> f.Vertex = g)) with
                    | Some n when
                        f.ShortestDistance.IsSome
                        && currentDistance.IsSome
                        && f.ShortestDistance.Value > currentDistance.Value
                        ->
                        { f with ShortestDistance = Some(currentDistance.Value + 1) }
                    | Some n -> f
                    | None when currentDistance.IsSome -> { f with ShortestDistance = Some(currentDistance.Value) }
                    | None -> f)

            // If distance is shorter than present, update state
            let toVsit =
                (unvisited |> (List.map (fun f -> List.find (fun g -> f = g.Vertex) state)))
                |> List.filter (fun f -> not (List.contains f.Vertex visited))
                |> List.sortBy (fun f -> match f.ShortestDistance with
                                          | Some n -> n
                                          | None -> Int32.MaxValue)
                |> List.map (fun f -> f.Vertex)
            SolveMapRec (x :: visited) (toVsit) (newState)
        | [] -> state

    let output = SolveMapRec [] vertices initialState
    output

[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Hill Climbing Algorithm"

    match Array.length argv with
    | x when x = 1 ->
        let input = ParseInput(File.ReadAllLines argv[0])
        let allNodes = input.Tiles |> Seq.cast<Node> |> Seq.toList
        let solved = SolveMapDijkstra input.Start input.End allNodes

        // printfn $"[* ] Shortest solution found uses %i{fst(solved[0]).Length} steps!"
        0
    | _ ->
        printfn "Did not find file!"
        1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
