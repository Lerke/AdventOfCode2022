open System
open System.IO
open System.Linq
open FSharp.Collections.ParallelSeq

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

    override this.GetHashCode() = $"{x}_{y}".GetHashCode()

    interface IComparable with
        member this.CompareTo(o) =
            match o with
            | :? Position as sc -> compare $"{x}_{y}" $"{sc.x}_{sc.y}"
            | _ -> -1

type Node(Position: Position, Links: Node list, Height: char) =
    member val Position = Position
    member val Links = Links with get, set
    member val Height = Height

    override this.Equals(o) =
        match o with
        | :? Node as p -> p.Position.x = Position.x && p.Position.y = Position.y

    override this.GetHashCode() = $"{Position.x}_{Position.y}".GetHashCode()

    interface IComparable with
        member this.CompareTo(o) =
            match o with
            | :? Node as sc -> compare Position sc.Position
            | _ -> -1

type DijkstraState(Vertex: Node, ShortestDistance: int option, Previous: Node option) =
    member val Vertex = Vertex
    member val ShortestDistance = ShortestDistance
    member val Previous = Previous
// { Vertex: Node; ShortestDistance: int option; Previous: Node option }

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
            | true -> DijkstraState(f, Some 0, None)
            | false -> DijkstraState(f, None, None))

    let rec solve (state: DijkstraState list) (stack: DijkstraState list) visited =
        match (List.sortBy (fun (f: DijkstraState) -> if f.ShortestDistance.IsSome then f.ShortestDistance.Value else Int32.MaxValue) stack) with
        | x :: xs ->
            let neighbors =
                x.Vertex.Links
                |> List.filter (fun f -> List.contains f (stack |> List.map (fun f -> f.Vertex)))

            let currentDistance =
                (List.find (fun (f: DijkstraState) -> f.Vertex = x.Vertex) state)
                    .ShortestDistance

            let newState =
                state
                |> List.map (fun f ->
                    match f with
                    | n when (List.contains n.Vertex neighbors) ->
                        match (n.ShortestDistance.IsNone || n.ShortestDistance < f.ShortestDistance)
                              && currentDistance.IsSome
                            with
                        | true -> DijkstraState(n.Vertex, Some(currentDistance.Value + 1), Some x.Vertex)
                        | false -> n
                    | n -> n)

            match (x.Vertex = endNode) with
            | true -> state
            | false -> solve newState (List.except visited newState) (x :: visited)
        | [] -> state

    solve initialState initialState []

[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Hill Climbing Algorithm"

    match Array.length argv with
    | x when x = 1 ->
        let input = ParseInput(File.ReadAllLines argv[0])
        let allNodes = input.Tiles |> Seq.cast<Node> |> Seq.toList
        let solved = SolveMapDijkstra input.Start input.End allNodes
        let shortest = solved |> List.find (fun f -> f.Vertex = input.End)

        printfn $"[* ] Shortest solution found uses %A{shortest.ShortestDistance} steps!"

        let allPaths = input.Tiles |> Seq.cast<Node> |> Seq.filter (fun f -> f.Height = 'a')

        let mutable count = 0
        let lockObj = new Object()

        let allSolves =
            allPaths
            |> PSeq.withDegreeOfParallelism 16
            |> PSeq.withExecutionMode ParallelExecutionMode.ForceParallelism
            |> PSeq.map (fun f ->
                let result = SolveMapDijkstra f input.End allNodes
                lock (lockObj) (fun _ ->
                    count <- count + 1
                    printfn $"Done {count}")
                result)
            |> PSeq.toList

        let allShortest =
            allSolves
            |> List.map (fun f -> f |> List.find (fun g -> g.Vertex = input.End))
            |> List.map (fun f -> f.ShortestDistance)
            |> List.sortBy (fun f -> if f.IsSome then f.Value else Int32.MaxValue)

        let shortestFromAll = List.head allShortest
        printfn $"[**] Shortest solution found uses %A{shortestFromAll} steps!"

        0
    | _ ->
        printfn "Did not find file!"
        1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
