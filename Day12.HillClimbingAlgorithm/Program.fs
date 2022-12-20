open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.InteropServices

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

    let exclude l1 l2 = List.filter (fun f -> (List.tryFind (fun g -> f = g) l2).IsNone) l1

    let rec SolveMapRec (visited: Node list) (unvisited: Node list) (state: DijkstraState list) (lastVisited: Node option) =
        printfn "%i / %i" (List.length visited) (List.length unvisited)
        // Take an element from unvisited node.
        match unvisited with
        | x :: xs ->
            // Calculate distance to connected nodes
            let currentDistance =
                match (state |> List.find (fun f -> f.Vertex = x))
                    .ShortestDistance
                    with
                | Some f -> f
                | None -> 0

            let availableToVisit = exclude x.Links visited

            let newState =
                state
                |> List.map (fun f ->
                    match ((availableToVisit) |> List.tryFind (fun g -> f.Vertex = g)) with
                    | Some n when f.ShortestDistance.IsNone || (f.ShortestDistance.Value > currentDistance) ->
                        { f with ShortestDistance = Some(currentDistance + 1); Previous = Some x }
                    | Some n -> { f with Previous = Some x }
                    | None -> f)

            let ToVisit2 =
                ((x.Links |> List.filter (fun f -> (List.tryFind (fun g -> f = g) visited).IsNone))
                 @ (List.filter (fun f -> (List.tryFind (fun g -> g = f) f.Links).IsSome) unvisited))
            // |> List.filter (fun f -> (List.tryFind (fun g -> f = g) visited).IsNone)

            let toVisit3 = (ToVisit2 @ (exclude xs ToVisit2))
            // SolveMapRec (x :: visited) (ToVisit2 @ (exclude xs ToVisit2)) (newState) (Some x)
            SolveMapRec (x :: visited) (toVisit3) (newState) (Some x)
        | [] -> state

    let output = SolveMapRec [] vertices initialState None
    output

[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Hill Climbing Algorithm"

    match Array.length argv with
    | x when x = 1 ->
        let input = ParseInput(File.ReadAllLines argv[0])
        let allNodes = input.Tiles |> Seq.cast<Node> |> Seq.toList
        let solved = SolveMapDijkstra input.Start input.End allNodes
        let shortest = solved |> List.find (fun f -> f.Vertex = input.End)
        let xx = solved |> List.sortByDescending (fun f -> f.ShortestDistance)

        printfn $"[* ] Shortest solution found uses %A{shortest.ShortestDistance} steps!"
        0
    | _ ->
        printfn "Did not find file!"
        1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
