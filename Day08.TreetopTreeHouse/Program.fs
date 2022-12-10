open System
open System.IO
open Microsoft.FSharp.Control.TaskBuilderExtensions

type Position(x: int, y: int) =
    member this.x = x
    member this.y = y
    static member Left = Position(-1, 0)
    static member Right = Position(1, 0)
    static member Up = Position(0, 1)
    static member Down = Position(0, -1)
    static member (+)(a: Position, b: Position) = Position(a.x + b.x, a.y + b.y)

type Tree(position: Position, height: int) =
    member val position = position
    member val height = height

    member this.VisibleFromDirection direction (map: TreeMap) h score =
        let np = this.position + direction

        match np with
        | p when p.x < 0 || p.y < 0 -> (true, score)
        | p when p.x > ((Array.length map.Map) - 1) || p.y > ((Array.length map.Map) - 1) -> (true, score)
        | _ ->
            match map.AtPosition(np) with
            | q when q.height < h -> q.VisibleFromDirection direction map h (score + 1)
            | _ -> (false, score + 1)

    member this.DirectionsVisibleFrom map =
        [ Position.Left; Position.Up; Position.Down; Position.Right ]
        |> List.map (fun f -> this.VisibleFromDirection f map height 0)
        |> List.filter (fun f -> (fst f) = true)
        |> List.length

    member this.VisibilityScore map =
        [ Position.Left; Position.Up; Position.Down; Position.Right ]
        |> List.map (fun f -> this.VisibleFromDirection f map height 0)
        |> ((List.fold (fun acc curr -> acc * (snd curr)) 1))

and TreeMap(map: Tree [] []) =
    member val Map = map

    member this.AtPosition(position: Position) : Tree = this.Map.[position.y].[position.x]

    member this.Print() =
        this.Map
        |> Array.iter (fun f -> printfn "%s" ((f |> (Array.fold (fun acc curr -> acc + (curr.height |> string)) ""))))

    member this.VisibilityMap =
        this.Map
        |> Array.map (fun f -> f |> Array.map (fun q -> (q.DirectionsVisibleFrom this)))

    member this.ScenicScoreMap =
        this.Map
        |> Array.map (fun f -> f |> Array.map (fun q -> (q.VisibilityScore this)))

    member this.PrintVisibilityMap() =
        printfn "Visibility view"

        this.VisibilityMap
        |> Array.iter (fun f -> printfn "%s" ((f |> (Array.fold (fun acc curr -> acc + (curr |> string)) ""))))

    member this.PrintScenicScoreMap() =
        printfn "Scenic view"

        this.ScenicScoreMap
        |> Array.iter (fun f -> printfn "%s" ((f |> (Array.fold (fun acc curr -> acc + (curr |> string)) ""))))


[<EntryPoint>]
let main argv =
    printfn "* Advent of Code 2022 - Treetop Tree House"

    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let trees =
                TreeMap(
                    (File.ReadAllLines argv[0])
                    |> Array.mapi (fun y f ->
                        (f
                         |> seq
                         |> Seq.mapi (fun x q -> Tree(Position(x, y), (Convert.ToInt32(q |> string))))
                         |> Seq.toArray))
                )

            printfn ""
            trees.Print()
            printfn ""
            trees.PrintVisibilityMap()
            printfn ""
            trees.PrintScenicScoreMap()
            printfn ""

            let visibleTrees =
                trees.VisibilityMap
                |> (Array.collect id)
                |> (Array.filter (fun f -> f > 0))
                |> Array.length

            printfn "[* ] Number of trees visible: %i" visibleTrees
            
            let highestScenicScore =
                trees.ScenicScoreMap
                |> (Array.collect id)
                |> Array.max
            
            printfn "[**] Highest scenic score possible for tree in this map: %i" highestScenicScore

            0
        | _ ->
            printfn "Did not find file!"
            1
    | _ ->
        printfn "Usage: dotnet <path-to-input>"
        1
