open System
open System.IO

type Input = Position

and Position(x: int, y: int) =
  member val x = x
  member val y = y
  static member Left = Position(-1, 0)
  static member Right = Position(1, 0)
  static member Up = Position(0, 1)
  static member Down = Position(0, -1)
  static member (+)(a: Position, b: Position) = Position(a.x + b.x, a.y + b.y)

  static member (-->)(a: Position, b: Position) =
    let nx =
      match (b.x - a.x) with
      | nx when nx <> 0 -> Position(nx, 0)
      | _ -> Position(0, 0)

    let ny =
      match (b.y - a.y) with
      | ny when ny <> 0 -> Position(0, ny)
      | _ -> Position(0, 0)

    match (floor (sqrt ((float (pown (abs nx.x) 2)) + (float (pown (abs ny.y) 2))))) with
    | f when (int f) > 1 -> Position(Math.Clamp(nx.x, -1, 1), 0) + Position(0, Math.Clamp(ny.y, -1, 1))
    | _ -> Position(0, 0)

type Rope =
  { Head: Position
    Tail: Position
    TailPositions: Position list }

  member this.Move(direction: Position) =
    let newHead = this.Head + direction
    let newTail = this.Tail + (this.Tail --> newHead)
    let tailPositions = this.TailPositions @ [ this.Tail ]

    { Head = newHead
      Tail = newTail
      TailPositions = tailPositions }

  member this.TailPositionsVisited() =
    (this.Tail :: this.TailPositions)
    |> List.distinctBy (fun f -> (f.x, f.y))
    |> List.length

let rec ApplyRopeInput (rope: Rope) (input: Input list) (state: Rope list) =
   match input with 
   | [] -> [ rope ]
   | x :: xs -> (ApplyRopeInput (rope.Move x) xs (state @ [rope]))

let rec ParseInputCommands (lines: string list) =
  match lines with
  | x :: xs ->
    (match (x.Split [| ' ' |]) with
     | [| "R"; x |] -> [ 1 .. (x |> int) ] |> (List.map (fun _ -> Position(1, 0)))
     | [| "L"; x |] -> [ 1 .. (x |> int) ] |> (List.map (fun _ -> Position(-1, 0)))
     | [| "U"; x |] -> [ 1 .. (x |> int) ] |> (List.map (fun _ -> Position(0, -1)))
     | [| "D"; x |] -> [ 1 .. (x |> int) ] |> (List.map (fun _ -> Position(0, 1))))
    @ ParseInputCommands xs
  | _ -> []

let PrettyPrintState (rope: Rope) =
  let maxPos =
    ([ rope.Head; rope.Tail ] @ rope.TailPositions)
    |> List.maxBy (fun f -> max f.x f.y)

  let dimensions = (max maxPos.x maxPos.y) + 1

  let grid =
    Array2D.init dimensions dimensions (fun x y ->
      match rope with
      | r when r.Head.x = x && r.Head.y = y -> 'H'
      | r when r.Tail.x = x && r.Tail.y = y -> 'T'
      | _ when x = 0 && y = 0 -> 's'
      | r when (r.TailPositions |> (List.tryFind (fun f -> f.x = x && f.y = y))).IsSome -> '#'
      | _ -> '.')

  [| 0 .. (dimensions - 1) |]
  |> Array.map (fun f -> (grid[*, f] |> Array.fold (fun acc curr -> acc + (curr |> string)) ""))
  |> Array.iter (fun f -> printfn $"%s{f}")

  printfn ""

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Rope Bridge"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let initialState =
        { Head = Position(0, 4)
          Tail = Position(0, 4)
          TailPositions = [] }

      let inputCommands = ParseInputCommands(File.ReadAllLines argv[0] |> Array.toList)
      let moved = ApplyRopeInput initialState inputCommands []

      // moved |> List.iter (PrettyPrintState)

      printfn $"[* ] Total tail positions visited: %i{(List.last moved).TailPositionsVisited()}"
      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
