open System
open System.IO

type Input = Position

and Position(x: int, y: int) =
  member val x = x
  member val y = y
  static member Origin = Position(0, 0)
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
    Tail: Rope option
    PositionsVisited: Position list }

  member this.Move(direction: Position) =
    let newHead = this.Head + direction

    let newTail =
      match this.Tail with
      | Some t -> Some(t.Move(t.Head --> newHead))
      | None -> None

    { Head = newHead
      Tail = newTail
      PositionsVisited = this.PositionsVisited @ [ newHead ] }

  member this.TailPositionsVisited() =
    (this.Head :: this.PositionsVisited)
    |> List.distinctBy (fun f -> (f.x, f.y))
    |> List.length

let rec ApplyRopeInput (rope: Rope) (input: Input list) (state: Rope list) =
  match input with
  | [] -> [ rope ]
  | x :: xs -> (ApplyRopeInput (rope.Move x) xs (state @ [ rope ]))

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

let rec BuildRope knots =
  match knots with
  | k when k > 0 ->
    { Head = Position.Origin
      Tail = Some(BuildRope(k - 1))
      PositionsVisited = [] }
  | _ ->
    { Head = Position.Origin
      Tail = None
      PositionsVisited = [] }

let rec UnwrapRopes (rope: Rope option) =
  match rope with
  | Some r -> r :: (UnwrapRopes r.Tail)
  | _ -> []

let PrettyPrintState (rope: Rope) =
  let maxPos = Position(5, 5)
  let allropes = UnwrapRopes(Some rope)

  let dimensions = (max maxPos.x maxPos.y) + 1

  let grid =
    Array2D.init dimensions dimensions (fun x y ->
      match (x, y) with
      | f when (allropes |> List.tryFind (fun f -> f.Head.x = x && f.Head.y = y)).IsSome -> 'H'
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
      let smallRope = BuildRope 1

      let inputCommands = ParseInputCommands(File.ReadAllLines argv[0] |> Array.toList)
      let moved = UnwrapRopes (Some (List.last (ApplyRopeInput smallRope inputCommands [])))
      printfn $"[* ] Total tail positions visited: %i{(List.last moved).TailPositionsVisited()}"

      let bigRope = BuildRope 9
      let movedBigRope = UnwrapRopes (Some (List.last (ApplyRopeInput bigRope inputCommands [])))
      printfn $"[**] Total tail positions visited: %i{(List.last movedBigRope).TailPositionsVisited()}"
      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
