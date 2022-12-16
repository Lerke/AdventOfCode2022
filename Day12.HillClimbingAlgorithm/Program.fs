open System.IO

type Node = { x: int; y: int; Links: Node list }

type Position(x: int, y: int) =
  member val x = x
  member val y = y
  static member Origin = Position(0, 0)
  static member Left = Position(-1, 0)
  static member Right = Position(1, 0)
  static member Up = Position(0, 1)
  static member Down = Position(0, -1)
  static member (+)(a: Position, b: Position) = Position(a.x + b.x, a.y + b.y)

type HeightMap =
  { Tiles: char[,]
    Start: Position
    End: Position
    Current: Position }

  member this.GetTile(p: Position) =
    match (p.x, p.y) with
    | (x, y) when
      x < 0
      || y < 0
      || (x >= this.Tiles[0, *].Length)
      || (y >= this.Tiles[*, 0].Length)
      ->
      None
    | _ -> Some(this.Tiles[p.y, p.x])

  member this.ValidMoves() =
    [| (Position.Left); (Position.Up); (Position.Down); (Position.Right) |]
    |> Array.map (fun f -> (this.GetTile(this.Current + f), f))
    |> Array.filter (fun f ->
      (fst f).IsSome
      && (((fst f).Value |> int)
          <= ((this.Tiles[this.Current.y, this.Current.x] |> int) + 1)))

  member this.Move(direction: Position) =
    { this with Current = (this.Current + direction) }

let ParseInput (lines: string[]) =
  let mutable startPosition = Position(0, 0)
  let mutable endPosition = Position(0, 0)

  let tiles =
    Array2D.init (lines.Length) (lines[0].Length) (fun x y ->
      match lines[x][y] with
      | 'S' ->
        startPosition <- Position(y, x)
        'a'
      | 'E' ->
        endPosition <- Position(y, x)
        'z'
      | x -> x)

  { Tiles = tiles
    Current = startPosition
    Start = startPosition
    End = endPosition }

let mutable ShortestImplementation = 9999

let rec SolveMap (h: HeightMap) (state: Position list) =
  match (h.Current.x = h.End.x && h.Current.y = h.End.y) with
  | true ->
    match state.Length < ShortestImplementation with
    | true ->
      ShortestImplementation <- state.Length
      [ (state, h) ]
    | false -> []
  | false ->
    match (List.exists (fun (p: Position) -> p.x = h.Current.x && p.y = h.Current.y) state) with
    | true -> []
    | false ->
      match (h.ValidMoves()) with
      | [||] -> [ (state, h) ]
      | x when x.Length > 0 ->
        match state.Length < ShortestImplementation with
        | true -> ((List.collect (fun f -> SolveMap (h.Move((snd f))) (h.Current :: state)) (x |> Array.toList)))
        | false -> []

let rec CreateNodeGraph map nodes = 0


[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Hill Climbing Algorithm"

  match Array.length argv with
  | x when x = 1 ->
    let input = ParseInput(File.ReadAllLines argv[0])
    let solved = (SolveMap input []) |> List.sortBy (fun f -> (fst f).Length)

    printfn $"[* ] Shortest solution found uses %i{fst(solved[0]).Length} steps!"
    0
  | _ ->
    printfn "Did not find file!"
    1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
