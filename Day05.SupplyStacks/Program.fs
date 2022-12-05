namespace Day05

open System.IO
open System.Text.RegularExpressions
open Day05.Stack

module Program =

  type Container = { value: char }
  type ContainerStack = { Containers: Container stack }
  type ContainerMove = { Amount: int; From: int; To: int }

  type ContainerStorage =
    { Stacks: ContainerStack [] }

    member this.MoveCrate move =
      [| 0 .. (move.Amount - 1) |]
      |> Array.iter (fun _ ->
        let from =
          pop this.Stacks[move.From - 1].Containers

        let too =
          push (from |> peek) this.Stacks[move.To - 1].Containers

        Array.set this.Stacks (move.From - 1) { Containers = from }
        Array.set this.Stacks (move.To - 1) { Containers = too })

    member this.TopCrates =
      this.Stacks
      |> Array.fold
           (fun acc curr ->
             acc
             + (((pop curr.Containers) |> peek).value |> string))
           ""

  let GetContainersForLine totalStacks (line: string) =
    [| 0 .. (totalStacks - 1) |]
    |> Array.map (fun f -> line[(4 * f) + 1])
    |> Array.map (fun f ->
      match f with
      | x when x <> ' ' -> Some({ value = x })
      | ' ' -> None)

  let GetNumberOfStacks (lines: string []) =
    // Number of columns can be calculated solving (strLength) = (n*3) + (n-1) -> n = (strLength+1) / 4
    let numberOfColumns =
      ((String.length (lines[0])) + 1) / 4

    let stacks =
      lines
      |> Array.takeWhile (fun f -> not (Regex(@"\d").IsMatch(f)))
      |> Array.rev
      |> Array.map (GetContainersForLine numberOfColumns)
      |> Array.transpose
      |> Array.map (fun f ->
        f
        |> (Array.fold
              (fun acc curr ->
                if curr.IsSome then
                  { acc with Containers = push curr.Value acc.Containers }
                else
                  acc)
              { Containers = Empty }))

    { Stacks = stacks }

  let GetMoves (lines: string []) =
    lines
    |> Array.skipWhile (fun f -> not (Regex(@"move").IsMatch(f)))
    |> Array.map (fun f ->
      { Amount = f.Split [| ' ' |][1] |> int
        From = f.Split [| ' ' |][3] |> int
        To = f.Split [| ' ' |][5] |> int })

  [<EntryPoint>]
  let main argv =
    printfn "* Advent of Code 2022 - Supply Stacks"

    match Array.length argv with
    | x when x = 1 ->
      match File.Exists(argv[0]) with
      | true ->
        let stac =
          GetNumberOfStacks(File.ReadAllLines argv[0])

        // let moves = GetMoves (File.ReadAllLines argv[0])
        // moves |> Array.iter (stac.MoveCrate)

        printfn "%A" stac
        // let f = popMany 1 (Node(1, Node(2, Node(3, Empty))))

        printfn "Top crates: %s" stac.TopCrates
        // printfn "%A" f
        
        let stack = Node(1, Node(2, Node(3, Empty)))
        let pick = pop (stack)
        
        printfn "%A" pick

        0
      | _ ->
        // let s = stack
        printfn "Did not find file!"
        1
    | _ ->
      printfn "Usage: dotnet <path-to-input>"
      1
