open System.IO
open Microsoft.FSharp.Core

type Token =
  | NumberToken of string
  | ListStartToken
  | ListEndToken
  | SeparatorToken

type ParsedToken =
  | Number of int
  | NumberList of ParsedToken list

let ParseLine (line: string) =
  let rec parse (line: char list) (context: Token list) =
    match line with
    | x :: xs ->
      match x with
      | '[' -> parse xs (ListStartToken :: context)
      | ']' -> parse xs (ListEndToken :: context)
      | ',' -> parse xs (SeparatorToken :: context)
      | x ->
        parse
          xs
          ((match context.Head with
            | NumberToken f -> NumberToken(f + (x |> string))
            | _ -> NumberToken(x |> string))
           :: context)
    | [] -> context |> List.filter (fun f -> f <> SeparatorToken) |> List.rev

  parse (line.ToCharArray() |> Array.toList) []

let rec ParseTokens (stack: Token list) =
  let rec parseStack (stack: Token list) =
    let mutable arr = []
    let mutable s = stack

    while ((not s.IsEmpty) && s.Head <> ListEndToken) do
      match s.Head with
      | NumberToken n ->
        arr <- List.append arr [ Number(n |> int) ]
        s <- s.Tail
      | ListStartToken ->
        let (r, stack: Token list) = parseStack (s.Tail)
        arr <- List.append arr [ (NumberList r) ]
        s <- stack.Tail
      | _ -> failwith "unexpected token"

    (arr, s)

  (fst (parseStack stack))[0]

let ComparePair (p1: ParsedToken list) (p2: ParsedToken list) =
  let rec compare (p1: ParsedToken list) (p2: ParsedToken list) =
    match (p1, p2) with
    | (x :: xs, y :: ys) ->
      match (x, y) with
      | (Number nx, Number ny) ->
        printfn $"Compare {nx} vs {ny}"

        match (nx, ny) with
        | _ when nx = ny -> compare xs ys
        | _ when nx < ny ->
          printfn "left side is smaller, inputs are in the right order"
          true
        | _ when nx > ny ->
          printfn "Right side is smaller, inputs are not in the right order"
          false
      | (NumberList nl1, NumberList nl2) -> compare nl1 nl2
    | ([], _) ->
      printfn "Left side ran out of items, so inputs are in the right order"
      true
    | (f, []) when f.Length > 0 ->
      printfn "Right side ran out of items, so inputs are NOT in the right order"
      false

  printfn "Compare %A with %A" p1 p2
  compare p1 p2
  printfn "\n"

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Distress Signal"

  match Array.length argv with
  | x when x = 1 ->
    let parsedInput =
      (File.ReadAllLines argv[0]
       |> Array.filter (fun f -> f.Length > 0)
       |> Array.toList)
      |> List.map (ParseLine)
      |> List.map (ParseTokens)

    let pairs = parsedInput |> List.chunkBySize 2
    // |> List.map (fun f ->
    //   match f with
    //   | [ NumberList n1; NumberList n2 ] -> List.zip n1 n2)

    let firstPair = pairs |> List.take 2

    let solved =
      match (firstPair[0][0], firstPair[0][1]) with
      | (NumberList p, NumberList q) -> ComparePair p q

    let solved2 =
      match (firstPair[1][0], firstPair[1][1]) with
      | (NumberList p, NumberList q) -> ComparePair p q


    0
  | _ ->
    printfn "Did not find file!"
    1
