open Microsoft.FSharp.Core

type Token =
  | NumberToken of string
  | ListStartToken
  | ListEndToken
  | SeparatorToken

type ParsedToken =
  | Number of int
  | NumberList of ParsedToken list

let rec ParseLine (line: char list) (context: Token list) =
  match line with
  | x :: xs ->
    match x with
    | '[' -> ParseLine xs (ListStartToken :: context)
    | ']' -> ParseLine xs (ListEndToken :: context)
    | ',' -> ParseLine xs (SeparatorToken :: context)
    | x ->
      ParseLine
        xs
        ((match context.Head with
          | NumberToken f -> NumberToken(f + (x |> string))
          | _ -> NumberToken(x |> string))
         :: context)
  | [] -> context |> List.filter (fun f -> f <> SeparatorToken) |> List.rev

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
        arr <- List.append [ (NumberList r) ] arr
        s <- stack.Tail

    (arr, s)

  (fst (parseStack stack))[0]

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Distress Signal"

  match Array.length argv with
  | x when x = 1 ->
    // let line = "[1,1,3,1,1]"
    // let parsedLine = ParseLine (line.ToCharArray() |> Array.toList) []
    let parsedTwo = ParseLine ("[[4,4],4,4]".ToCharArray() |> Array.toList) []

    // let q = ParseTokens parsedLine
    let v = ParseTokens parsedTwo
    0
  | _ ->
    printfn "Did not find file!"
    1
