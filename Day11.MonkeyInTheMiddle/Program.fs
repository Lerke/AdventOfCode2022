open System.IO
open System.Text.RegularExpressions

let ParseExpression =
  Regex(
    @"(?<monkey_def>Monkey (?<monkey_number>\d+):\n\s Starting items: (?<monkey_starting_items>(\d+,?\s?)+)\n\s+Operation: (?<monkey_operation>[^\n]+)\n\s+Test:\s(?<monkey_test>[^\n]+)\n\s+If true: (?<monkey_if_true>[^\n]+)\n\s+If false: (?<monkey_if_false>[^\n]+))",
    RegexOptions.Singleline
  )

type MonkeyOperator =
  | MonkeyAddition
  | MonkeyMultiplication

type Item = { WorryLevel: int64 }

type Monkey =
  { MonkeyNumber: int64
    Items: Item list
    DivisionFactor: int64
    Operation: Item -> Item
    Test: Item -> int64
    RoundInspections: int64 }

let rec LCM (a: int64) (b: int64) =
  match (a % b) with
  | 0L -> a
  | x -> a * (LCM b x) / x

let MonkeyLcm (monkeys: Monkey list) =
  (monkeys |> List.skip 2)
  |> List.fold (fun acc curr -> LCM acc curr.DivisionFactor) (monkeys |> List.take 2 |> (fun f -> LCM f[0].DivisionFactor f[1].DivisionFactor))

let MonkeyOperation paramA paramB operator item =
  let x =
    match paramA with
    | "old" -> item.WorryLevel
    | f -> ((f |> int64))

  let y =
    match paramB with
    | "old" -> item.WorryLevel
    | f -> ((f |> int64))

  let op =
    match operator with
    | MonkeyAddition -> (+)
    | MonkeyMultiplication -> (*)

  let result = op x y
  { item with WorryLevel = result }

let rec ParseMonkeyOperation (line: string) =
  let [| _; _; a; op; b |] = line.Trim().Split [| ' ' |]

  let operation =
    match op with
    | "*" -> MonkeyMultiplication
    | "+" -> MonkeyAddition

  MonkeyOperation a b operation

let MonkeyTest (div: int64) (dst_true: int64) (dst_false: int64) item =
  let result = item.WorryLevel % div

  match result with
  | x when (x = (0L)) -> dst_true
  | _ -> dst_false

let ParseInput (lines: string) =
  let monkey_matches = ParseExpression.Matches(lines.Replace("\r\n", "\n"))

  let monkeys =
    monkey_matches
    |> Seq.map (fun f ->
      { MonkeyNumber = f.Groups["monkey_number"].Value |> int64
        DivisionFactor = ((Array.last (f.Groups[ "monkey_test" ].Value.Split [| ' ' |])) |> int64)
        Items =
          (f.Groups[ "monkey_starting_items" ].Value.Split [| ' ' |]
           |> Array.map (fun f -> { WorryLevel = ((f.Replace(",", "") |> int64)) })
           |> Array.toList)
        Operation = ParseMonkeyOperation(f.Groups["monkey_operation"].Value)
        Test =
          (MonkeyTest
            (((Array.last (f.Groups[ "monkey_test" ].Value.Split [| ' ' |])) |> int64))
            ((Array.last (f.Groups[ "monkey_if_true" ].Value.Split [| ' ' |]) |> int64))
            ((Array.last (f.Groups[ "monkey_if_false" ].Value.Split [| ' ' |]) |> int64)))
        RoundInspections = 0 })
    |> Seq.toList

  monkeys

let rec SimulateRound (monkeys: Monkey list) monkeyIdx withDivision =

  match monkeyIdx with
  | x when (x < monkeys.Length) ->
    let monkeyArr = monkeys |> List.toArray
    let monkey = monkeyArr[monkeyIdx]

    let newItems =
      monkey.Items
      |> List.map (fun f ->
        let inter = (monkey.Operation f)

        let interAfterBored =
          match withDivision with
          | true -> { inter with WorryLevel = (inter.WorryLevel / (3L)) }
          | false -> { inter with WorryLevel = (inter.WorryLevel % (MonkeyLcm monkeys)) }
        let newMonkey = monkey.Test interAfterBored
        (newMonkey, interAfterBored))

    let newMonkeys =
      monkeyArr
      |> Array.mapi (fun i f ->
        match i = monkeyIdx with
        | true ->
          { f with
              Items = []
              RoundInspections = (f.Items.Length) }
        | false ->
          { f with
              Items =
                f.Items
                @ (newItems |> List.filter (fun z -> (fst z) = i) |> List.map (fun z -> (snd z))) })
      |> Array.toList

    SimulateRound newMonkeys (monkeyIdx + 1) withDivision
  | _ -> monkeys

let rec SimulateRounds monkeys rounds withDivision =
  match rounds with
  | 0 -> [ monkeys ]
  | _ ->
    [ monkeys ]
    @ (SimulateRounds (SimulateRound monkeys 0 withDivision) (rounds - 1) withDivision)

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Monkey in the Middle"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let monkeys = (ParseInput(File.ReadAllText argv[0]))
      let after20Round = SimulateRounds monkeys 20 true |> List.skip 1

      let inspections = after20Round |> List.map (List.map (fun z -> z.RoundInspections))

      let inspectionsSum =
        List.fold (fun acc curr -> (List.zip acc curr) |> List.map (fun i -> (fst i) + (snd i))) (List.init inspections[0].Length (fun i -> 0L)) inspections

      let monkeyBusiness =
        ((List.sortDescending inspectionsSum) |> List.take 2)
        |> List.reduce (fun acc curr -> acc * curr)

      printfn $"[* ] Monkey business: %A{monkeyBusiness}"

      let after10000Round = SimulateRounds monkeys 10000 false |> List.skip 1

      let inspectionsAfter10000 =
        after10000Round |> List.map (List.map (fun z -> z.RoundInspections))

      let inspectionsSumAfter10000 =
        List.fold
          (fun acc curr -> (List.zip acc curr) |> List.map (fun i -> (fst i) + (snd i)))
          (List.init inspectionsAfter10000[0].Length (fun i -> 0L))
          inspectionsAfter10000

      let monkeyBusinessAfter10000 =
        ((List.sortDescending inspectionsSumAfter10000) |> List.take 2)
        |> List.reduce (fun acc curr -> acc * curr)

      printfn $"[**] Monkey business: %A{monkeyBusinessAfter10000}"

      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
