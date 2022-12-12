open System
open System.IO
open System.Text.RegularExpressions

let ParseExpression =
  Regex(
    @"(?<monkey_def>Monkey (?<monkey_number>\d+):\n\s Starting items: (?<monkey_starting_items>(\d+,?\s?)+)\n\s+Operation: (?<monkey_operation>[^\n]+)\n\s+Test:\s(?<monkey_test>[^\n]+)\n\s+If true: (?<monkey_if_true>[^\n]+)\n\s+If false: (?<monkey_if_false>[^\n]+))",
    RegexOptions.Singleline
  )
  
let DebugPrint = false

type MonkeyOperator =
  | MonkeyAddition
  | MonkeyMultiplication

type Item = { WorryLevel: bigint }

type Monkey =
  { MonkeyNumber: int
    Items: Item list
    Operation: Item -> Item
    Test: Item -> bigint
    RoundInspections: bigint }

let MonkeyPrint f =
  match DebugPrint with
  | true -> printfn "%s" f
  | false -> ()

let MonkeyOperation paramA paramB operator item =
  let x =
    match paramA with
    | "old" -> item.WorryLevel
    | f -> (bigint (f |> int))

  let y =
    match paramB with
    | "old" -> item.WorryLevel
    | f -> (bigint (f |> int))

  let op =
    match operator with
    | MonkeyAddition -> (+)
    | MonkeyMultiplication -> (*)

  let result = op x y

  match (paramA, paramB) with
  | (fx, fy) when fx = fy ->
    match operator with
    | (MonkeyAddition) -> MonkeyPrint $"\t\tWorry level is added to itself to %A{result}f"
    | (MonkeyMultiplication) -> MonkeyPrint $"\t\tWorry level is multiplied by itself to %A{result}"
  | (fx, fy) when fx = "old" || fy = "old" ->
    match operator with
    | (MonkeyAddition) -> MonkeyPrint $"\t\tWorry level is added with %A{y} to %A{result}"
    | (MonkeyMultiplication) -> MonkeyPrint $"\t\tWorry level is multiplied by %A{y} to %A{result}"
  | (fx, fy) ->
    match operator with
    | (MonkeyAddition) -> MonkeyPrint $"\t\tWorry level is set to constant %A{(x + y)}"
    | (MonkeyMultiplication) -> MonkeyPrint $"\t\tWorry level is set to constant %A{(x * y)}"

  { item with WorryLevel = result }

let rec ParseMonkeyOperation (line: string) =
  let [| _; _; a; op; b |] = line.Trim().Split [| ' ' |]

  let operation =
    match op with
    | "*" -> MonkeyMultiplication
    | "+" -> MonkeyAddition

  MonkeyOperation a b operation

let MonkeyTest (div: bigint) (dst_true: bigint) (dst_false: bigint) item =
  let result = item.WorryLevel % div

  match result with
  | x when (x = (bigint 0)) ->
    MonkeyPrint $"\t\tCurrent worry level is divisible by %A{div}."
    dst_true
  | _ ->
    MonkeyPrint $"\t\tCurrent worry level is not divisible by %A{div}."
    dst_false

let ParseInput (lines: string) =
  let monkey_matches = ParseExpression.Matches(lines.Replace("\r\n", "\n"))

  let monkeys =
    monkey_matches
    |> Seq.map (fun f ->
      { MonkeyNumber = f.Groups["monkey_number"].Value |> int
        Items =
          (f.Groups[ "monkey_starting_items" ].Value.Split [| ' ' |]
           |> Array.map (fun f -> { WorryLevel = bigint ((f.Replace(",", "") |> int)) })
           |> Array.toList)
        Operation = ParseMonkeyOperation(f.Groups["monkey_operation"].Value)
        Test =
          (MonkeyTest
            (bigint ((Array.last (f.Groups[ "monkey_test" ].Value.Split [| ' ' |])) |> int))
            (bigint (Array.last (f.Groups[ "monkey_if_true" ].Value.Split [| ' ' |]) |> int))
            (bigint (Array.last (f.Groups[ "monkey_if_false" ].Value.Split [| ' ' |]) |> int)))
        RoundInspections = 0 })
    |> Seq.toList

  monkeys

let PrintInspectionsPerRound (monkeys: Monkey list) =
  monkeys
  |> List.iteri (fun i f -> MonkeyPrint $"Monkey %i{i} inspected items %A{f.RoundInspections} times")

let rec SimulateRound (monkeys: Monkey list) monkeyIdx withDivision =

  match monkeyIdx with
  | x when (x < monkeys.Length) ->
    let monkeyArr = monkeys |> List.toArray
    let monkey = monkeyArr[monkeyIdx]
    MonkeyPrint $"Monkey %i{monkeyIdx}:"

    let newItems =
      monkey.Items
      |> List.map (fun f ->
        MonkeyPrint $"\tMonkey inspects an item with a worry level of %A{f.WorryLevel}"
        let inter = (monkey.Operation f)

        let interAfterBored =
          match withDivision with
          | true -> { inter with WorryLevel = (inter.WorryLevel / (bigint 3)) }
          | fasle -> inter

        MonkeyPrint $"\t\tMonkey gets bored with item. Worry level is divided by 3 to %A{interAfterBored.WorryLevel}"
        let newMonkey = monkey.Test interAfterBored
        MonkeyPrint $"\t\tItem with worry level %A{interAfterBored.WorryLevel} is thrown to monkey %A{newMonkey}"
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
        List.fold (fun acc curr -> (List.zip acc curr) |> List.map (fun i -> (fst i) + (snd i))) (List.init inspections[0].Length (fun i -> bigint 0)) inspections

      let monkeyBusiness =
        ((List.sortDescending inspectionsSum) |> List.take 2)
        |> List.reduce (fun acc curr -> acc * curr)

      printfn $"[* ] Monkey business: %A{monkeyBusiness}"

      let after10000Round = SimulateRounds monkeys 10000 false |> List.skip 1

      let inspectionsAfter10000 =
        after10000Round |> List.map (List.map (fun z -> z.RoundInspections))

      let inspectionsSumAfter10000 =
        List.fold (fun acc curr -> (List.zip acc curr) |> List.map (fun i -> (fst i) + (snd i))) (List.init inspectionsAfter10000[0].Length (fun i -> bigint 0)) inspectionsAfter10000

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
