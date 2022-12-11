open System.IO

type CommandType =
  | AddX of int
  | Noop

type Command = { Cycles: int; Type: CommandType }
type Register = { X: int }

type Device =
  { Register: Register
    ExecutingCommand: CommandType option
    ExecutedCommands: Command list }

  static member Initial =
    { Register = { X = 1 }
      ExecutedCommands = []
      ExecutingCommand = None }

let ParseCommand (cmd: string) =
  match (cmd.Split [| ' ' |]) with
  | [| c |] when c = "noop" -> { Cycles = 1; Type = Noop }
  | [| c; n |] ->
    match c with
    | "addx" -> { Cycles = 2; Type = AddX(n |> int) }

let ParseInput (lines: string array) = lines |> Array.map (ParseCommand)

let rec ExecuteCycle (device: Device) (input: Command list) =
  match input with
  | x :: xs ->
    match x with
    | { Cycles = c } when c > 1 ->
      device
      :: ExecuteCycle
           { device with
               ExecutedCommands = (device.ExecutedCommands @ [ x ])
               ExecutingCommand = Some x.Type }
           ({ x with Cycles = (c - 1) } :: xs)
    | completed ->
      match completed.Type with
      | Noop ->
        device
        :: ExecuteCycle
             { device with
                 ExecutingCommand = Some x.Type
                 ExecutedCommands = (device.ExecutedCommands @ [ x ]) }
             xs
      | AddX toAdd ->
        device
        :: ExecuteCycle
             { device with
                 Register = { device.Register with X = device.Register.X + toAdd }
                 ExecutingCommand = Some x.Type
                 ExecutedCommands = (device.ExecutedCommands @ [ x ]) }
             xs
  | [] -> [ device ]

let SampleSignalStrengthAtIndices (indices: int list) (states: Device list) =
  indices |> List.map (fun f -> states[f - 1].Register.X * f) |> List.sum

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Cathode-Ray Tube"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let device = Device.Initial

      let commands = ParseInput(File.ReadAllLines argv[0]) |> Array.toList
      let executionStates = ExecuteCycle device commands

      let signalStrength = SampleSignalStrengthAtIndices [ 20..40..220 ] executionStates
      printfn $"[* ] First six signal strengths: %i{signalStrength}"

      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
