open System
open System.IO

type CommandType =
  | AddX of int
  | Noop

type Command = { Cycles: int; Type: CommandType }
type Register = { X: int }

type Device =
  { Register: Register
    ExecutingCommand: CommandType option
    ExecutedCommands: Command list
    CrtRow: char array }

  static member Initial =
    { Register = { X = 1 }
      ExecutedCommands = []
      ExecutingCommand = None
      CrtRow = Array.init 40 (fun f -> '.') }

  member this.SpriteRow() =
    let arr = Array.init 40 (fun _ -> '.')
    arr[Math.Clamp(this.Register.X - 1, 0, 39)] <- '#'
    arr[Math.Clamp(this.Register.X, 0, 39)] <- '#'
    arr[Math.Clamp(this.Register.X + 1, 0, 39)] <- '#'
    arr

let ParseCommand (cmd: string) =
  match (cmd.Split [| ' ' |]) with
  | [| c |] when c = "noop" -> { Cycles = 1; Type = Noop }
  | [| c; n |] ->
    match c with
    | "addx" -> { Cycles = 2; Type = AddX(n |> int) }

let ParseInput (lines: string array) = lines |> Array.map (ParseCommand)

let rec ExecuteCycle (device: Device) (input: Command list) cyclecount =
  match input with
  | x :: xs ->
    match x with
    | { Cycles = c } when c > 1 ->
      device
      :: ExecuteCycle
           { device with
               ExecutedCommands = (device.ExecutedCommands @ [ x ])
               ExecutingCommand = Some x.Type
               CrtRow =
                 (device.CrtRow
                  |> Array.mapi (fun i f ->
                    if i = (cyclecount % 40) then
                      device.SpriteRow()[i % 40]
                    else
                      f)) }
           ({ x with Cycles = (c - 1) } :: xs)
           (cyclecount + 1)
    | completed ->
      match completed.Type with
      | Noop ->
        device
        :: ExecuteCycle
             { device with
                 ExecutingCommand = Some x.Type
                 ExecutedCommands = (device.ExecutedCommands @ [ x ])
                 CrtRow =
                   (device.CrtRow
                    |> Array.mapi (fun i f ->
                      if i = (cyclecount % 40) then
                        device.SpriteRow()[i % 40]
                      else
                        f)) }
             xs
             (cyclecount + 1)
      | AddX toAdd ->
        device
        :: ExecuteCycle
             { device with
                 Register = { device.Register with X = device.Register.X + toAdd }
                 ExecutingCommand = Some x.Type
                 ExecutedCommands = (device.ExecutedCommands @ [ x ])
                 CrtRow =
                   (device.CrtRow
                    |> Array.mapi (fun i f ->
                      if i = (cyclecount % 40) then
                        device.SpriteRow()[i % 40]
                      else
                        f)) }
             xs
             (cyclecount + 1)
  | [] -> [ device ]

let SampleSignalStrengthAtIndices (indices: int list) (states: Device list) =
  indices |> List.map (fun f -> states[f - 1].Register.X * f) |> List.sum

let GetRegisterImage (states: Device list) =
  [ 40..40 .. (states.Length) ]
  |> List.map (fun f -> new string (states[f].CrtRow))
  |> List.reduce (fun acc curr -> acc + "\n" + curr)

[<EntryPoint>]
let main argv =
  printfn "* Advent of Code 2022 - Cathode-Ray Tube"

  match Array.length argv with
  | x when x = 1 ->
    match File.Exists(argv[0]) with
    | true ->
      let device = Device.Initial

      let commands = ParseInput(File.ReadAllLines argv[0]) |> Array.toList
      let executionStates = ExecuteCycle device commands 0

      let signalStrength = SampleSignalStrengthAtIndices [ 20..40..220 ] executionStates
      printfn $"[* ] First six signal strengths: %i{signalStrength}"

      let image = GetRegisterImage executionStates

      printfn $"%s{image}"
      0
    | _ ->
      printfn "Did not find file!"
      1
  | _ ->
    printfn "Usage: dotnet <path-to-input>"
    1
