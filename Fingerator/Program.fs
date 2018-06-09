open System
open System.IO
open CNCFingers

[<EntryPoint>]
let main argv =
    let configFileNames =
        match argv with
        | [||] -> [| "Config.txt" |]
        | _ -> argv

    try
        for configFileName in configFileNames do
            if not (File.Exists(configFileName)) then
                failwithf "File not found: %s" configFileName
            else
                let text = File.ReadAllText(configFileName)
                let job = Fingerator.Parsing.parseJob text configFileName
                printfn "%O" job
                for start in [ FingerThenPocket; PocketThenFinger ] do
                    let instructions = FingerCode.instructions { job with Start = start }
                    let outputName =
                        Path.Combine
                            ( Path.GetDirectoryName(configFileName)
                            , Path.GetFileNameWithoutExtension(configFileName) + "-" + string start + ".gcode"
                            )
                    use output = new StreamWriter(File.Create(outputName))
                    for instruction in instructions do
                        let gcode = instruction.ToGCode(job.Machine)
                        output.WriteLine(gcode)
        0
    with
    | exn ->
        eprintfn "%s" exn.Message
        printfn "Aborted. Press any key to continue."
        ignore <| Console.ReadKey()
        1
