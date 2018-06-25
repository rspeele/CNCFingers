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
                let instructions = FingerCode.instructions job
                if job.SingleFile then
                    let outputName =
                        Path.Combine
                            ( Path.GetDirectoryName(configFileName)
                            , Path.GetFileNameWithoutExtension(configFileName)
                                + "-" + string job.Start + "-all.gcode"
                            )
                    use output = new StreamWriter(File.Create(outputName))
                    for copy in job.Copies do
                        for instruction in instructions do
                            let instruction = copy instruction
                            let gcode = instruction.ToGCode(job.Machine)
                            output.WriteLine(gcode)
                else
                    for i, copy in job.Copies |> Seq.indexed do
                        let outputName =
                            Path.Combine
                                ( Path.GetDirectoryName(configFileName)
                                , Path.GetFileNameWithoutExtension(configFileName)
                                    + "-" + string job.Start + "-" + string i + ".gcode"
                                )
                        use output = new StreamWriter(File.Create(outputName))
                        for instruction in instructions do
                            let instruction = copy instruction
                            let gcode = instruction.ToGCode(job.Machine)
                            output.WriteLine(gcode)
        0
    with
    | exn ->
        eprintfn "%s" exn.Message
        printfn "Aborted. Press any key to continue."
        ignore <| Console.ReadKey()
        1
