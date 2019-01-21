module EasyBox.Program
open System
open System.IO
open FSharp.Data.UnitSystems.SI.UnitSymbols
open CNCFingers

let inch = 0.0254<m>
let ips = inch / 60.0<s>

let config =
    {   Tool =
            {   Diameter = 0.125 * inch
                StepOver = 0.2
                DepthOfCut = 0.0625 * inch
                FeedRate = 50.0 * ips
                PlungeRate = 20.0 * ips
                RampFactor = 2.0
                MaxScaleFeedRate = 2.0
            }
        Machine = { Unit = Millimeters }
        LidThickness = 0.25 * inch
        SideThickness = 0.25 * inch
        BottomThickness = 0.75 * inch
        ExteriorDimensions = 8.0 * inch, 7.0 * inch, 2.0 * inch
    }

let save (machine : Machine) (fileName : string)(instructions : Instruction seq) =
    use output = new StreamWriter(File.Create(fileName))
    for instruction in instructions do
        let gcode = instruction.ToGCode(machine)
        output.WriteLine(gcode)

[<EntryPoint>]
let main argv =
    let generator = BoxCode.BoxGenerator(config)
    generator.FrontSide() |> save config.Machine "front.gcode"
    printfn "Done."
    0 // return an integer exit code
