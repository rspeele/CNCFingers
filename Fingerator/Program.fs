open System
open System.IO
open CNCFingers
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<EntryPoint>]
let main argv =
    let inch = 0.0254<m>
    let thou = inch / 1000.0
    let minute = 60.0<s>
    let parameters = 
        let diameter = inch / 4.0
        let boardWidth = 6.0 * inch
        {   Tool =
                {   Diameter = diameter
                    StepOver = 0.2
                    DepthOfCut = diameter * 0.5
                    FeedRate = 60.0 * inch / minute
                    PlungeRate = 20.0 * inch / minute
                    RampFactor = 2.0
                }
            Board =
                {   Width = boardWidth
                    Thickness = inch
                }
            Finger =
                {   Count = 12
                    SideAllowance = 4.0 * thou
                    EndAllowance = 4.0 * thou
                }
            Start = FingerThenPocket
        }
    let machine =
        {   Unit = Millimeters
        }
    let instructions = FingerCode.instructions parameters
    use output = new StreamWriter(File.Create("fingers.gcode"))
    for instruction in instructions do
        let gcode = instruction.ToGCode(machine)
        output.WriteLine(gcode)
    0 // return an integer exit code
